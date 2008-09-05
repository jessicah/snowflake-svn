
#include <asm.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <threads.h>

/*** thread ***/

static thread_t * current;

typedef struct thread_queue {
	thread_t * thread;
	struct thread_queue * next;
	struct thread_queue * prev;
} thread_queue_t;

thread_queue_t * runnable;

static int next_id = 0;

void thread_init() {
	current = (thread_t *)malloc(sizeof(struct thread));
		
	runnable = (thread_queue_t *)malloc(sizeof(struct thread));
		
	current->id = next_id++;
	current->status = RUNNABLE;
	current->slot = NULL;
	
	runnable->thread = current;
	runnable->next = runnable;
	runnable->prev = runnable;
}

unsigned long *thread_schedule(unsigned long *esp) {
	thread_queue_t *q;
	thread_t *next;
try_again:
	q = runnable->next; /* don't scan current thread */
	for ( ; q->thread != runnable->thread; q = q->next ) {
		if (q->thread->status == RUNNABLE) {
			runnable = q;
			/* switch to this thread */
			current->esp = esp;
			current = runnable->thread;
			//dprintf("switching to %d\n", current->id);
			return current->esp;
		}
		else if (q->thread->status == KILLED) {
			/* free a killed thread */
			continue;
		}
	}
	/* if we're here, we didn't find a new runnable thread */
	/* so check if the current thread can still run... */
	if (current->status != RUNNABLE) {
		/* it's not runnable, so nothing to run... */
		//dprintf("idle\n");
		asm volatile("hlt");
		/* then try again :) */
		goto try_again;
	}
	//dprintf("resuming current thread %d\n", current->id);
	/* only the current thread is runnable, so keep on going */
	return esp;
}

void thread_yield() {
	asm volatile("int $0x30");
}

extern void thread_start();

void thread_exit(void *retval) {
	/* set it as KILLED .. fairly simple :) */
	//dprintf("killing %d\n", current->id);
	current->status = KILLED;
	asm volatile("movl %0, %%eax" : : "c" (retval));
	//dprintf("exit\n");
	asm volatile("int $0x30");
}

#define STACK_SIZE 16384

void thread_create(thread_t *thread, void *(*closure)(void *), void *arg) {
	unsigned long *stack;
	unsigned long *esp, *ebp;
	
	stack = (unsigned long *)malloc(STACK_SIZE * sizeof(unsigned long));
	thread = (thread_t *)malloc(sizeof(struct thread));
		
	memset(stack, 0, 0x10000);
	
	thread->id = next_id++;
	thread->status = RUNNABLE;
	thread->slot = NULL;
	thread->stack = stack;
	thread->esp = thread->stack + STACK_SIZE;
	
	//dprintf("new thread (%d)\n", thread->id);
	
	*--thread->esp = 0x202; /* eflags */
	*--thread->esp = 0x08; /* code segment */
	*--thread->esp = (unsigned long)thread_start; /* eip */
	*--thread->esp = (unsigned long)closure; /* eax */
	*--thread->esp = (unsigned long)arg; /* ecx */
	*--thread->esp = 0xedededed; /* edx */
	*--thread->esp = 0xebebebeb; /* ebx */
	*--thread->esp = 0xcafebabe; /* dummy ebp value */
	ebp = thread->esp;
	*--thread->esp = 0xcafebabe; /* dummy esp value */
	esp = thread->esp;
	*--thread->esp = 0x00ed1ed1; /* edi */
	*--thread->esp = 0x00e51e51; /* esi */
	/* we don't manipulate segment registers, so left out */
	
	*esp = (unsigned long)thread->esp;
	*ebp = (unsigned long)thread->esp;
	
	/* link into runnable threads */
	thread_queue_t * q;
	q = (thread_queue_t *)malloc(sizeof(struct thread_queue));
		
	q->thread = thread;
	q->next = runnable->next;
	q->prev = runnable;
	runnable->next->prev = q;
	runnable->next = q;
}

thread_t thread_self() {
	return *current;
}

void thread_setspecific(void *data) {
	current->slot = data;
}

void *thread_getspecific() {
	return current->slot;
}

/*** mutex ***/

void mutex_init(mutex_t *mutex) {
	mutex->locked = 0;
	mutex->owner = NULL;
	thread_queue_t *q = (thread_queue_t *)malloc(sizeof(struct thread_queue));
		
	mutex->waiting = q;
	mutex->waiting->thread = NULL;
	mutex->tail = mutex->waiting;
}

void mutex_destroy(mutex_t *mutex) {
	/* free thread queue */
	thread_queue_t *q, *p;
	q = mutex->waiting; p = q->prev;
	for (p = q->prev; p != NULL; p = p->prev) {
		free(q);
		q = p;
	}
	free(q);
}

#if 1
void mutex_lock(mutex_t *mutex) {
	/* we only work on UP systems, so just cli/sti */
	//asm volatile("pushf; cli; nop");
	asm volatile("cli");
	if (mutex->locked == 1 && mutex->owner != current) {
		current->status = SUSPENDED;
		/* add to thread queue */
		if (mutex->waiting->thread == NULL) {
			mutex->waiting->thread = current;
		} else {
			/* need to allocate another node */
			thread_queue_t *q = (thread_queue_t *)malloc(sizeof(struct thread_queue));
				
			q->thread = current;
			mutex->tail->prev = mutex->tail;
			mutex->tail = q;
		}
		/* restore interrupts, and reschedule */
		//dprintf("lock: suspending %d\n", current->id);
		//asm volatile("popf; int $0x30");
		asm volatile("sti");
		thread_yield();
		/* when we're resumed, we've acquired the lock */
	} else {
		/* no-one held this lock */
		//dprintf("lock: acquired by %d\n", current->id);
		mutex->locked = 1;
		mutex->owner = current;
		//asm volatile("popf");
		asm volatile("sti");
	}
}

void mutex_unlock(mutex_t *mutex) {
	//asm volatile("pushf; cli; nop");
	asm volatile("cli");
	/* resume first thread waiting on the mutex */
	if (mutex->waiting->thread != NULL) {
		//dprintf("lock: released by %d and acquired by %d\n", current->id, mutex->waiting->thread->id);
		mutex->waiting->thread->status = RUNNABLE;
		if (mutex->waiting->prev != NULL) {
			thread_queue_t *q = mutex->waiting->prev;
			free(mutex->waiting);
			mutex->waiting = q;
		}
	} else {
		//dprintf("lock: released by %d\n", current->id);
		mutex->locked = 0;
	}
	//asm volatile("popf");
	asm volatile("sti");
}
#else
void mutex_lock(mutex_t *mutex) {
try_again:
	asm volatile("cli");
	if (mutex->locked == 1) {
		asm volatile("sti");
		dprintf("%d suspended on a lock\n", current->id);
		thread_yield();
		goto try_again;
	}
	mutex->locked = 1;
	dprintf("%d acquired lock\n", current->id);
	asm volatile("sti");
}

void mutex_unlock(mutex_t *mutex) {
	asm volatile("cli");
	dprintf("%d unlocking mutex\n", current->id);
	mutex->locked = 0;
	asm volatile("sti");
}
#endif

int mutex_trylock(mutex_t *mutex) {
	int retcode;
	//asm volatile("pushf; cli; nop");
	asm volatile("cli");
	if (mutex->locked == 0) {
		mutex->owner = current;
		mutex->locked = 1;
		//dprintf("trylock: acquired by %d\n", current->id);
		//asm volatile("popf");
		asm volatile("sti");
		return 1;
	} else {
		//dprintf("trylock: already locked by %d\n", mutex->owner->id);
		//asm volatile("popf");
		asm volatile("sti");
		return 0;
	}
}

/*** condition variable ***/

/* unlike mutex above, these busy wait =/ */

void cond_init(cond_t *cond) {
	cond->waiting = 0;
}

void cond_destroy(cond_t *cond) {
	/* no-op */
}

void cond_wait(cond_t *cond, mutex_t *mutex) {
	/* mutex is already locked */
	
	//asm volatile("pushf; cli; nop");
	//dprintf("wait: suspending %d\n", current->id);
	cond->waiting++;
	
	while (cond->waiting > 0) {
		/* unlock the mutex */
		mutex_unlock(mutex);
		/* yield... */
		//dprintf("wait: yielding %d\n", current->id);
		//asm volatile("popf; int $0x30; pushf; cli; nop");
		thread_yield();
		/* relock the mutex */
		mutex_lock(mutex);
		//asm volatile("pushf; cli; nop");
	}
	//dprintf("wait: resuming %d\n", current->id);
	//asm volatile("popf");
}

void cond_signal(cond_t *cond) {
	//asm volatile("pushf; cli; nop");
	asm volatile("cli");
	if (cond->waiting > 0) {
		//dprintf("wait: signalled by %d\n", current->id);
		cond->waiting--;
	}
	//asm volatile("popf");
	asm volatile("sti");
}

void cond_broadcast(cond_t *cond) {
	//asm volatile("pushf; cli; nop");
	asm volatile("cli");
	//dprintf("wait: broadcast by %d\n", current->id);
	cond->waiting = 0;
	//asm volatile("popf");
	asm volatile("sti");
}
