
#include <asm.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "threads.h"

static thread_t * current;

static thread_t * cleanup_stack = NULL;

static unsigned long next_id = 0;

static int num_threads = 0;

void thread_init() {
	current = (thread_t *)malloc(sizeof(struct thread));
	
	current->id = next_id++;
	current->status = RUNNABLE;
	current->next = current;
	current->prev = current;
	current->slot = NULL;
	
	num_threads = 1;
}

void schedule_for_deletion(thread_t *thread)
{
	if (num_threads <= 1) {
		dprintf("System halted: nothing to run!\n");
		asm volatile("cli");
		asm volatile("hlt");
	}
	
	thread->prev->next = thread->next;
	thread->next->prev = thread->prev;
	
	if (cleanup_stack == NULL) {
		thread->next = NULL;
		thread->prev = NULL;
		cleanup_stack = thread;
	} else {
		thread->next = cleanup_stack;
		thread->prev = NULL;
		cleanup_stack = thread;
	}
}

unsigned long *thread_schedule(unsigned long *esp) {
	thread_t *next;
	
	dprintf("thread_schedule:\r\n");

try_again:
	for (next = current->next; next != current; next = next->next) {
		if (next->status == RUNNABLE) {
			break;
		}
	}
	
	if (current == next) {
		if (current->status == BLOCKED) {
			// nothing to do!
			dprintf("%d blocked\r\n", num_threads);
			asm("hlt");
			goto try_again;
		}
		if (current->status == KILLED || current->status == EXITED) {
			// killed/exited last thread
			//dprintf("no more threads to run! system halted :)\r\n");
			//asm("cli");
			dprintf("none runnable\r\n");
			asm("hlt");
			goto try_again;
		}
		dprintf("t %d:%x resumes\r\n", current->id, current->stack);
		return esp;
	}
	
	current->esp = esp;
	
	if (current->status == KILLED) {
		// prepare thread to be deleted
		schedule_for_deletion(current);
	}
	
	current = next;
	
	dprintf("t %d:%x starts (of %d)\r\n", current->id, current->stack, num_threads);
	
	return current->esp;
}

void thread_yield() {
	thread_t * thread;
	// cleanup threads that have terminated
	while (cleanup_stack != NULL) {
		thread = cleanup_stack;
		cleanup_stack = cleanup_stack->next;
		dprintf("t %d%x deleted\r\n", thread->id, thread->stack);
		free(thread->stack);
		free(thread);
	}
	asm volatile("int $0x30");
}

extern void thread_start();

void thread_exit(void *retval) {
	num_threads--;
	// update status so it will get removed later...
	current->status = EXITED;
	dprintf("t %d:%x exited\r\n", current->id, current->stack);
	asm volatile("movl %0, %%eax" : : "c" (retval));	
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
 
	*--thread->esp = 0x202; /* EFLAGS */
	*--thread->esp = 0x08; /* CS */
	*--thread->esp = (unsigned long)thread_start; /* EIP */
	*--thread->esp = (unsigned long)closure; /* EAX */
	*--thread->esp = (unsigned long)arg; /* ECX */
	*--thread->esp = 0xED; /* EDX */
	*--thread->esp = 0xEB; /* EBX */
	*--thread->esp = 0xCAFEBABE; /* ESP; dummy value */
	ebp = thread->esp;
	*--thread->esp = 0x0; /* EBP; 0 for stacktrace() */
	esp = thread->esp;
	*--thread->esp = 0xED1; /* EDI */
	*--thread->esp = 0xE51; /* ESI */
	/* we don't ever change segment registers, so removed :) */
	
	*esp = (unsigned long)thread->esp;
	*ebp = (unsigned long)thread->esp;

	++num_threads;
	
	thread->next = current->next;
	thread->prev = current;
	current->next->prev = thread;
	current->next = thread;
	
	dprintf("t %d:%x created\r\n", thread->id, thread->stack);
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

/* thread synchronisation primitives */

void mutex_init(mutex_t *mutex) {
	mutex->queue = NULL;
	mutex->head = NULL;
	mutex->owner = 0;
	mutex->id = next_id++;
	dprintf("m %d:%d init\r\n", mutex->id, current->id);
}

void mutex_destroy(mutex_t *mutex) {
	thread_queue_t *queue;
	while (mutex->queue != NULL) {
		/* resume threads that were blocked on this mutex */
		mutex->queue->thread->status = RUNNABLE;
		queue = mutex->queue;
		mutex->queue = mutex->queue->next;
		free(queue);
	}
	dprintf("m %d:%d destroyed\r\n", mutex->id, current->id);
}

void mutex_lock(mutex_t *mutex) {
	thread_queue_t *queue;
	queue = (thread_queue_t *)malloc(sizeof(struct thread_queue));
	asm volatile("cli; nop");
	dprintf("m %d:%d locking\r\n", mutex->id, current->id);
	if (mutex->queue == NULL) {
		mutex->queue = queue;
		mutex->head = queue;
	} else {
		queue->next = mutex->queue;
		mutex->queue = queue;
	}
	queue->thread = current;
	while (mutex->owner != NULL && mutex->owner != current) {
		// mutex locked by someone else
		dprintf("m %d:%d locked by %d\r\n", mutex->id, current->id, mutex->owner->id);
		asm volatile("sti; nop");
		current->status = BLOCKED;
		thread_yield();
		dprintf("m %d:%d retrying\r\n", mutex->id, current->id);
		asm volatile("cli; nop");
	}
	mutex->owner = current;
	dprintf("m %d:%d locked\r\n", mutex->id, current->id);
	asm volatile("sti; nop");
}

void mutex_unlock(mutex_t *mutex) {
	thread_queue_t *queue, *prev;
	asm volatile("cli; nop");
	prev = mutex->head;
	for (queue = prev; queue != NULL; queue = prev->next)
	{
		if (queue == mutex->head) {
			/* remove from queue */
			if (queue == prev) {
				mutex->queue = mutex->queue->next;
			} else {
				prev->next = queue->next;
			}
			break;
		} else {
			prev = prev->next;
		}
	}
	dprintf("m %d:%d unlocked\r\n", mutex->id, current->id);
	dprintf("mx %x\r\n", mutex->head);
	dprintf("mx %x\r\n", mutex->head->thread);
	mutex->head->thread->status = RUNNABLE;
	queue = mutex->head;
	mutex->head = mutex->head->next;
	free(queue);
	mutex->owner = NULL;
	asm volatile("sti; nop");
}

int mutex_trylock(mutex_t *mutex) {
	int retcode;
	asm volatile("cli; nop");
	retcode = mutex->owner == NULL ? 0 : -1;
	dprintf("m %d:%d try lock = %d\r\n", mutex->id, current->id, retcode);
	asm volatile("sti; nop");
	return retcode;
}

void cond_init(cond_t *cond) {
	cond->queue = NULL;
	cond->head = NULL;
	cond->waiting = 0;
	cond->id = next_id++;
	dprintf("c %d:%d init\r\n", cond->id, current->id);
}

void cond_destroy(cond_t *cond) {
	thread_queue_t *queue;
	while (cond->queue != NULL) {
		/* resume threads that were blocked on this mutex */
		cond->queue->thread->status = RUNNABLE;
		queue = cond->queue;
		cond->queue = cond->queue->next;
		free(queue);
	}
	dprintf("c %d:%d destroyed\r\n", cond->id, current->id);
}

void cond_wait(cond_t *cond, mutex_t *mutex) {
	thread_queue_t *queue, *prev;
	/* mutex is already locked */
	queue = (thread_queue_t *)malloc(sizeof(struct thread_queue));
	queue->thread = current;
	
	/* another thread waiting to be signalled */
	if (cond->head == NULL) {
		queue->next = NULL;
		cond->head = queue;
		cond->queue = queue;
	} else {
		queue->next = cond->queue;
		cond->queue = queue;
	}
	
	cond->waiting++;
	
	/* put this thread to sleep */
	cond->head->thread->status = BLOCKED;
	while (cond->waiting > 0) {
		dprintf("c %d:%d waiting\r\n", cond->id, current->id);
		/* unlock the mutex */
		mutex_unlock(mutex);
		thread_yield();
		mutex_lock(mutex);
	}
	
	/* we're runnable, remove us from the queue */
	prev = cond->head;
	for (queue = prev; queue != NULL; queue = prev->next)
	{
		if (queue == cond->head) {
			/* remove from queue */
			if (queue == prev) {
				cond->queue = cond->queue->next;
			} else {
				prev->next = queue->next;
			}
			break;
		} else {
			prev = prev->next;
		}
	}
	queue = cond->head;
	cond->head = cond->head->next;
	free(queue);
	dprintf("c %d:%d resumed\r\n", cond->id, current->id);
}

/* so these don't need the mutex as we don't modify the queue? */

void cond_signal(cond_t *cond) {
	dprintf("c %d:%d signalled\r\n", cond->id, current->id);
	if (cond->waiting > 0) {
		--cond->waiting;
		cond->head->thread->status = RUNNABLE;
		dprintf("c %d:%d released thread %d\r\n", cond->id, current->id, cond->head->thread->id);
	}
}

void cond_broadcast(cond_t *cond) {
	dprintf("c %d:%d broadcasted\r\n", cond->id, current->id);
	while (cond->waiting > 0) {
		cond_signal(cond);
	}
}
