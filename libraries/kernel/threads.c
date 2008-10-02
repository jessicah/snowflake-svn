
#include <asm.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "threads.h"

extern void _thread_switch_stacks(unsigned long *new_esp, unsigned long **old_esp);

static void *do_idle(void *);
static void *do_reaper(void *);

static unsigned long next_id = 0;

static LIST_INITIALIZE(all_threads);
static LIST_INITIALIZE(run_queue);
static LIST_INITIALIZE(zombie_list);
// static int num_threads = 0;
static real_thread_t *current;

/* Thread for cleanup post stack switch in schedule */
static real_thread_t *zombie_thread = NULL;

static real_thread_t kernel_thread;
static thread_t idle_thread;
/* Reaper: Slayer of dead threads */
static thread_t reaper_thread;

void thread_init() {
	/* Kernel thread is special, it already has a stack and is currently running */
	kernel_thread.id = next_id++;
	kernel_thread.status = RUNNABLE;
	link_initialize(&kernel_thread.run_link);
	link_initialize(&kernel_thread.global_link);
	list_insert_prev(&kernel_thread.global_link, &all_threads);
	kernel_thread.slot = NULL;
	current = &kernel_thread;
	
	thread_create(&idle_thread, do_idle, NULL);
	thread_create(&reaper_thread, do_reaper, NULL);
}

static void schedule(void)
{
	/* Save the current state of IF and disable interrupts */
	long intr_state = interrupts_disable();
	real_thread_t *previous = current;
	
	/* Possibly put the thread back on the run queue
	 * The idle thread is special, it never goes on the run queue */
	if(current != idle_thread) {
		switch(current->status) {
		case RUNNABLE:
			/* Place on the end of the run queue */
			list_append(&current->run_link, &run_queue);
			break;
		case BLOCKED:
			/* Nothing */
			break;
		case KILLED:
		case EXITED:
			/* The thread is dead but cannot be freed here because 
			 * we're currently running on it's stack
			 * Prepare it for deletion and wake Reaper */
			list_append(&current->run_link, &zombie_list);
			if(reaper_thread->status == BLOCKED) {
				reaper_thread->status = RUNNABLE;
				list_append(&reaper_thread->run_link, &run_queue);
			}
			break;
		default:
			dprintf("schedule: Aiee! invalid thread state %u in %u/%x\r\n", current->status, current->id, (long)current);
			assert(0);
		}
	}
	
	/* Pick a new thread to run */
	if(list_empty(&run_queue)) {
		/* Nothing to run, schedule the idle thread */
		current = idle_thread;
	} else {
		/* Pull it from the fron of the run queue */
		current = list_get_instance(run_queue.next, real_thread_t, run_link);
		list_remove(&current->run_link);
	}
	
	if(previous == current) {
		/* Nothing to do, early return now to avoid the stack switch code */
		return;
	}
	
	/* MAGIC! */
	_thread_switch_stacks(current->esp, &previous->esp);
	/* Now we're running on current's stack, so local variable have changed
	 * intr_state now holds the IF state for this thread, not the previous thread */
	interrupts_restore(intr_state);
}

void thread_yield(void) {
	schedule();
}

void thread_exit(void *retval) {
	dprintf("t %d:%x exited\r\n", current->id, current->stack);
	/* Signal schedule that this thread has exited */
	current->status = EXITED;
	schedule();
	/* Can't reach here */
	assert(0);
}

#define STACK_SIZE 16384

static void thread_entry_trampoline(void *(*closure)(void *), void *arg)
{
	interrupts_enable();
	thread_exit(closure(arg));
}

void thread_create(thread_t *thread, void *(*closure)(void *), void *arg) {
	*thread = malloc(sizeof(real_thread_t));
	(*thread)->id = next_id++;
	(*thread)->status = RUNNABLE;
	(*thread)->slot = NULL;
	(*thread)->stack = (unsigned long *)malloc(STACK_SIZE * sizeof(unsigned long));
	(*thread)->esp = (*thread)->stack + STACK_SIZE;
	
	memset((*thread)->stack, 0, STACK_SIZE * sizeof(unsigned long));
	
	link_initialize(&(*thread)->run_link);
	link_initialize(&(*thread)->global_link);
	
	/* Set up the stack for _thread_switch_stacks */
	*--(*thread)->esp = (unsigned long)arg;                      /* Argument 2 for the trampoline */
	*--(*thread)->esp = (unsigned long)closure;                  /* Argument 1 for the trampoline */
	*--(*thread)->esp = 0;                                       /* Return address */
	*--(*thread)->esp = (unsigned long)thread_entry_trampoline;  /* EIP/_stack_switch return address */
	*--(*thread)->esp = 0;                                       /* EBP */
	*--(*thread)->esp = 0;                                       /* EBX */
	*--(*thread)->esp = 0;                                       /* ESI */
	*--(*thread)->esp = 0;                                       /* EDI */
	
	long istate = interrupts_disable();
	list_append(&(*thread)->global_link, &all_threads);
	list_append(&(*thread)->run_link, &run_queue);
	interrupts_restore(istate);
	
	dprintf("t %d:%x:%x created\r\n", (*thread)->id, (*thread)->stack, thread);
}

thread_t thread_self() {
	return current;
}

void thread_setspecific(void *data) {
	current->slot = data;
}

void *thread_getspecific() {
	return current->slot;
}

/* thread synchronisation primitives */

void mutex_init(mutex_t *mutex) {
	mutex->owner = NULL;
	mutex->id = next_id++;
	dprintf("m %d:%d %x init\r\n", mutex->id, current->id, (long)mutex);
}

void mutex_destroy(mutex_t *mutex) {
	dprintf("m %d:%d %x destroyed\r\n", mutex->id, current->id, (long)mutex);
}

void mutex_lock(mutex_t *mutex) {
	long istate = interrupts_disable();
	dprintf("m %d:%d %x locking\r\n", mutex->id, current->id, (long)mutex);
	
	/* Check for recursive locking */
	assert(mutex->owner != current);
	
	while(mutex->owner) {
		/* Locked by something else */
		dprintf("m %d:%d %x locked by %d\r\n", mutex->id, current->id, mutex->owner->id, (long)mutex);
		/* Block here ### */
	}
	
	mutex->owner = current;
	dprintf("m %d:%d %x locked\r\n", mutex->id, current->id, (long)mutex);
	interrupts_restore(istate);
}

void mutex_unlock(mutex_t *mutex) {
	long istate = interrupts_disable();
	
	/* Ensure the mutex is locked by us */
	assert(mutex->owner == current);
	
	/* Wake the first thread */
	/* ### */
	
	dprintf("m %d:%d %x unlocked\r\n", mutex->id, current->id, (long)mutex);
	mutex->owner = NULL;
	interrupts_restore(istate);
}

int mutex_trylock(mutex_t *mutex) {
	long istate = interrupts_disable();
	int retcode = -1;
	if(mutex->owner == NULL) {
		mutex->owner = current;
		retcode = 0;
	}
	dprintf("m %d:%d %x try lock = %d\r\n", mutex->id, current->id, (long)mutex, retcode);
	interrupts_restore(istate);
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
	prev = cond->queue;
	for (queue = prev; queue != NULL; queue = prev->next)
	{
		if (queue == cond->head) {
			/* remove from queue */
			if (queue == cond->head) {
				cond->queue = cond->queue->next;
			} else {
				prev->next = queue->next;
			}
			break;
		}
		prev = queue;
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
