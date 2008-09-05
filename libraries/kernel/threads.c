
#include <asm.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "threads.h"

static thread_t * current;

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

unsigned long *thread_schedule(unsigned long *esp) {
	thread_t *next;

try_again:
	for (next = current->next; next != current; next = next->next) {
		if (next->status == RUNNABLE) {
			break;
		}
	}
	
//	dprintf("schedule: %d threads running\n", num_threads);
//	dprintf("from %d to %d\n", current->id, next->id);

	if (current == next) {
		if (current->status == BLOCKED) {
			asm("hlt");
			goto try_again;
		}
		return esp;
	}
	current->esp = esp;
	/* do some cleanup if required */
	if (current->status == KILLED) {
		free(current);
	}
	current = next;
	return current->esp;
}

void thread_yield() {
	asm volatile("int $0x30");
}

extern void thread_start();

void thread_exit(void *retval) {
	num_threads--;
//	dprintf("exit: %d threads running\n", num_threads);
	/* exit the thread by unlinking it, then rescheduling */
	current->prev->next = current->next;
	current->next->prev = current->prev;
	asm volatile("movl %0, %%eax" : : "c" (retval));
	free(current->stack);
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
	*--thread->esp = 0xCAFEBABE; /* EBP; dummy value */
	esp = thread->esp;
	*--thread->esp = 0xED1; /* EDI */
	*--thread->esp = 0xE51; /* ESI */
	/* we don't ever change segment registers, so removed :) */
	
	*esp = (unsigned long)thread->esp;
	*ebp = (unsigned long)thread->esp;

	++num_threads;
//	dprintf("created: %d threads running\n", num_threads);
	
	thread->next = current->next;
	thread->prev = current;
	current->next->prev = thread;
	current->next = thread;
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

/* could just do a very naive busy waiting scheme :P */
/* i.e.: check flag, yield & try again, or return */

void mutex_init(mutex_t *mutex) {
	mutex->locked = 0;
	mutex->owner = 0;
}

void mutex_destroy(mutex_t *mutex) {
	/* don't really need to do anything here */
}

void mutex_lock(mutex_t *mutex) {
try_again:
	asm volatile("cli; nop");
	while (mutex->locked == 1 && mutex->owner != current) {
		asm volatile("sti; nop");
		/* busy wait because I suck... */
		thread_yield();
		goto try_again;
	}
	mutex->locked = 1;
	mutex->owner = current;
	asm volatile("sti; nop");
}

void mutex_unlock(mutex_t *mutex) {
	asm volatile("cli; nop");
	mutex->locked = 0;
	mutex->owner = NULL;
	asm volatile("sti; nop");
}

int mutex_trylock(mutex_t *mutex) {
	int retcode;
	asm volatile("cli; nop");
	retcode = mutex->locked == 0 ? 1 : -1;
	asm volatile("sti; nop");
	return retcode;
}

/* so how do we do condition variables again? */

void cond_init(cond_t *cond) {
	cond->waiting = NULL;
}

void cond_destroy(cond_t *cond) {
	/* free up waiting list */
	cond->waiting = NULL;
}

void cond_wait(cond_t *cond, mutex_t *mutex) {
	/* mutex is already locked */
	
	cond->waiting++;
	
	/* put this thread to sleep */
	while (cond->waiting > 0) {
		/* unlock the mutex */
		mutex_unlock(mutex);
		//dprintf("waiting\n");
		thread_yield();
		mutex_lock(mutex);
	}
}

/* so these don't need the mutex as we don't modify the queue */

void cond_signal(cond_t *cond) {
	if (cond->waiting > 0) {
		--cond->waiting;
	}
}

void cond_broadcast(cond_t *cond) {
	cond->waiting = 0;
}
