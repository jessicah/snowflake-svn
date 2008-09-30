
#define RUNNABLE 0
#define BLOCKED 1
#define KILLED 2
#define EXITED 4

typedef struct thread {
	unsigned long *stack;
	unsigned long *esp;
	void *slot;
	unsigned long id;
	unsigned long status;
	struct thread * next;
	struct thread * prev;
} thread_t;

typedef struct thread_queue {
	thread_t *thread;
	struct thread_queue *next;
} thread_queue_t;

typedef struct mutex {
	thread_queue_t *queue;
	thread_queue_t *head;
	thread_t *owner;
	unsigned long id;
} mutex_t;

typedef struct cond {
	thread_queue_t *queue;
	thread_queue_t *head;
	unsigned long waiting;
	unsigned long id;
} cond_t;

typedef void *(*thread_func)(void *);

extern void thread_init();
extern void thread_yield();
extern void thread_exit(void *);
extern void thread_create(thread_t *, void *(*)(void *), void *);
extern thread_t thread_self();
extern void thread_setspecific(void *);
extern void *thread_getspecific();

//extern mutex_t *mutex_create();
extern void mutex_init(mutex_t *);
extern void mutex_destroy(mutex_t *);
extern void mutex_lock(mutex_t *);
extern void mutex_unlock(mutex_t *);
extern int mutex_trylock(mutex_t *);

//extern cond_t *cond_create();
extern void cond_init(cond_t *);
extern void cond_destroy(cond_t *);
extern void cond_wait(cond_t *, mutex_t *);
extern void cond_signal(cond_t *);
extern void cond_broadcast(cond_t *);
