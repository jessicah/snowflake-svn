
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <stdio.h>
#include <asm.h>
#include <threads.h>

extern void caml_startup(char **args);

extern void idt_init();

void __startup(void *argv, int magic)
{
	dprintf("Welcome to Snowflake Serial Debugging!\r\n");
	
	// set up C thread machinery, exception and irq handlers
	thread_init();
	idt_init();
	asm volatile("int $0x30");
	
	caml_startup(argv);
	caml_enter_blocking_section();
	
	// caml_startup has finished initialising the OS
	dprintf("INFO: Startup completed. Exiting startup thread...\r\n");
}

// simplistic handing out of memory to malloc()
extern char *sbrk(int);

char *sbrk(int incr){
  extern char end;
  static char *heap_end;
  char *prev_heap_end;

  if ( heap_end == 0 ) {
    heap_end = &end;
  }
  prev_heap_end = heap_end;

  heap_end += incr;
  // FIXME: require check to see if physical memory is exhausted
  return prev_heap_end;
}
