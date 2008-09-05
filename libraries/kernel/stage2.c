
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <stdio.h>
#include <asm.h>
#include <threads.h>

extern void caml_startup(char **args);

extern void idt_init();

void __startup(void *argv, int magic)
{
	dprintf("beginning bootstrap...\n");
	
	thread_init(); /* set up threading basics */
	idt_init(); /* this also sets up the interrupt handler for threads */
	
	asm volatile("int $0x30");
    
    dprintf("entering ocaml runtime...\n");
	
	caml_startup(argv);
	
	dprintf("entering blocking section...\n");
    
	caml_enter_blocking_section();
    
	dprintf("init finished\n");
	//thread_exit(); /* we have nothing to do now, so exit */
}

value snowflake_print_endline(value str) {
	char *s = String_val(str);
	char *video = (char *)0xB8000;
	
	while ( *s ) {
		*video++ = *s++;
		*video++ = 0x07;
	}
	return Val_unit;
}

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
  return prev_heap_end;
}
