
#include <stdlib.h>

void * caml_dlopen(char * libname, void * data, size_t len, int for_execution) {
	return NULL;
}

void * caml_dlsym(void * handle, char * name) {
	return NULL;
}

char * caml_dlerror(void) {
	return "caml_dl: unknown error";
}
