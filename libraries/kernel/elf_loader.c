#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "elf.h"

#define hash_nbucket(h) ((h)[0])
#define hash_nchain(h) ((h)[1])
#define hash_bucket(h) (&(h)[2])
#define hash_chain(h) (&hash_bucket((h))[hash_nbucket((h))])

extern Elf32_Dyn _DYNAMIC[];

/** ELF symbol hashing function */
static unsigned long elf_hash(const unsigned char *name)
{
	unsigned long h = 0, g;
	while(*name) {
		h = (h << 4) + *name++;
		if((g = (h & 0xF0000000)))
			h ^= g >> 24;
		h &= ~g;
	}
	return h;
}

/** Find a symbol in an ELF hashtable
 * @param hashtab Hashtable to look at
 * @param strtab String table to use
 * @param symtab Symbol table to use
 * @param strtab_size Size of the string table
 * @param syment Size of each symbol table entry
 * @param name Name of the symbol to find
 * @returns Address of the symbol, or NULL if not found */
static void *get_sym(Elf32_Word *hashtab, char *strtab, void *symtab, Elf32_Word strtab_size, Elf32_Word syment, const char *name)
{
	unsigned long hash;
	Elf32_Sym *sym;
	Elf32_Word y;	/* Symtab/chain index */

	hash = elf_hash((const unsigned char *)name);
	y = hash_bucket(hashtab)[hash % hash_nbucket(hashtab)];
	while(y != STN_UNDEF) {
		assert(y < hash_nchain(hashtab));
		sym = (Elf32_Sym *)(symtab + y * syment);
		assert(sym->st_name < strtab_size);
		if(strcmp(strtab + sym->st_name, name) == 0) {
			return (void *)sym->st_value;
		}
		y = hash_chain(hashtab)[y];
	}

	return NULL;
}

/** Find a symbol in the kernel symbol table
 * @param name Name of the symbol to find
 * @returns Address of the symbol or NULL */
static void *get_kernel_sym(const char *name)
{
	static Elf32_Word *khashtab;
	static void *ksymtab;
	static char *kstrtab;
	static Elf32_Word kstrtab_size = -1;
	static Elf32_Word ksyment = -1;

	if(khashtab == NULL) {
		Elf32_Dyn *i;

		for(i = _DYNAMIC; i->d_tag != DT_NULL; i++) {
			if(i->d_tag == DT_HASH) {
				khashtab = (Elf32_Word *)i->d_un.d_ptr;
			} else if(i->d_tag == DT_STRTAB) {
				kstrtab = (char *)i->d_un.d_ptr;
			} else if(i->d_tag == DT_SYMTAB) {
				ksymtab = (void *)i->d_un.d_ptr;
			} else if(i->d_tag == DT_STRSZ) {
				kstrtab_size = i->d_un.d_val;
			} else if(i->d_tag == DT_SYMENT) {
				ksyment = i->d_un.d_val;
			}
		}
		assert(khashtab);
		assert(ksymtab);
		assert(kstrtab);
		assert(kstrtab_size != (Elf32_Word)-1);
		assert(ksyment != (Elf32_Word)-1);
	}

	return get_sym(khashtab, kstrtab, ksymtab, kstrtab_size, ksyment, name);
}

typedef union {
	Elf32_Word d_val;		/* Integer value */
	Elf32_Addr d_ptr;		/* Address value */
} d_un_t;

typedef d_un_t dynamic_info_t[DT_NUM];

char *sym_name(dynamic_info_t *dynamic_info, unsigned int sym_id)
{
	Elf32_Sym *sym;
	char *name;
	sym = (Elf32_Sym *)((*dynamic_info)[DT_SYMTAB].d_ptr + sym_id * (*dynamic_info)[DT_SYMENT].d_val);
	name = (char *)((*dynamic_info)[DT_STRTAB].d_ptr + sym->st_name);

	return name;
}

uint32_t find_sym(void *image, dynamic_info_t *dynamic_info, unsigned int sym_id)
{
	Elf32_Sym *sym;
	uint32_t symval;

	sym = (Elf32_Sym *)((*dynamic_info)[DT_SYMTAB].d_ptr + sym_id * (*dynamic_info)[DT_SYMENT].d_val);
	if(sym->st_shndx == SHN_UNDEF) {
		symval = (uint32_t)get_kernel_sym(sym_name(dynamic_info, sym_id));
		if(!symval) {
			if(ELF32_ST_BIND(sym->st_info) == STB_WEAK) {
				dprintf("find_sym: Weak symbol '%s'\n", sym_name(dynamic_info, sym_id));
				return 0;
			} else {
				return -1;
			}
		}
		return symval;
	} else {
		return sym->st_value + (uint32_t)image;
	}
}

/* returns "string * (string * int) list"
           "soname * (sym name * sym value) list" */
CAMLprim value caml_elfopen(value filename, value data)
{
	CAMLparam1(data);
	CAMLlocal4(cell, list, symbol, result);

	dynamic_info_t dynamic_info;

	/* dynamic = pointer to the base, dyn = iterator */
	Elf32_Dyn *dynamic = NULL, *dyn;
	Elf32_Ehdr *ehdr;
	Elf32_Phdr *phdr;
	void *buf, *phdrs;
	size_t buf_len, memsz = 0;
	int i;
	void *targ_image;

	memset(dynamic_info, 0, sizeof(dynamic_info_t));
	dynamic_info[DT_SONAME].d_ptr = -1;

	buf = String_val(data);
	buf_len = caml_string_length(data);

	if(buf_len < sizeof(Elf32_Ehdr)) {
		caml_invalid_argument("caml_dlopen: buffer too short!");
	}

	ehdr = buf;
	if(ehdr->e_ident[EI_MAG0] != ELFMAG0 || ehdr->e_ident[EI_MAG1] != ELFMAG1 ||
			ehdr->e_ident[EI_MAG2] != ELFMAG2 || ehdr->e_ident[EI_MAG3] != ELFMAG3) {
		caml_invalid_argument("caml_dlopen: not an ELF file");
	}
	if(ehdr->e_ident[EI_CLASS] != ELFCLASS32 || ehdr->e_ident[EI_DATA] != ELFDATA2LSB ||
			ehdr->e_type != ET_DYN || ehdr->e_machine != EM_386) {
		caml_invalid_argument("caml_dlopen: can't load this file");
	}

	phdrs = buf + ehdr->e_phoff;
	dyn = NULL;

	/* Work out in-memory size and get dynamic section */
	for(i = 0; i < ehdr->e_phnum; i++) {
		phdr = phdrs + i * ehdr->e_phentsize;

		if(phdr->p_type == PT_LOAD) {
			if(phdr->p_vaddr + phdr->p_memsz > memsz) {
				memsz = phdr->p_vaddr + phdr->p_memsz;
			}
		} else if(phdr->p_type == PT_DYNAMIC) {
			if(dyn != NULL) {
				caml_invalid_argument("caml_dlopen: file has more than 1 dynamic section");
			}
			dynamic = buf + phdr->p_offset;
		}
	}
	if(dynamic == NULL) {
		caml_invalid_argument("caml_dlopen: file lacks a dynamic section");
	}

	/* Find important stuff */
	for(dyn = dynamic; dyn->d_tag != DT_NULL; dyn++) {
		if(dyn->d_tag == DT_TEXTREL) {
			caml_invalid_argument("caml_dlopen: file contains text relocations");
		} else if(dyn->d_tag < DT_NUM) {
			dynamic_info[dyn->d_tag].d_val = dyn->d_un.d_val;
		}
	}

	targ_image = memalign(0x1000, memsz);
	if(targ_image == NULL) {
		caml_failwith("caml_dlopen: no memory!");
	}

	/* Target image allocated, relocate stuff in dynamic_info */
	if(dynamic_info[DT_HASH].d_ptr == 0) {
		free(targ_image);
		caml_invalid_argument("caml_dlopen: file has no hash table");
	} else {
		dynamic_info[DT_HASH].d_ptr += (Elf32_Addr)buf;
	}
	if(dynamic_info[DT_STRTAB].d_ptr == 0) {
		free(targ_image);
		caml_invalid_argument("caml_dlopen: file has no string table");
	} else {
		dynamic_info[DT_STRTAB].d_ptr += (Elf32_Addr)buf;
		if(dynamic_info[DT_STRSZ].d_val == 0) {
			free(targ_image);
			caml_invalid_argument("caml_dlopen: file has DT_STRTAB but missing DT_STRSZ");
		}
	}
	if(dynamic_info[DT_SYMTAB].d_ptr == 0) {
		free(targ_image);
		caml_invalid_argument("caml_dlopen: file has no symbol table");
	} else {
		dynamic_info[DT_SYMTAB].d_ptr += (Elf32_Addr)buf;
		if(dynamic_info[DT_SYMENT].d_val == 0) {
			free(targ_image);
			caml_invalid_argument("caml_dlopen: file has DT_SYMTAB but missing DT_SYMENT");
		}
	}
	if(dynamic_info[DT_RELA].d_ptr) {
		dynamic_info[DT_RELA].d_ptr += (Elf32_Addr)buf;
		if(dynamic_info[DT_RELASZ].d_val == 0 || dynamic_info[DT_RELAENT].d_val == 0) {
			free(targ_image);
			caml_invalid_argument("caml_dlopen: file has DT_RELA but missing DT_RELASZ or DT_RELAENT");
		}
	}
	if(dynamic_info[DT_REL].d_ptr) {
		dynamic_info[DT_REL].d_ptr += (Elf32_Addr)buf;
		if(dynamic_info[DT_RELSZ].d_val == 0 || dynamic_info[DT_RELENT].d_val == 0) {
			free(targ_image);
			caml_invalid_argument("caml_dlopen: file has DT_REL but missing DT_RELSZ or DT_RELENT");
		}
	}

	if(dynamic_info[DT_JMPREL].d_ptr == 0) {
		free(targ_image);
		caml_invalid_argument("caml_dlopen: file missing DT_JMPREL");
	} else {
		dynamic_info[DT_JMPREL].d_ptr += (Elf32_Addr)buf;
		if(dynamic_info[DT_PLTREL].d_val == 0 || dynamic_info[DT_PLTRELSZ].d_val == 0 || dynamic_info[DT_PLTGOT].d_val == 0) {
			free(targ_image);
			caml_invalid_argument("caml_dlopen: file has DT_JMPREL but missing DT_PLTREL, DT_PLTRELSZ or DT_PLTGOT");
		}
		if(dynamic_info[DT_PLTREL].d_val != DT_REL) {
			free(targ_image);
			caml_invalid_argument("caml_dlopen: DT_PLTREL != DT_REL");
		}
		if(dynamic_info[DT_RELENT].d_val == 0) {
			/* Guess */
			dynamic_info[DT_RELENT].d_val = sizeof(Elf32_Rel);
		}
	}
	if(dynamic_info[DT_INIT].d_ptr) {
		dynamic_info[DT_INIT].d_ptr += (Elf32_Addr)targ_image;
	}
	if(dynamic_info[DT_FINI].d_ptr) {
		dynamic_info[DT_FINI].d_ptr += (Elf32_Addr)targ_image;
	}
	if(dynamic_info[DT_SONAME].d_ptr == (Elf32_Addr)-1) {
		dynamic_info[DT_SONAME].d_ptr = (Elf32_Addr)0;
	} else {
		dynamic_info[DT_SONAME].d_ptr += dynamic_info[DT_STRTAB].d_ptr;
	}

	/* Now we have the important stuff, check that this lib only depends on proper libs */
	for(dyn = dynamic; dyn->d_tag != DT_NULL; dyn++) {
		if(dyn->d_tag == DT_NEEDED) {
			if(strncmp("libc.so", (char *)dynamic_info[DT_STRTAB].d_ptr + dyn->d_un.d_val, 7) == 0) {
				/* Nothing */
				dprintf("caml_dlopen: ignoring dependency on %s\n", dynamic_info[DT_STRTAB].d_ptr + dyn->d_un.d_val);
			} else if(strncmp("libm.so", (char *)dynamic_info[DT_STRTAB].d_ptr + dyn->d_un.d_val, 7) == 0) {
				/* Nothing */
				dprintf("caml_dlopen: ignoring dependency on %s\n", dynamic_info[DT_STRTAB].d_ptr + dyn->d_un.d_val);
			} else {
				dprintf("caml_dlopen: file depends on %s\n", dynamic_info[DT_STRTAB].d_ptr + dyn->d_un.d_val);
				caml_invalid_argument("caml_dlopen: file depends on other libraries");
			}
		}
	}

	/* Copy loaded sections to targ_image */
	for(i = 0; i < ehdr->e_phnum; i++) {
		phdr = phdrs + i * ehdr->e_phentsize;

		if(phdr->p_type == PT_LOAD) {
			memcpy(targ_image + phdr->p_vaddr, buf + phdr->p_offset, phdr->p_filesz);
			if(phdr->p_memsz > phdr->p_filesz) {
				memset(targ_image + phdr->p_vaddr + phdr->p_filesz, 0, phdr->p_memsz - phdr->p_filesz);
			}
		}
	}

	Elf32_Rel *rel = NULL;
	uint32_t *where, symval;

	/* Perform relocations */
	if(dynamic_info[DT_REL].d_ptr) {
		for(i = 0; i < dynamic_info[DT_RELSZ].d_val/dynamic_info[DT_RELENT].d_val; i++) {
			rel = (Elf32_Rel *)(dynamic_info[DT_REL].d_ptr + i * dynamic_info[DT_RELENT].d_val);
			where = targ_image + rel->r_offset;
			switch(ELF32_R_TYPE(rel->r_info)) {
			case R_386_RELATIVE: /* B + A */
				*where += (uint32_t)targ_image;
				break;
			case R_386_GLOB_DAT: /* S */
				symval = find_sym(targ_image, &dynamic_info, ELF32_R_SYM(rel->r_info));
				if(symval == -1) {
					free(targ_image);
					dprintf("caml_dlopen: Can't find '%s'/%u\n", sym_name(&dynamic_info, ELF32_R_SYM(rel->r_info)), ELF32_R_SYM(rel->r_info));
					caml_failwith("caml_dlopen: can't find external symbol (in DT_REL)");
				}

				*where = symval;
				break;
			case R_386_32: /* S + A */
				symval = find_sym(targ_image, &dynamic_info, ELF32_R_SYM(rel->r_info));
				if(symval == -1) {
					free(targ_image);
					dprintf("caml_dlopen: Can't find '%s'/%u\n", sym_name(&dynamic_info, ELF32_R_SYM(rel->r_info)), ELF32_R_SYM(rel->r_info));
					caml_failwith("caml_dlopen: can't find symbol (in DT_REL)");
				} else if(symval == 0) { /* Weak symbol */
					*where = 0;
				} else {
					*where += symval;
				}
				break;
			case R_386_PC32: /* S + A - P */
				symval = find_sym(targ_image, &dynamic_info, ELF32_R_SYM(rel->r_info));
				if(symval == -1) {
					free(targ_image);
					dprintf("caml_dlopen: Can't find '%s'/%u\n", sym_name(&dynamic_info, ELF32_R_SYM(rel->r_info)), ELF32_R_SYM(rel->r_info));
					caml_failwith("caml_dlopen: can't find symbol (in DT_REL)");
				} else if(symval == 0) { /* Weak symbol */
					*where = 0;
				} else {
				        *where += symval - (unsigned long)where;
				}
				break;
			case R_386_JMP_SLOT: /* S */
				symval = find_sym(targ_image, &dynamic_info, ELF32_R_SYM(rel->r_info));
				if(symval == -1) {
					free(targ_image);
					dprintf("caml_dlopen: Can't find '%s'/%u\n", sym_name(&dynamic_info, ELF32_R_SYM(rel->r_info)), ELF32_R_SYM(rel->r_info));
					caml_failwith("caml_dlopen: can't find symbol (in DT_REL)");
				} else if(symval == 0) { /* Weak symbol */
					*where = 0;
				} else {
					*where = symval;
				}
				break;
			default:
				free(targ_image);
				caml_failwith("caml_dlopen: can't handle relocation!");
			}
		}
	}
	if(dynamic_info[DT_RELA].d_ptr) {
		free(targ_image);
		caml_failwith("caml_dlopen: TODO: handle DT_RELA relocations");
	}

	if(dynamic_info[DT_JMPREL].d_ptr) {
		for(i = 0; i < dynamic_info[DT_PLTRELSZ].d_val/dynamic_info[DT_RELENT].d_val; i++) {
			rel = (Elf32_Rel *)(dynamic_info[DT_JMPREL].d_ptr + i * dynamic_info[DT_RELENT].d_val);
			where = targ_image + rel->r_offset;
			if(ELF32_R_TYPE(rel->r_info) == R_386_JMP_SLOT) {
				symval = find_sym(targ_image, &dynamic_info, ELF32_R_SYM(rel->r_info));
				if(symval == -1) {
					free(targ_image);
					dprintf("caml_dlopen: Can't find '%s'/%u\n", sym_name(&dynamic_info, ELF32_R_SYM(rel->r_info)), ELF32_R_SYM(rel->r_info));
					caml_failwith("caml_dlopen: can't find symbol (in DT_JMPREL)");
				}

				*where = symval;
			} else {
				free(targ_image);
				caml_failwith("caml_dlopen: can't handle relocation (in DT_JMPREL)!");
			}
		}
	}

	if(dynamic_info[DT_INIT].d_ptr) {
		void (*initf)(void) = (void (*)(void))dynamic_info[DT_INIT].d_ptr;
		initf();
	}

	/* Relocations all done, build the result tuple */
	Elf32_Sym *sym;
	result = caml_alloc_tuple(2);
	list = Val_int(0);
	for(i = 0; i < hash_nchain((Elf32_Word *)dynamic_info[DT_HASH].d_ptr); i++) {
		sym = (Elf32_Sym *)(dynamic_info[DT_SYMTAB].d_ptr + i * dynamic_info[DT_SYMENT].d_val);

		/* Symbols must be named & not be absolute & not be external */
		if(sym->st_name != 0 && sym->st_shndx != SHN_UNDEF && sym->st_shndx != SHN_ABS) {
			cell = caml_alloc_tuple(2);
			symbol = caml_alloc_tuple(2);

			Store_field(symbol, 0, caml_copy_string((char *)dynamic_info[DT_STRTAB].d_ptr + sym->st_name));
			Store_field(symbol, 1, (value)((void *)(sym->st_value + targ_image)));

			Store_field(cell, 0, symbol);
			Store_field(cell, 1, list);
			list = cell;
		}
	}
	if (dynamic_info[DT_SONAME].d_ptr) {
		Store_field(result, 0, caml_copy_string((char *)dynamic_info[DT_SONAME].d_ptr));
	} else {
		Store_field(result, 0, filename);
	}
	Store_field(result, 1, list);

	CAMLreturn(result);
}
