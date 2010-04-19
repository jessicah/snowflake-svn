
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>

#include <asm.h>
#include <string.h>

CAMLprim value snowflake_out8(value port, value val) {
	out8(Int_val(port), Int_val(val));
	return Val_unit;
}

CAMLprim value snowflake_out16(value port, value val) {
	out16(Int_val(port), Int_val(val));
	return Val_unit;
}

CAMLprim value snowflake_out32(value port, value val) {
	out32(Int_val(port), Int32_val(val));
	return Val_unit;
}

CAMLprim value snowflake_in8(value port) {
	return Val_int(in8(Int_val(port)));
}

CAMLprim value snowflake_in16(value port) {
	return Val_int(in16(Int_val(port)));
}

CAMLprim value snowflake_in32(value port) {
	return caml_copy_int32(in32(Int_val(port)));
}

CAMLprim value snowflake_hlt(value unit) {
	asm("hlt");
	return unit;
}

CAMLprim value snowflake_cli(value unit) {
	asm("cli");
	return unit;
}

CAMLprim value snowflake_sti(value unit) {
	asm("sti");
	return unit;
}

CAMLprim value snowflake_in16s(value port, value count) {
	value string = caml_alloc_string(Int_val(count) * 2);
	void *addr = (void *)String_val(string);
	ins16(Int_val(port), Int_val(count), addr);
	return string;
}

CAMLprim value snowflake_out16s(value port, value string, value count) {
	void *addr = (void *)String_val(string);
	outs16(Int_val(port), Int_val(count), addr);
	return Val_unit;
}

CAMLprim value snowflake_address(value ba) {
	unsigned long addr = (unsigned long)Caml_ba_data_val(ba);
	return caml_copy_int32(addr);
}

CAMLprim value snowflake_array8(value address, value length) {
	intnat dims[] = { Long_val(length) };
	value ba = caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, (void *)Int32_val(address), dims);
	return ba;
}

CAMLprim value snowflake_array16(value address, value length) {
	intnat dims[] = { Long_val(length) };
	value ba = caml_ba_alloc(CAML_BA_UINT16 | CAML_BA_C_LAYOUT, 1, (void *)Int32_val(address), dims);
	return ba;
}

CAMLprim value snowflake_array32(value address, value length) {
	intnat dims[] = { Long_val(length) };
	value ba = caml_ba_alloc(CAML_BA_INT32 | CAML_BA_C_LAYOUT, 1, (void *)Int32_val(address), dims);
	return ba;
}

CAMLprim value snowflake_matrix8(value address, value dim1, value dim2) {
	intnat dims[] = { Long_val(dim1), Long_val(dim2) };
	value ba = caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 2, (void *)Int32_val(address), dims);
	return ba;
}

CAMLprim value snowflake_matrix16(value address, value dim1, value dim2) {
	intnat dims[] = { Long_val(dim1), Long_val(dim2) };
	value ba = caml_ba_alloc(CAML_BA_UINT16 | CAML_BA_C_LAYOUT, 2, (void *)Int32_val(address), dims);
	return ba;
}

CAMLprim value snowflake_matrix32(value address, value dim1, value dim2) {
	intnat dims[] = { Long_val(dim1), Long_val(dim2) };
	value ba = caml_ba_alloc(CAML_BA_INT32 | CAML_BA_C_LAYOUT, 2, (void *)Int32_val(address), dims);
	return ba;
}

CAMLprim value get_dma_region(value unit) {
	long dims[] = { 0x10000 };
	value r = caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 2, (unsigned char *)(0x100000), dims);
	return r;
}

CAMLprim value snowflake_peek32(value address) {
    return caml_copy_int32(*(volatile uint32 *)((char *)(Int32_val(address))));
}

CAMLprim value snowflake_poke32(value address, value data) {
    *(volatile uint32 *)((char *)(Int32_val(address))) = Int32_val(data);
    return Val_unit;
}

CAMLprim value snowflake_peek32_offset(value address, value offset) {
    return caml_copy_int32(*(volatile uint32 *)((char *)(Int32_val(address)) + Int_val(offset)));
}

CAMLprim value snowflake_poke32_offset(value address, value offset, value data) {
    *(volatile uint32 *)((char *)(Int32_val(address)) + Int_val(offset)) = Int32_val(data);
    return Val_unit;
}

typedef union {
	unsigned long long tick;
	struct {
		unsigned long low;
		unsigned long high;
	} sub;
} tick_t;

#define ticks(tick) __asm__ __volatile__("rdtsc" : "=a" \
				((tick).sub.low),"=d" ((tick).sub.high));

unsigned long snowflake_random_seed() {
	tick_t tick;
	ticks(tick);
	tick.tick /= 1000ULL;
	return (unsigned long)tick.tick;
}

CAMLprim value snowflake_rdtsc(value unit) {
	tick_t tick;
	ticks(tick);
	return caml_copy_int64(tick.tick >> 16);
}

CAMLprim value caml_sys_random_seed (value unit)
{
	return Val_long(snowflake_random_seed());
}

unsigned long long get_ticks()
{
	tick_t tick;
	ticks(tick);
	return (tick.tick >> 16);
}

/*
CAMLprim value snowflake_usleep(value usec) {
	tick_t start, tick;
	int diff = Int_val(usec);
	ticks(start);
	do {
		ticks(tick);
	} while (tick
*/
