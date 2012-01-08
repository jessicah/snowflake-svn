
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/intext.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* Create / update proxy to indicate that b2 is a sub-array of b1 */

static void caml_ba_update_proxy(struct caml_ba_array * b1,
                                 struct caml_ba_array * b2)
{
  struct caml_ba_proxy * proxy;
  /* Nothing to do for un-managed arrays */
  if ((b1->flags & CAML_BA_MANAGED_MASK) == CAML_BA_EXTERNAL) return;
  if (b1->proxy != NULL) {
    /* If b1 is already a proxy for a larger array, increment refcount of
       proxy */
    b2->proxy = b1->proxy;
    ++ b1->proxy->refcount;
  } else {
    /* Otherwise, create proxy and attach it to both b1 and b2 */
    proxy = caml_stat_alloc(sizeof(struct caml_ba_proxy));
    proxy->refcount = 2;      /* original array + sub array */
    proxy->data = b1->data;
    proxy->size =
      b1->flags & CAML_BA_MAPPED_FILE ? caml_ba_byte_size(b1) : 0;
    b1->proxy = proxy;
    b2->proxy = proxy;
  }
}

/* Convert between different array kinds */

value caml_ba_change_flags(value vb, value vkind, value vlen)
{
	CAMLparam3 (vb, vkind, vlen);
	CAMLlocal1 (res);
	#define b ((struct caml_ba_array *) Caml_ba_array_val(vb))
	int flags = Int_val(vkind) | CAML_BA_C_LAYOUT | CAML_BA_MANAGED;
	intnat len = Long_val(vlen);

	res = caml_ba_alloc(flags, b->num_dims, b->data, b->dim);
	Caml_ba_array_val(res)->dim[0] = len;
	caml_ba_update_proxy(b, Caml_ba_array_val(res));
	CAMLreturn (res);
	#undef b
}

/* Bigarray from string */
CAMLprim value caml_ba_from_string(value vkind, value vlayout, value vstr)
{
  intnat dim[CAML_BA_MAX_NUM_DIMS];
  mlsize_t num_dims;
  int i, flags;

  num_dims = 1;
  if (num_dims < 1 || num_dims > CAML_BA_MAX_NUM_DIMS)
    caml_invalid_argument("Bigarray.create: bad number of dimensions");
  for (i = 0; i < num_dims; i++) {
    dim[i] = caml_string_length(vstr);
    if (dim[i] < 0)
      caml_invalid_argument("Bigarray.create: negative dimension");
  }
  flags = Int_val(vkind) | Int_val(vlayout) | CAML_BA_EXTERNAL;
  return caml_ba_alloc(flags, num_dims, String_val(vstr), dim);
}
