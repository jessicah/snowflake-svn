# Inspect OCaml values in GDB via Python scripting.
#
# http://ygrek.org.ua/p/code/mlvalues.py
# (c) 2011 ygrek
#
# Description
# -----------
#
# You will need GDB with python support (>=7.0?).
# This code reimplements Std.dump from extlib and is
# limited to information available in runtime representation
# of OCaml values. Not complete, doesn't handle cycles at all,
# tested only on x64.
#
# At GDB prompt input:
#    source mlvalues.py
# Then inspect any OCaml value:
#    ml_dump <value address or symbol>
# Or inspect the chunk of memory treating contents as values:
#    ml_dump <start address> <number of values>
# Show OCaml heap information:
#    ml_heap
#
# Changelog
# ---------
#
# 2012-04-11
#   Dump the array of OCaml values with ml_dump
#   Inspect closure blocks
#   Tweak formatting
#
# 2012-03-07
#   New command `ml_heap` - shows some general information about OCaml heap:
#     * GC parameters
#     * total heap size
#     * allocated chunks layout
#
# 2011-12-27
#   Show symbol and name for custom values
#   Catch gdb.MemoryError and continue printing
#   Correctly lookup types
#
# 2011-12-24
#   Initial


# This class represents gdb.Value as OCaml value.
# Analogue to stdlib Obj module.
#
# It probably contains more casts then strictly necessary,
# but after all who am I to understand python type system?
# Just a mere OCaml coder. And this is my first python program.
class OCamlValue:

  LAZY_TAG = 246
  CLOSURE_TAG = 247
  OBJECT_TAG = 248
  INFIX_TAG = 249
  FORWARD_TAG = 250
  NO_SCAN_TAG = 251
  ABSTRACT_TAG = 251
  STRING_TAG = 252
  DOUBLE_TAG = 253
  DOUBLE_ARRAY_TAG = 254
  CUSTOM_TAG = 255
  FINAL_TAG = 255
  INT_TAG = 1000
  OUT_OF_HEAP_TAG = 1001
  UNALIGNED_TAG = 1002

  def __init__(self,v):
    # do not lookup types at class level cause this script may be run before
    # the inferior image is loaded and gdb can't know sizeof(long) in advance
    self.intnat = gdb.lookup_type("long")
    self.t_charp = gdb.lookup_type("char").pointer()
    self.t_doublep = gdb.lookup_type("double").pointer()
    if type(v) == type(0):
      self.v = gdb.Value(v)
    else:
      self.v = v.cast(self.intnat)

  def is_int(self):
    return self.v & 1 != 0

  def is_block(self):
    return self.v & 1 == 0

  @staticmethod
  def of_int(x):
    return OCamlValue(gdb.Value((x<<1) + 1))

  @staticmethod
  def of_val(x):
    assert(x & 1 == 0)
    return OCamlValue(gdb.Value(x))

  @staticmethod
  def of_bool(x):
    return OCamlValue.of_int(x != 0)

  def int(self):
    assert self.is_int()
    return self.v >> 1

  def val(self):
    return self.v

  def string(self,enc='ascii'):
    assert self.tag() == OCamlValue.STRING_TAG
    # FIXME caml_string_length
    return self.v.reinterpret_cast(self.t_charp).string(enc)

  def float(self):
    assert self.tag() == OCamlValue.DOUBLE_TAG
    # FIXME test
    return self.v.reinterpret_cast(self.t_doublep).dereference()

  def __cmp__(self,other):
    if self.v == other.v:
      return 0
    else:
      if self.v > other.v:
        return 1
      else:
        return -1

  def __str__(self):
    if self.is_int():
      return ("%d" % self.int())
    else:
      return ("0x%X" % self.val())

  def __repr__(self):
    return "OCamlValue(%s)" % self.__str__()

  def hd(self):
    return (self.v - self.intnat.sizeof).reinterpret_cast(self.intnat.pointer()).dereference()

  def tag(self):
    if self.is_int():
      return OCamlValue.INT_TAG
    else:
      return self.hd() & 0xFF

  def unsafe_field(self,i):
    x = (self.v + i * self.intnat.sizeof).reinterpret_cast(self.intnat.pointer()).dereference()
    return OCamlValue(x)

  def field(self,i):
    assert self.is_block()
    assert self.tag () != OCamlValue.DOUBLE_ARRAY_TAG # FIXME not implemented
    n = self.size()
    if i < 0 or i >= n:
      raise IndexError("field %d size %d" % (i,n))
    return self.unsafe_field(i)
    #t = self.intnat.array(n).pointer()
    #return OCamlValue(self.v.reinterpret_cast(t).dereference()[i])

  def fields(self):
    assert self.is_block()
    assert self.tag () != OCamlValue.DOUBLE_ARRAY_TAG # FIXME not implemented
    a = []
    for i in range(self.size()):
      a.append(self.unsafe_field(i))
    return a

  def size(self):
    assert self.is_block()
    return self.hd() >> 10

  def bsize(self):
    return self.size() * self.intnat.sizeof

  def is_list(self):
    if self.is_int():
      return self.int() == 0
    else:
      return self.size() == 2 and self.tag() == 0 and self.field(1).is_list()

  def get_list(self):
    a = []
    l = self
    while l.is_block():
      a.append(l.field(0))
      l = l.field(1)
    return a

  def resolve(self):
    symbol = gdb.execute('info symbol ' + str(self.val()),False,True).split(' ',1)[0]
    if symbol == "No": # FIXME "No symbol matches"
      return "0x%x" % self.val()
    else:
      return "%s" % symbol

  def show_opaque(self,s):
    print "<%s at 0x%x>" % (s,self.val()),

  def show_all(self,seq,delim,raw=False):
    for i, x in enumerate(seq):
      if i:
        print delim,
      if raw:
        print x.resolve(),
      else:
        x.show()

  def show(self):
    try:
      if self.is_int():
        print "%d" % self.int(),
      elif self.is_list():
        print "[",
        self.show_all(self.get_list(), ';')
        print "]",
      else:
        t = self.tag()
        if t == 0:
          print "(",
          self.show_all(self.fields(), ',')
          print ")",
        elif t == OCamlValue.LAZY_TAG:
          self.show_opaque("lazy")
        elif t == OCamlValue.CLOSURE_TAG:
          print "Closure(",
          self.show_all(self.fields(), ',', raw=True)
          print ")",
        elif t == OCamlValue.OBJECT_TAG:
#	| x when x = Obj.object_tag ->
#		let fields = get_fields [] s in
#		let clasz, id, slots =
#			match fields with
#			| h::h'::t -> h, h', t 
#			| _ -> assert false
#		in
#		(* No information on decoding the class (first field).  So just print
#		* out the ID and the slots. *)
#		"Object #" ^ dump id ^ " (" ^ String.concat ", " (List.map dump slots) ^ ")"
# FIXME todo
          self.show_opaque("object")
        elif t == OCamlValue.INFIX_TAG:
          self.show_opaque("infix")
        elif t == OCamlValue.FORWARD_TAG:
          self.show_opaque("forward")
        elif t < OCamlValue.NO_SCAN_TAG:
          print "Tag%d(" % t,
          self.show_all(self.fields(), ',')
          print ")",
        elif t == OCamlValue.STRING_TAG:
          print self.string().__repr__(),
        elif t == OCamlValue.DOUBLE_TAG:
          print "%f" % self.float(),
        elif t == OCamlValue.ABSTRACT_TAG:
          self.show_opaque("abstract")
        elif t == OCamlValue.CUSTOM_TAG:
          # FIXME better handle builtin caml custom values : int32, int64, etc
          try:
            sym = self.field(0).resolve()
          except:
            sym = '?'
          try:
            name = self.field(0).val().cast(self.t_charp.pointer()).dereference().string()
          except:
            name = ''
            raise
          self.show_opaque("custom " + sym + ' "' + name + '"')
        elif t == OCamlValue.FINAL_TAG:
          self.show_opaque("final")
        elif t == OCamlValue.DOUBLE_ARRAY_TAG:
          print "<float array>",
#        return "[|%s|]" % "; ".join([x.dump() for x in self.fields()])
        else:
          self.show_opaque("unknown tag %d size %d" % (t,self.size()))
    except gdb.MemoryError as exn:
      print '<<gdb.MemoryError : %s>>' % exn,

#  nil = OCamlValue.of_int(0)
#  true = OCamlValue.of_int(1)
#  false = OCamlValue.of_int(0)

class DumpOCamlValue(gdb.Command):
  """Dump OCaml value: ml_dump <value>
     Dump the array of OCaml values: ml_dump <addr> <N>
 
Recursively dump runtime representation of OCaml value """

  def __init__(self):
    gdb.Command.__init__(self, "ml_dump", gdb.COMMAND_DATA, gdb.COMPLETE_SYMBOL, True)

  def invoke(self, arg, from_tty):
    arg_list = gdb.string_to_argv(arg)
    if len(arg_list) < 1:
      print "usage: ml_dump value"
      return
    size_t = gdb.lookup_type("size_t")
    addr = gdb.parse_and_eval(arg_list[0]).cast(size_t.pointer())
    if len(arg_list) > 1:
      for i in range(int(arg_list[1])):
        print "0x%x:" % (addr + i).cast(size_t),
        OCamlValue((addr + i).dereference()).show()
        print ""
    else:
      OCamlValue(addr).show()
      print ""

DumpOCamlValue()

class ShowOCamlHeap(gdb.Command):
  """Show some facts about OCaml heap: ml_heap """

  def __init__(self):
    gdb.Command.__init__(self, "ml_heap", gdb.COMMAND_NONE, gdb.COMPLETE_NONE, False)

  def e(self,x,t="int"):
    return gdb.parse_and_eval("*(("+str(t)+"*)&"+x+")")

  def malloced_size(self,x):
    size_t = gdb.lookup_type("size_t")
    # see caml_aligned_malloc, FIXME Page_size = 4K assumption
    return x + 4*size_t.sizeof + 4*1024

  def invoke(self, arg, from_tty):
    size_t = gdb.lookup_type("size_t")

    print "     major heap size =", self.e("caml_stat_heap_size"), "bytes"
    print " major heap top size =", self.e("caml_stat_top_heap_size"), "bytes"
    print "   total heap chunks =", self.e("caml_stat_heap_chunks")
    print "         gray values =", gdb.parse_and_eval("gray_vals_size") * size_t.sizeof, "bytes"
    print "extra heap resources =", self.e("caml_extra_heap_resources","double")
    print
    print "minor heap :"
    minor_size = self.e("caml_minor_heap_size")
    minor_base = self.e("caml_young_ptr","size_t")
    print "0x%x %d bytes (0x%x-0x%x)" % \
      (self.e("caml_young_start","size_t"), minor_size, minor_base, minor_base + self.malloced_size(minor_size))
    print
    print "major heap :"
    # the following casting is needed, otherwise gdb may sign-extend values without debug info
    v = gdb.parse_and_eval("*((size_t*)&caml_heap_start)")
    i = 0
    while v != 0:
      p = gdb.Value(v - 4 * size_t.sizeof)
# typedef struct {
#   void *block;           /* address of the malloced block this chunk live in */
#   asize_t alloc;         /* in bytes, used for compaction */
#   asize_t size;          /* in bytes */
#   char *next;
# } heap_chunk_head;
      p = p.cast(size_t.pointer())
      block = p.dereference()
      size = (p + 2).dereference()
      print "%2d) chunk 0x%x %d bytes (0x%x-0x%x)" % (i, v, size, block, block + self.malloced_size(size))
      i = i + 1
      v = (p + 3).dereference()

ShowOCamlHeap()

