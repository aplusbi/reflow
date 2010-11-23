#include <stdio.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

value ocaml_posix_openpt(value flags)
{
	CAMLparam1(flags);
	int pid = posix_openpt(0);
	if(pid == -1)
		uerror("posix_openpt", Nothing);
	CAMLreturn(Val_int(pid));
}
