#include <stdio.h>
#include <stdlib.h>
#include<fcntl.h>
#include <termios.h>
#include <unistd.h>
#include <sys/types.h>
#include <limits.h>
#include <memory.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

CAMLprim value ocaml_sigwinch(value unit)
{	
	return Val_int(SIGWINCH);
}

CAMLprim value ocaml_get_winsize(value fd)
{
	CAMLparam1(fd);

	int ifd = Int_val(fd);
	struct winsize ws;
	if(-1 == ioctl(ifd, TIOCGWINSZ, &ws))
		uerror("get_winsize", fd);

	CAMLlocal1(ret);
	ret = caml_alloc(4, 0);
	Store_field(ret, 0, Val_int(ws.ws_row));
	Store_field(ret, 1, Val_int(ws.ws_col));
	Store_field(ret, 2, Val_int(ws.ws_xpixel));
	Store_field(ret, 3, Val_int(ws.ws_ypixel));
	CAMLreturn(ret);
}

CAMLprim value ocaml_set_winsize(value fd, value winp)
{
	CAMLparam2(fd, winp);

	int ifd = Int_val(fd);
	struct winsize ws;
	ws.ws_row = Int_val(Field(winp, 0));
	ws.ws_col = Int_val(Field(winp, 1));
	ws.ws_xpixel = Int_val(Field(winp, 2));
	ws.ws_ypixel = Int_val(Field(winp, 3));
	if(-1 == ioctl(ifd, TIOCSWINSZ, &ws))
		uerror("set_winsize", fd);

	CAMLreturn(Val_unit);
}

