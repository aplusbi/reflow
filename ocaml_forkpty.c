#include<stdio.h>
#include<stdlib.h>
#include <pty.h>
#include <termios.h>
#include <unistd.h>
#include <sys/types.h>
#include <limits.h>
#include <memory.h>
#include <signal.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>

value ocaml_sigwinch(value unit)
{	
	return Val_int(SIGWINCH);
}

value ocaml_forkpty(value f, value termp, value winp)
{
	CAMLparam3(f, termp, winp);
	struct termios term;
	struct winsize win;
	int fd = -1;
	pid_t pid = -1;
	char name[BUFSIZ];
	struct termios *pterm = NULL;
	struct winsize *pwin = NULL;

	memset(&term, 0, sizeof(term));
	memset(&win, 0, sizeof(win));

	if(Is_block(termp))
	{
		value t = Field(termp, 0);
		term.c_iflag = Int_val(Field(t, 0));
		term.c_oflag = Int_val(Field(t, 1));
		term.c_cflag = Int_val(Field(t, 2));
		value c_cc = Field(t, 3);
		int size = Wosize_val(c_cc) * sizeof(value);
		int i;
		for(i=0; i<size; ++i)
			term.c_cc[i] = Int_val(Field(c_cc, i));
		pterm = &term;
	}
	if(Is_block(winp))
	{
		value w = Field(winp, 0);
		win.ws_row = Int_val(Field(w, 0));
		win.ws_col = Int_val(Field(w, 1));
		win.ws_xpixel = Int_val(Field(w, 2));
		win.ws_ypixel = Int_val(Field(w, 3));
		pwin = &win;
	}

	pid = forkpty(&fd, name, pterm, pwin);

	if(Is_block(f) && pid == 0)
	{
		callback(f, Val_int(0));
		exit(0);
	}

	value ret = caml_alloc_tuple(3*sizeof(value));
	Store_field(ret, 0, Val_int(pid));
	Store_field(ret, 1, Val_int(fd));
	Store_field(ret, 2, caml_copy_string(name));
	return ret;
}

value ocaml_forkpty_nocallback(value termp, value winp)
{
	return ocaml_forkpty(Val_int(0), termp, winp);
}

