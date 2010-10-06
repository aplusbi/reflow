#include <stdio.h>
#include <stdlib.h>
#include <pty.h>
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

#include <fcntl.h>

value ocaml_sigwinch(value unit)
{	
	return Val_int(SIGWINCH);
}

value ocaml_forkpty(value unit)
{
	int fd = -1;
	pid_t pid = -1;
	char name[BUFSIZ];

	pid = forkpty(&fd, name, NULL, NULL);

	value ret = caml_alloc_tuple(3*sizeof(value));
	Store_field(ret, 0, Val_int(pid));
	Store_field(ret, 1, Val_int(fd));
	Store_field(ret, 2, caml_copy_string(name));
	return ret;
}

value ocaml_openpty(value unit)
{
	int master = -1;
	int slave = -1;
	int rv = -1;
	char name[BUFSIZ];

	rv = openpty(&master, &slave, name, NULL, NULL);

	value ret = caml_alloc_tuple(4*sizeof(value));
	Store_field(ret, 0, Val_int(rv));
	Store_field(ret, 1, Val_int(master));
	Store_field(ret, 2, Val_int(slave));
	Store_field(ret, 3, caml_copy_string(name));
	return ret;
}

value ocaml_get_winsize(value fd)
{
	CAMLparam1(fd);

	int ifd = Int_val(fd);
	struct winsize ws;
	ioctl(ifd, TIOCGWINSZ, &ws);

	CAMLlocal1(ret);
	ret = caml_alloc(4, 0);
	Store_field(ret, 0, Val_int(ws.ws_row));
	Store_field(ret, 1, Val_int(ws.ws_col));
	Store_field(ret, 2, Val_int(ws.ws_xpixel));
	Store_field(ret, 3, Val_int(ws.ws_ypixel));
	CAMLreturn(ret);
}

value ocaml_set_winsize(value fd, value winp)
{
	CAMLparam2(fd, winp);
	int log = open("log.txt", O_WRONLY | O_CREAT);
	char buff[256];

	int ifd = Int_val(fd);
	sprintf(buff, "fd %d\n", ifd);
	write(log, buff, strlen(buff));
	struct winsize ws;
	ws.ws_row = Int_val(Field(winp, 0));
	ws.ws_col = Int_val(Field(winp, 1));
	ws.ws_xpixel = Int_val(Field(winp, 2));
	ws.ws_ypixel = Int_val(Field(winp, 3));
	sprintf(buff, "ws %d, %d, %d, %d\n", ws.ws_row, ws.ws_col, ws.ws_xpixel, ws.ws_ypixel);
	write(log, buff, strlen(buff));
	ioctl(ifd, TIOCSWINSZ, &ws);

	close(log);

	return Val_int(0);
}

