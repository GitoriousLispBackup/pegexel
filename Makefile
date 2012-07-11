DESTDIR := /usr/local
SHAREDIR := ${DESTDIR}/share/pegexel
BINDIR := ${DESTDIR}/bin
LIBDIR := ${SHAREDIR}/lib
HOOKDIR := ${SHAREDIR}/hooks
GRAMMARDIR := ${SHAREDIR}/grammars

LISP := /usr/bin/sbcl

CMD_SHEBANG := case ${LISP} in \
*"sbcl") echo  "\#\!${LISP} --script";;\
*"clisp") echo "\#\!${LISP} -q -q";;\
esac

SHEBANG := $(shell $(CMD_SHEBANG))

all: shebang

shebang:
	@echo "${SHEBANG}"
 