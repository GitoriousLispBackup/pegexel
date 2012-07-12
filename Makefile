DESTDIR := /usr/local

INSTALL := install

SHAREDIR := ${DESTDIR}/share/pegexel
BINDIR := ${DESTDIR}/bin
LIBDIR := ${SHAREDIR}/lib
HOOKDIR := ${SHAREDIR}/hooks
GRAMMARDIR := ${SHAREDIR}/grammars
EXAMPLESDIR := ${DESTDIR}/doc/pegexel/examples

LISP := /usr/bin/sbcl

CMD_SHEBANG := case ${LISP} in \
*"sbcl") echo  "\#\!${LISP} --script";;\
*"clisp") echo "\#\!${LISP} -q -q";;\
esac

SHEBANG := $(shell $(CMD_SHEBANG))

SCRIPT := $(shell ls src/bin/*.lsp)
LIBS := $(shell ls  src/lib/*.lsp)
HOOKS :=  $(shell ls src/hooks/*.lsp)
GRAMMARS := $(shell ls src/grammars/*.grammar)
EXAMPLES := $(shell ls examples/*.pgxl)

all:
	echo ${LIBS}
shebang:
	@echo "${SHEBANG}"

install: install-bin install-hooks install-libs install-grammars

install-bin:
	${INSTALL} -d ${BINDIR}
	${INSTALL} -m 755  -t ${BINDIR} ${SCRIPT}

install-hooks:
	${INSTALL} -d ${HOOKDIR}
	${INSTALL} -m 755  -t ${HOOKDIR} ${HOOKS}

install-libs:
	${INSTALL} -d ${LIBDIR}
	${INSTALL} -m 755  -t ${LIBDIR} ${LIBS}

install-grammars:
	${INSTALL} -d ${GRAMMARDIR}
	${INSTALL} -m 755  -t ${GRAMMARDIR} ${GRAMMARS}

install-examples:
	${INSTALL} -d ${EXAMPLESDIR}
	${INSTALL} -m 755  -t ${EXAMPLESDIR} ${EXAMPLES}
