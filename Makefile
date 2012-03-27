GSC		=	gsc
GSI		=	gsi
INSTALLDIR	= 	$(shell ${GSI} -e "(display (path-expand \"~~/ansuz\"))")
SRCDIR		= 	src
LIBDIR		= 	lib
TESTDIR		= 	test
SOURCES		=	${SRCDIR}/expressions.scm ${SRCDIR}/regexp.scm
INCLUDES	=	${SRCDIR}/*.scm ${SRCDIR}/re ${SRCDIR}/sources
OBJECTS		=	$(SOURCES:.scm=.o1)
MAKE		=	make
INSTALL		= 	cp
ANSUZDEF	= 	~~ansuz=${LIBDIR}

all: libdir

clean: 
	rm ${SRCDIR}/*.o1
	rm -r ${LIBDIR}
	rm ${TESTDIR}/*.o1

libdir: compile $(LIBDIR)
	cp $(OBJECTS) $(LIBDIR)
	cp -r $(INCLUDES) $(LIBDIR)

compile: $(OBJECTS)

$(LIBDIR):
	mkdir $(LIBDIR)

%.o1 : %.scm
	$(GSC) -o $@ $<

$(INSTALLDIR): 
	mkdir $(INSTALLDIR)

install: libdir $(INSTALLDIR) 
	@echo "installing in:"
	@echo $(INSTALLDIR)
	cp -r $(LIBDIR)/* $(INSTALLDIR)


$(TESTDIR)/calc.o1:
	$(GSC) $(TESTDIR)/calc

calc: $(TESTDIR)/calc.o1
	$(GSI) -:$(ANSUZDEF) -e "(load \"~~ansuz/expressions\")" $(TESTDIR)/calc
