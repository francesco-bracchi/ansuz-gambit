GSC		=	gsc
GSI		=	gsi
LIBNAME		= 	ansuz
SRCDIR		= 	src
LIBDIR		= 	lib
TESTDIR		= 	test
DIR		=	ls

INSTALLDIR	= 	$(shell ${GSI} -e "(display (path-expand \"~~${LIBNAME}\"))")
SOURCES		=	$(shell ls ${SRCDIR}/*[a-zA-Z0-9].scm)
INCLUDES	=	${SRCDIR}/*.scm ${SRCDIR}/re ${SRCDIR}/sources
OBJECTS		=	$(SOURCES:.scm=.o1)
MAKE		=	make
INSTALL		= 	cp

all: libdir

clean: 
	-rm ${SRCDIR}/*.o1 
	-rm -r ${LIBDIR}
	-rm ${TESTDIR}/*.o1

libdir: compile $(LIBDIR)
	cp $(OBJECTS) $(LIBDIR)
	cp -r $(INCLUDES) $(LIBDIR)

compile: $(OBJECTS)

$(LIBDIR):
	-mkdir $(LIBDIR)

%.o1 : %.scm
	$(GSC) -o $@ $<

$(INSTALLDIR): 
	-mkdir $(INSTALLDIR)

install: libdir $(INSTALLDIR) 
	@echo "installing in:"
	@echo $(INSTALLDIR)
	cp -r $(LIBDIR)/* $(INSTALLDIR)


$(TESTDIR)/calc.o1:
	$(GSC) -:~~$(LIBNAME)=$(LIBDIR) $(TESTDIR)/calc

calc: libdir $(TESTDIR)/calc.o1
	$(GSI) -:~~$(LIBNAME)=$(LIBDIR) -e "(load \"~~${LIBNAME}/expressions\")" $(TESTDIR)/calc
