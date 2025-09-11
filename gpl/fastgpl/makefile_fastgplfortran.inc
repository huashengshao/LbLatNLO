LIBRARY= libfastgplfortran.$(libext)
LIBDIR= ../../lib/

include make_opts

FASTGPLPATH =
CXXFLAGS=-O2
INCLUDES += -I${FASTGPLPATH}/include/
CXXFLAGS += ${INCLUDES}

OBJ= fastgplfortran.o

all: $(LIBDIR)$(LIBRARY)

$(LIBDIR)$(LIBRARY): $(OBJ)
	@$(call CREATELIB, $@, $^)
	rm *.o

clean_fastgplfortran:
	rm -vf $(LIBDIR)$(LIBRARY) *~

clean:
	rm -vf *.o $(LIBDIR)$(LIBRARY) *~

end

