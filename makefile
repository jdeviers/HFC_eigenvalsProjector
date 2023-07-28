PROG = $(wildcard prog_*.f90)
MODS = $(wildcard mod_*.f90)
OBJS = $(patsubst %.f90,%.o,$(MODS))

FC = gfortran
FCFLAGS = -fbacktrace -Wall -Wno-tabs
LPFLAGS = -llapack
DEBUGFLAGS = -Og -g -Wall -Wextra -Wno-tabs -fcheck=all -fbacktrace
PROGRAM = project.x


default: $(PROGRAM)

$(PROGRAM) : $(PROG) $(OBJS)
	$(FC) $(FCFLAGS) -o $@ $^ $(LPFLAGS)
#	$(FC) $(DEBUGFLAGS) -o $@ $^ $(LPFLAGS)

$(OBJS) : %.o : %.f90
	$(FC) $(FCFLAGS) -c $< $(LPFLAGS)
#	$(FC) $(DEBUGFLAGS) -c $< $(LPFLAGS)

mod_proc.o : mod_prec.o
mod_diag.o : mod_prec.o mod_proc.o


debug:
	@echo "PROG = $(PROG)"
	@echo "MODS = $(MODS)"
	@echo "OBJS = $(OBJS)"
	@echo "PROGRAM = $(PROGRAM)"


clean:
	rm -rf $(PROGRAM) $(OBJS) $(patsubst %.o,%.mod,$(OBJS))

.PHONY: default debug clean

