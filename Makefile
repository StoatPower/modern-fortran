# Example Makefile for a Fortran program using OpenCoarrays

# Compiler and flags
FC = caf
FFLAGS =

# Source file and executable
SRC = hello.f90


# Targets and rules
all: hello

hello: $(SRC)
	$(FC) $(FFLAGS) -o $@ $<

clean:
	rm -f hello

