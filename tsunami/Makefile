# Example Makefile for a Fortran program using OpenCoarrays

# Compiler and flags
FC = caf
FFLAGS =

# Source file and executable
SRC = tsunami.f90


# Targets and rules
all: tsunami

tsunami: $(SRC)
	$(FC) $(FFLAGS) -o $@ $<

clean:
	rm -f tsunami

