program print_compiler_info
  ! Some compiler options:
  ! -fcheck=all - enables all runtime checks, such as exceeding array bounds
  ! -g          - compiles the program with additional instructions that allow it to be run by a debugger
  ! -O0         - disables any optimizations by setting the optimization level to zero
  ! -fbacktrace - cause the program to print a useful traceback in case of runtime failure

  ! Fortran Builtin Modules
  ! iso_fortran_env
  ! iso_c_binding
  ! ieee_arithmetic
  ! ieee_exceptions
  ! ieee_features

  use iso_fortran_env
  implicit none

  print *, 'Compiler version: ', compiler_version()
  print *, 'Compiler options: ', compiler_options()

end program print_compiler_info
