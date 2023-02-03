#
#### Script that Compiles the pce_converter program
#  Written by Zach Waldron with help of D.Rowlands 
#             April 2021
#
#####   -c flag   : Compile only, do not procude executable
#####   -o flag   : Specify name of object, library, or executable file to write (-o filename)
#####   -O2 flag  : Enables basic block level optimizations.
#####   -pg       : allows linking for the GPROF profiler
#####
#####  -std=legacy 
###             Specify the standard to which the program is expected to conform, 
###             which may be one of ‘f95’, ‘f2003’, ‘f2008’, ‘f2018’, ‘gnu’, or ‘legacy’.
###             The default value for std is ‘gnu’, which specifies a superset of the latest Fortran
###             standard that includes all of the extensions supported by GNU Fortran, although 
###             warnings will be given for obsolete extensions not recommended for use in new code.
###             The ‘legacy’ value is equivalent but without the warnings for 
###             obsolete extensions, and may be useful for old non-standard programs.
##### -fdefault-integer-8 
###             Set the default integer and logical types to an 8 byte wide type.
###             This option also affects the kind of integer constants like 42. 
###             Unlike -finteger-4-integer-8, it does not promote variables with explicit kind declaration.
#####  -fcray-pointer 
###             Enable the Cray pointer extension, which provides C-like pointer functionality.
#####  -llapack and -lblas
###             LAPACK (Linear Algebra PACKage) is a FORTRAN program system for solving linear equations for
###             matrices which fit entirely in core. Links to LAPACK and its associated BLAS (Basic Linear Algebra Subroutines)  



## Must be compiled with fortran 90 to call environment variables
gfortran -std=legacy -fdefault-integer-8 pce_converter.f90 2>err_compile2 -o ExecutePCE.exe 