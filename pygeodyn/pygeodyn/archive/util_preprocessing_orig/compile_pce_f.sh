#
#### Script that Compiles the pce_converter program
#  Written by Zach Waldron with help of D.Rowlands 
#             April 2021
#


gfortran -std=legacy -fdefault-integer-8 pce_converter.f 2>err_compile2 -o ExecutePCE.exe 