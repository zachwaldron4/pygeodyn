




gfortran -c -O2 alt2gph.F90 msis_constants.F90 msis_init.F90 msis_gfn.F90 msis_tfn.F90 msis_dfn.F90 msis_calc.F90 msis_gtd8d.F90>err3 2> err4

gfortran *.o -o msis2_speedtest -llapack -lblas 
