program msistest

  use msis_init, only          : msisinit

  implicit none

  integer, parameter          :: nrec = 321908

  integer                     :: iyd, mass
  real(4)                     :: sec, alt, glat, glong, stl, f107a, f107, ap(7), apd
  real(4)                     :: d(9),t(2)

  integer                     :: i
  character(128)              :: dummy

  !Initialize model
  call msisinit(parmpath='/data/geodyn_proj/analysis/benchmark_speed_tests/',parmfile='msis20.parm')

  !Open input and output files, loop through records, and call model
  open(77, file='/data/data_geodyn/results/st/msis2/msis2_acceloff/DENSITY/st030914_2wk.goco05s_msisin',status='old')
! 
  open(78,file='msis2.0_test_out.txt',status='replace')
  read(77,*) dummy
  write(78,'(9a7,9a13,a8)') &
    'iyd','sec','alt','glat','glong','stl','f107a','f107','Ap','He','O','N2','O2','Ar','rho','H','N','O*','T'
 
  do i = 1,200
          read(77,*) iyd,sec,alt,glat,glong,stl,f107a,f107,apd
    ap(1) = apd
    call gtd8d(iyd,sec,alt,glat,glong,stl,f107a,f107,ap,mass,d,t)
    write(78,'(2i7,3f7.1,f7.2,3f7.1,9e13.4,f8.2)')  &
      iyd,int(sec),alt,glat,glong,stl,f107a,f107,ap(1),d(1:9),t(2)
  enddo
  close(77)
  close(78)

  stop

end program msistest
