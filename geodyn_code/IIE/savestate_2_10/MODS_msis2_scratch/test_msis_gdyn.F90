program msistest

  use msis_init, only          : msisinit

  implicit none

  !Initialize model
  call msisinit(parmpath='',parmfile='msis20.parm')

  call gtd8d(IYYDDD,UTSEC,ALTKM,GLAT,GLON,STLOC,AVGFLX,FLUX,AP,     &
              &          48,DEN,TEMP)

  stop
end program msistest
