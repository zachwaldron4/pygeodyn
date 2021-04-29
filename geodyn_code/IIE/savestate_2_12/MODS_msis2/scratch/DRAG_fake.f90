program msistest

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE





!      integer(8)   
!      integer                     :: iyd, mass
!      real(4)                     :: sec, alt, glat, glong, stl, f107a,f107, ap(7), apd
!      real(4)                     :: d(9),t(2)


      DAY=256.00074287 
      IYR=3
      ALTI=1063083.0736091621
      PHI=0.85725231
      XLAMB=-1.6954969513
      COSHLN=0.10657605
      SINHLN=0.99430455
      FLUXIN=94.82722793
      FLXAVG= 122.74379089
      FLUXM=3.67000000
      IMARK=153
      IKPAP=1
      I324=3
      IDRV=1
      RHO=0.00000000
      DRHODZ=0.00000000
      IERR=0

!*  PRINTOUT FOR TESTING.
         PRINT 801, DAY,IYR,ALTI,PHI,COSHLN,SINHLN,FLUXIN,FLXAVG,       &
                & FLUXM,IMARK,IKPAP,I324,IDRV,RHO,DRHODZ,IERR
!
  801 FORMAT(1X,  '    ',/,                                             &
              1X, '     INPUT to MSIS.f90 from DRAG.f90:  ',/,          &
              1X, '     DAY:     ' ,F12.8    ,/,                        &
              1X, '     IYR:     ' ,I8      ,/,                         &
              1X, '     ALTI:    ' ,F20.10   ,/,                        &
              1X, '     PHI:     ' ,F12.8    ,/,                        &
              1X, '     COSHLN:  ' ,F12.8   ,/,                         &
              1X, '     SINHLN:  ' ,F12.8    ,/,                        &
              1X, '     FLUXIN:  ' ,F12.8    ,/,                        &
              1X, '     FLUXAVG: ' ,F12.8   ,/,                         &
              1X, '     FLUXM:   ' ,F12.8   ,/,                         &
              1X, '     IMARK:   ' ,I8      ,/,                         &
              1X, '     IKPAP:   ' ,I8   ,/,                            &
              1X, '     I324:    ' ,I8   ,/,                            &
              1X, '     IDRV:    ' ,I8   ,/,                            &
              1X, '     RHO:     ' ,F12.8   ,/,                         &
              1X, '     DRHODZ:  ' ,F12.8   ,/,                         &
              1X, '     IERR     ' ,I8   ,/,                            &
              1X, '     ' )

         CALL MSIS(DAY,IYR,ALTI,PHI,XLAMB,COSHLN,SINHLN,FLUXIN,FLXAVG,  &
     &          FLUXM,IMARK,IKPAP,I324,IDRV,RHO,DRHODZ,IERR)
end program msistest     
