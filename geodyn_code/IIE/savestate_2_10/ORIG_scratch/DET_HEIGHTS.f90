      SUBROUTINE DET_HEIGHTS(LAT_IN,LON_IN,OROGRAPHY_ARRAY, &
     &                       HEIGHT_INTERP)
!*******************************************************************************
! FUNCTION: Determines the mean grid height by interpolation. The mean grid
!           heights are provided in an array that is created by subroutine
!           read_orography.f
!
! I/O PARAMETERS:
!
!
!   NAME              I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------            ---  ---   -----------------------------------------------
!   LAT_IN             I    S    Station latitude in degrees
!   LON_IN             I    S    Station longitude in degrees
!   OROGRAPHY_ARRAY    I    S    Mean Grid height array (91,145)
!   HEIGHT_INTERP      O    S    Interpolated mean grod height
!
!*******************************************************************************
      IMPLICIT NONE

      INTEGER :: LAT_ARRAY,DELTA_LAT,LAT_P,LAT_M
      INTEGER :: N, I, J , K, ORO_COL
      DOUBLE PRECISION :: LAT_PLUS,LAT_MINUS,LON_PLUS,LON_MINUS
      DOUBLE PRECISION :: BOUND_MM, BOUND_PP, BOUND_MP, BOUND_PM
      DOUBLE PRECISION :: DELTA_LON,HEIGHT_INTERP
      DOUBLE PRECISION :: LON_ARRAY,HEADER(6),LON_IN,LAT_IN,LON_MOD
      DOUBLE PRECISION :: DELTA_LAT_INV,DELTA_LON_INV,t,u,t1,u1
      LOGICAL :: L_COMPUTE
      INTEGER :: BUFFER(15,10),RECORD(10),IRET,LONBOUNDLOC(2)
      INTEGER :: LATBOUNDLOC(2)
      INTEGER :: OROGRAPHY_ARRAY(91,145)

      DIMENSION  LAT_ARRAY(91), LON_ARRAY(145)

      DATA LAT_ARRAY/ 90, 88, 86, 84, 82, 80, 78, 76, 74, 72, 70, 68,  &
     &                66, 64, 62, 60, 58, 56, 54, 52, 50, 48, 46, 44,  &
     &                42, 40, 38, 36, 34, 32, 30, 28, 26, 24, 22, 20,  &
     &                18, 16, 14, 12, 10,  8,  6,  4,  2,  0, -2, -4,  &
     &                -6, -8,-10,-12,-14,-16,-18,-20,-22,-24,-26,-28,  &
     &               -30,-32,-34,-36,-38,-40,-42,-44,-46,-48,-50,-52,  &
     &               -54,-56,-58,-60,-62,-64,-66,-68,-70,-72,-74,-76,  &
     &               -78,-80,-82,-84,-86,-88,-90/

      DATA LON_ARRAY/  0.0,  2.5,  5.0,  7.5, 10.0, 12.5, 15.0, 17.5,  &
     &                20.0, 22.5, 25.0, 27.5, 30.0, 32.5, 35.0, 37.5,  &
     &                40.0, 42.5, 45.0, 47.5, 50.0, 52.5, 55.0, 57.5,  &
     &                60.0, 62.5, 65.0, 67.5, 70.0, 72.5, 75.0, 77.5,  &
     &                80.0, 82.5, 85.0, 87.5, 90.0, 92.5, 95.0, 97.5,  &
     &               100.0,102.5,105.0,107.5,110.0,112.5,115.0,117.5,  &
     &               120.0,122.5,125.0,127.5,130.0,132.5,135.0,137.5,  &
     &               140.0,142.5,145.0,147.5,150.0,152.5,155.0,157.5,  &
     &               160.0,162.5,165.0,167.5,170.0,172.5,175.0,177.5,  &
     &               180.0,182.5,185.0,187.5,190.0,192.5,195.0,197.5,  &
     &               200.0,202.5,205.0,207.5,210.0,212.5,215.0,217.5,  &
     &               220.0,222.5,225.0,227.5,230.0,232.5,235.0,237.5,  &
     &               240.0,242.5,245.0,247.5,250.0,252.5,255.0,257.5,  &
     &               260.0,262.5,265.0,267.5,270.0,272.5,275.0,277.5,  &
     &               280.0,282.5,285.0,287.5,290.0,292.5,295.0,297.5,  &
     &               300.0,302.5,305.0,307.5,310.0,312.5,315.0,317.5,  &
     &               320.0,322.5,325.0,327.5,330.0,332.5,335.0,337.5,  &
     &               340.0,342.5,345.0,347.5,350.0,352.5,355.0,357.5,  &
     &               360.0/

      DATA DELTA_LAT/2.0/,DELTA_LON/2.5/,DELTA_LAT_INV/0.5/
      DATA DELTA_LON_INV/0.4/


!*******************************************
! DETERMINE BOUNDING POINTS FOR LATITUDE IN
! AND LONGITUDE IN
!*******************************************
!      LAT_IN = 89.0
!      LON_IN = 26.5

!      write(6,*) "DET_H: ORO_ARRAY is :", OROGRAPHY_ARRAY(91,1:145)

      ! get 4 bounding points for lon, lat

! ( lon_minus, lat_minus )
! ( lon_minus, lat_plus  )
! ( lon_plus , lat_minus )
! ( lon_plus , lat_plus  )

       lon_mod = MOD( lon_in + 360.0D0 , 360.0D0 )

!      write(6,'(/A,2(1x,F8.3 ))')  &
!       'roe: lon_in, lon_mod ', lon_in, lon_mod

      lat_minus = lat_in
      lat_plus  = lat_in  + delta_lat
      lon_minus = lon_mod
      lon_plus  = lon_mod + delta_lon

      lon_minus = DBLE( INT( lon_minus * delta_lon_inv ))&
     & * delta_lon
      lon_plus  = DBLE( INT( lon_plus  * delta_lon_inv ))&
     & * delta_lon


!       write(6,*) "ROE: LON_MINUS,LON_PLUS :", LON_MINUS,LON_PLUS

      if (lat_in >= 0.D0) then

          lat_minus = DBLE(( INT( lat_minus * delta_lat_inv ) * &
     &                delta_lat))

          lat_plus  = DBLE(( INT( lat_plus  * delta_lat_inv ) * &
     &                delta_lat))

!               write(6,*) "roe: lat_minus, lat_plus:", &
!                           lat_minus, lat_plus

      elseif (lat_in < 0.D0) then

          lat_minus = DBLE( (INT( lat_minus * delta_lat_inv ) * &
     &                delta_lat)-2)

          lat_plus  = DBLE(( INT( lat_plus  * delta_lat_inv ) * &
     &                delta_lat)-2)

!               write(6,*) "roe: lat_minus, lat_plus:", &
!                          lat_minus, lat_plus

       else

          lat_minus = DBLE( INT( lat_minus * delta_lat_inv ) * &
     &                 delta_lat)
          lat_plus  = DBLE( INT( lat_plus  * delta_lat_inv ) * &
     &                delta_lat)

       endif

!-----------------------------------------------------

! make sure the numbers are in the proper ranges

      lat_plus  = MIN(  90.0D0, lat_plus  )

      lon_minus = MOD( lon_minus + 360.0D0 , 360.0D0 )
      lon_plus  = MOD( lon_plus  + 360.0D0 , 360.0D0 )

      LAT_P = INT(LAT_PLUS)
      LAT_M = INT(LAT_MINUS)



!      write(6,*) "roe: lat_minus, lat_plus:", &
!                           lat_m, lat_p

!      write(6,*) "ROE: LON_MINUS,LON_PLUS :", LON_MINUS,LON_PLUS



!-----------------------------------------------------
!-----------------------------------------------------
! CALL FNDNUM TO DETERMINE THE LOCATIONS IN THE
! LAT AND LON ARRAYS THE BOUNDS ARE
!-----------------------------------------------------

      CALL FNDNUM(LAT_P,LAT_ARRAY,91,IRET)

      LATBOUNDLOC(1) = IRET

      CALL FNDNUM(LAT_M,LAT_ARRAY,91,IRET)

      LATBOUNDLOC(2) = IRET

      CALL FNDNUM(LON_PLUS,LON_ARRAY,145,IRET)

      LONBOUNDLOC(1) = IRET

      CALL FNDNUM(LON_MINUS,LON_ARRAY,145,IRET)

      LONBOUNDLOC(2) = IRET

!      write(6,*) "ROE: LAT BOUNDS ", LATBOUNDLOC(1), LATBOUNDLOC(2)
!      write(6,*) "ROE: LON BOUNDS ", LONBOUNDLOC(1), LONBOUNDLOC(2)

      BOUND_MM = DBLE(OROGRAPHY_ARRAY(LATBOUNDLOC(2),LONBOUNDLOC(2)))
      BOUND_MP = DBLE(OROGRAPHY_ARRAY(LATBOUNDLOC(2),LONBOUNDLOC(1)))
      BOUND_PM = DBLE(OROGRAPHY_ARRAY(LATBOUNDLOC(1),LONBOUNDLOC(2)))
      BOUND_pp = DBLE(OROGRAPHY_ARRAY(LATBOUNDLOC(1),LONBOUNDLOC(1)))


!      write(6,*) "BOUND HEIGHTS:", BOUND_MM,BOUND_MP,BOUND_PM,BOUND_pp

      CALL VMF_INTERPOLATION(LON_MINUS,LAT_MINUS,LON_PLUS,LAT_PLUS,  &
     &                       BOUND_MM,BOUND_PM,BOUND_PP,BOUND_MP,    &
     &                       lon_in,lat_in,t,u,t1,u1,.TRUE.,         &
     &                       HEIGHT_INTERP)


!       write(6,*) "HEIGHT is :", HEIGHT_INTERP

       HEIGHT_INTERP = HEIGHT_INTERP



       RETURN

       END SUBROUTINE DET_HEIGHTS
