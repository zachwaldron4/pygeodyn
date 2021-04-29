!$PLTPRT
      SUBROUTINE PLTPRT(ISTANO,STNAME,SXYZ,PLTINF,PLTVEL,MJDPLT)
!********1*********2*********3*********4*********5*********6*********7**
! PLTPRT           83/10/15            8311.0    PGMR - D. ROLANDS
!
! FUNCTION:  PRINT OUT STATION COORDINATES AT VARIOUS TIMES
!            TO SHOW DRIFT DUE TO PLATE MOTION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ISTANO   I    A    EXTERNAL STATION NUMBER
!   STNAME   I    A    STAION NAME
!   PLTINF   I    A    LAT,LON OF EACH PLATE POLE AND SPIN RATE
!   PLTVEL   I    A    STATION VELOCITY VECTOR DUE TO PLATE MOTION
!   MJDPLT   I    S    REFERENCE DATE FOR PLATE MOTION IN MODIFIED
!                      JULIAN SECONDS
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION SXYZ(3,1),ISTANO(1),STNAME(1),PLTINF(3,1),PLTVEL(3,1)
      DIMENSION MJDPLT(1),FSEC0(1),IYMD(1),IHM(1),SEC(1)
! /CEARTH/ GEOMETRICAL EARTH CONSTANTS USED FOR MEAS. PROCESSING
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/PLATE /NPLATE,MJDPLR,NPLTMS,NXPLAT
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
      DATA ZERO /0.D0/,ONE/1.D0/,EPS/.1D0/,FSEC0/0.D0/
      DATA T6/1000000.D0/
!
!***********************************************************************
! START OF AXECUTABLE CODE *********************************************
!***********************************************************************
!
      ILINE6=MLINE6
      ISTACT=0
   10 CONTINUE
      ISTACT=ISTACT+1
      IF(ISTACT.GT.NSTA) RETURN
      JLINE6=ILINE6+1
      IF(JLINE6.LE.MLINE6) GO TO 400
      IPAGE6=IPAGE6+1
      JLINE6=6
      WRITE(IOUT6,10000)
      WRITE(IUNT88,10000)
      WRITE(IOUT6,10200)
  400 CONTINUE
      ILINE6=JLINE6
      WRITE(IOUT6,10300)
      DO 1000 I=1,NPLTMS
      JLINE6=ILINE6+1
      IF(JLINE6.LE.MLINE6) GO TO 500
      IPAGE6=IPAGE6+1
      JLINE6=6
      WRITE(IOUT6,10000)
      WRITE(IUNT88,10000)
      WRITE(IOUT6,10200)
  500 CONTINUE
      ILINE6=JLINE6
      CALL YMDHMS(MJDPLT(I),FSEC0,IYMD,IHM,SEC,1)
      TM=SEC(1)+DBLE(100*IHM(1))+T6*DBLE(IYMD(1))
      X=SXYZ(1,ISTACT)+(MJDPLT(I)-MJDPLR)*PLTVEL(1,ISTACT)
      Y=SXYZ(2,ISTACT)+(MJDPLT(I)-MJDPLR)*PLTVEL(2,ISTACT)
      Z=SXYZ(3,ISTACT)+(MJDPLT(I)-MJDPLR)*PLTVEL(3,ISTACT)
      WRITE(IOUT6,10400) TM,STNAME(ISTACT),ISTANO(ISTACT),X,Y,Z
 1000 END DO
      GO TO 10
10000 FORMAT('1',32X,'TRACKING COMPLEMENT MODIFIED FOR PLATE MOTION ')
10200 FORMAT(' ',' TIME ',12X,'STATION NAME      NUMBER ',              &
     & 8X,'  X           Y              Z ')
10300 FORMAT(' ')
10400 FORMAT(' ',F14.1,5X,A10,3X,I10,3F14.3)
      END
