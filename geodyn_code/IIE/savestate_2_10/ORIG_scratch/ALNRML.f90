!$ALNRML
      SUBROUTINE ALNRML(ECFSAT,AUV,BUV,XUV,R)
!********1*********2*********3*********4*********5*********6*********7**
! ALNRML           89/05/23                      PGMR: JJM, SBL
!
! FUNCTION: CALCULATES 2 VECTORS AUV AND BUV WHICH ARE NORMAL TO THE
!           RADIAL VECTOR AND TO EACH OTHER.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ECFSAT   I    A    CARTESIAN EARTH CENTERED FIXED COORDINATES
!   AUV      O    A    FIRST UNIT VECTOR NORMAL TO ECFSAT
!   BUV      O    A    SECOND UNIT VECTOR NORMAL TO ECFSAT AND AUV
!   XUV      O    A    THE UNIT VECTOR OF ECFSAT
!   R        I    S    SATELLITE - EARTH CENTER DISTANCE
!
!
! COMMENTS:  A GEODYN I ROUTINE MODIFIED FOR GEODYN II
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
          DIMENSION ECFSAT(3),AUV(3),BUV(3),XUV(3)
!
          DATA ONE/1.0D0/,ZERO/0.0D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!
      DO 100 J=1,3
      XUV(J) = ECFSAT(J)/R
  100 END DO
!
!     ....NORTH OR SOUTH POLE PROBLEMS
!
      IF(ABS(XUV(3)).GE. ONE ) GO TO 300
!
!     ....C IS THE COSINE OF THE SATELLITE LATITUDE
!
      C = SQRT(ONE - XUV(3)**2 )
!
!     ....AUV IS THE UNIT VECTOR OF ZHAT CROSS XUV
!
      AUV(1) = -XUV(2)/C
      AUV(2) = XUV(1)/C
      AUV(3) = ZERO
!
!  IF AT THE NORTH POLE POSITION AUV = 0 THUS BMAG = 0
!     ....BUV IS THE UNIT VECTOR OF XUV CROSS AUV
!
      BUV(1) = -XUV(3)*AUV(2)
      BUV(2) = XUV(3)*AUV(1)
      BUV(3) = XUV(1)*AUV(2) - XUV(2)*AUV(1)
! CALCULATE DOT PRODUCT TO GET MAG
      BMAG = BUV(1)**2+BUV(2)**2+BUV(3)**2
      IF(BMAG.LE.ZERO) GO TO 300
      BMAG = SQRT(BMAG)
      DO 200 J=1,3
      BUV(J) = BUV(J)/BMAG
  200 END DO
      GO TO 400
  300 AUV(1) = ONE
      AUV(2) = ZERO
      AUV(3) = ZERO
      BUV(1) = ZERO
      BUV(2) = ONE
      BUV(3) = ZERO
  400 CONTINUE
      RETURN
      END
