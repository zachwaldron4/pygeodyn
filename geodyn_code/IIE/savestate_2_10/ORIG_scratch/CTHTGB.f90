!$CTHETG
!********1*********2*********3*********4*********5*********6*********7**
! CTHETG           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  BLOCK DATA ROUTINE FOR COMMON BLOCK/CTHETG/
!            DJ1900 = MJD OF 1900.0
!            TG1900 = THETAG AT 1900.0
!            TDOT00 = ROT RATE (DEGREES/100 DAYS)
!            TGDDOT = THETAG DOT
!            DCENT = 1 JULIAN CENTURY IN DAYS
!            DAYEAR = # DAYS PER YEAR
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      BLOCK DATA CTHTGB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CTHETG/DJ1900,TG1900,TDOT00,TGDDOT, DCENT,DAYEAR
      DATA DJ1900/15019.5D0/,TG1900/99.69098333333333D0/,               &
     &     TDOT00/36000.768925D0/,TGDDOT/3.870833333333333D-4/
      DATA DCENT/36525.0D0/,DAYEAR/365.25D0/
      END
