!$FIXTIM
      SUBROUTINE FIXTIM(  ET, MJDSK, XTIM )
!********1*********2*********3*********4*********5*********6*********7**
! FIXTIM           89/04/24            0000.0    PGMR - JJM
!
! FUNCTION:  THIS SUBROUTINE CONVERTS ET FROM JED
!            TO MJD SECONDS AND FRACTIONS OF A SECOND
!            (MJDSK AND XTIM )
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ET       I    S    EPHEMERIS TIME IN JED
!   MJDSK    O    S    MODIFIED JULIAN DAY IN SECONDS PAST GEODYN REF.TI
!   XTIM     O    S    FRACTION OF SECONDS
!
! COMMENTS:
!
! REFERENCES:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
      XMJD = ET - TMGDN1
      MJD  = INT(XMJD)
      XTIM = ( XMJD - MJD ) * 86400.D0
      MJDSK = MJD * 86400
!
!
      RETURN
      END
