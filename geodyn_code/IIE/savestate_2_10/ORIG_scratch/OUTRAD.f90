!**********************************************************************
!$OUTRAD
      SUBROUTINE OUTRAD(RAD,IDEG,IMIN,SEC,LLONG)
!********1*********2*********3*********4*********5*********6*********7**
! OUTRAD           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  CONVERTS RADIANS INTO DEGREES, MINUTES, SECONDS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   RAD      I    S    RADIANS TO BE CONVERTED IN DDMMSS
!   IDEG     O    S    DEGREES
!   IMIN     O    S    MINUTES
!   SEC      O    S    SECONDS
!   LLONG    I    S    .FALSE. FOR LATITUDE CALCULATION
!                      .TRUE. FOR LONGITUDE CALCULATION
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      DATA ZERO/0.0D0/,TWO/2.0D0/,SIXTY/60.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      ARAD=ABS(RAD)
      IF(LLONG) ARAD=RAD+TWO*TWOPI
      ARAD=MOD(ARAD,TWOPI)
      DEG=ARAD/DEGRAD
      IDEG=INT(DEG)
      XMIN=(DEG-DBLE(IDEG))*SIXTY
      IMIN=INT(XMIN)
      SEC=(XMIN-DBLE(IMIN))*SIXTY
      IF(RAD.GT.ZERO.OR.LLONG) RETURN
      IDEG=-IDEG
      IMIN=-IMIN
      SEC=-SEC
      RETURN
      END
