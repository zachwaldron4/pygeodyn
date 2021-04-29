!$RADOUT
      SUBROUTINE RADOUT(DMSEC,RAD,K)
!********1*********2*********3*********4*********5*********6*********7**
! RADOUT           03/20/84            0000.0    PGMR - BILL EDDY
!                  03/25/91            0000.0    PGMR - J. MCCARTHY
!
! FUNCTION:  CONVERT DEG/MIN/SEC OR HOUR/MIN/SEC TO RADIANS
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   DMSEC    I         DEG/MIN/SEC (DDDMMSS.SSS) OR HOUR/MIN/SEC
!                      (HHMMSS.SSS)
!   RAD      O         RADIANS
!   K        I         FLAG CONTROLLING INPUT AS FOLLOWS
!                        = 1  DEG /MIN/SEC
!                        = 2  HOUR/MIN/SEC
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
!
      DATA C60D0/60.D0/,C15D0/15.D0/,C36D2/3600.D0/
      DATA ROUND/.5D-12/
!
!**********************************************************************
! START OF EXECUTABLE CODE
!**********************************************************************
!
!
      DSGN  = SIGN(1.D0, DMSEC)
      DMSABS= ABS(DMSEC)
      IDEG  =(DMSABS+ROUND)/10000
! IF INPUT IS IN HOURS CONVERT TO DEGREES
      IMIN  =(DMSABS-IDEG*10000)/100
      SEC   =DMSABS-IDEG*10000-IMIN*100
      RAD   =(DBLE(IDEG)+DBLE(IMIN)/C60D0+SEC/C36D2)*DEGRAD
      RAD   =DSGN*RAD
      IF(K.EQ.2) RAD =RAD *C15D0
      RETURN
      END
