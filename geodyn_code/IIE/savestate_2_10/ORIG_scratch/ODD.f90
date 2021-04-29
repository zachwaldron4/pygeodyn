!$ODD
      SUBROUTINE ODD(INUM,LODD)
! *******1*********2*********3*********4*********5*********6*********7**
!     VERSION 9101.00             DATE 01/15/91  PGMR - DESPINA PAVLIS
!
!     FUNCTION - DETERMINES WHETHER OR NOT AN INTEGER NUMBER IS ODD
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS IN ALPHABETICAL ORDER
!   ------  ---  ---   ------------------------------------------------
!   INUM    I     S    INTEGER INPUT NUMBER
!   LODD    O     S    .TRUE. IF INUM IS ODD
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
!
      LODD=.FALSE.
      IAUX=INUM/2
      IAUX=IAUX*2
      IF(IAUX.NE.INUM) LODD=.TRUE.
!
      RETURN
      END
