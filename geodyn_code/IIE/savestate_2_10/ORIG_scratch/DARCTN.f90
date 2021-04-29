!$DARCTN
      SUBROUTINE DARCTN(Y,X,RESULT,NDIM)
!********1*********2*********3*********4*********5*********6*********7**
! DARCTN           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   Y
!   X
!   RESULT
!   NDIM
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      DIMENSION Y(NDIM),X(NDIM),RESULT(NDIM)
      DATA ZERO/0.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 N=1,NDIM
      RESULT(N)=ATAN2(Y(N),X(N))
 1000 END DO
      DO 2000 N=1,NDIM
      IF(RESULT(N).LT.ZERO) RESULT(N)=RESULT(N)+TWOPI
 2000 END DO
      RETURN
      END
