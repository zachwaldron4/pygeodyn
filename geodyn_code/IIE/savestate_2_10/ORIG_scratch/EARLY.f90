! DUMMY routine until code for E2CSF is obtained
!$EARLY
      SUBROUTINE EARLY(X,N,A,IEARLY)
!********1*********2*********3*********4*********5*********6*********7**
! EARLY            87/07/00            0000.0    PGMR - DESPINA PAVLIS
!
! FUNCTION:  GIVEN AN ARRAY FINDS WHICH ELEMENT HAS THE SMALLEST VALUE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   X        I    A    ARRAY OF VALUES
!   N        I    S    SIZE OF THE ARRAY
!   A        I    S    MAXIMUM VALUE ANY ELEMENT OF THE
!                      ARRAY COULD TAKE
!   IEARLY   O    S    INDEX POINTING TO THE SMALLEST VALUE
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION X(N)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      XEARLY=A
      DO 10 I=1,N
      IF(X(I).GT.XEARLY) GOTO 10
      IEARLY=I
      XEARLY=X(I)
   10 END DO
      RETURN
      END
