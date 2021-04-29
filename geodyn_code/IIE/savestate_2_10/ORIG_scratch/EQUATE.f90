!$EQUATE
      SUBROUTINE EQUATE(TNEW,TOLD,NM)
!********1*********2*********3*********4*********5*********6*********7**
! EQUATE           00/00/00            8804.0    PGMR - TVM
!
! FUNCTION: COPY ONE ARRAY TO ANOTHER.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   TNEW     O    A    NEW ARRAY.
!   TOLD     I    A    OLD ARRAY.
!   NM       I    S    NUMBER OF ELEMENTS IN ARRAY.
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION TNEW(NM),TOLD(NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 N=1,NM
      TNEW(N)=TOLD(N)
 1000 END DO
      RETURN
      END
