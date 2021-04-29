!$ADDTIM
      SUBROUTINE ADDTIM(TNEW,TOLD,TADD,SIGNA,NM)
!********1*********2*********3*********4*********5*********6*********7**
! ADDTIM           88/04/88            8804.0    PGMR - TVM
!
!
! FUNCTION: ADD INCREMENTAL TIME TO AN EXISTING TIME.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   TNEW               RESULT OF THE TIME ADDITION.
!   TOLD               STARTING ARRAY OF TIMES.
!   TADD               ARRAY OF TIMES TO BE ADDED.
!   SIGNA              SENSE OF THE ARITHMETIC.
!   NM                 NUMBER OF MEASUREMENTS FOR WHICH TIMES
!                      ARE TO BE ADDED
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION TNEW(NM),TOLD(NM),TADD(NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 N=1,NM
      TNEW(N)=TOLD(N)+SIGNA*TADD(N)
 1000 END DO
      RETURN
      END
