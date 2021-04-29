!$REFCRV
      SUBROUTINE REFCRV(RNPPN,RPN,NM)
!********1*********2*********3*********4*********5*********6*********7**
! REFCRV           83/06/29            8306.0    PGMR - D. ROWLANDS
!
!   FUNCTION:  FORM THE MATRIX TO GO FROM TRUE OF REFERENCE TO TRUE OF
!              MEASUREMENT TIME.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   RNPPN    O    A    TRUE OF REFERENCE TO TRUE OF MEASUREMENT
!                      TIME
!   RPN      I    A    TRUE OF REFERENCE TO MEAN 50 MATRIX
!   NM       I    S    NUMBER OF MEASUREMENTS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CRDDIM/NDCRD2,NDCRD3,NDCRD4,NDCRD5,NDCRD6,NDCRD7,          &
     &              NDCRD8,NDCRD9,NXCRDM
      COMMON/CREFMT/REFMT(9)
      DIMENSION RPN(NDCRD2,9),RNPPN(NM,3,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 100 I=1,NM
      RNPPN(I,1,1)=RPN(I,1)*REFMT(1)+RPN(I,4)*REFMT(2)                  &
     &            +RPN(I,7)*REFMT(3)
      RNPPN(I,2,1)=RPN(I,2)*REFMT(1)+RPN(I,5)*REFMT(2)                  &
     &            +RPN(I,8)*REFMT(3)
      RNPPN(I,3,1)=RPN(I,3)*REFMT(1)+RPN(I,6)*REFMT(2)                  &
     &            +RPN(I,9)*REFMT(3)
      RNPPN(I,1,2)=RPN(I,1)*REFMT(4)+RPN(I,4)*REFMT(5)                  &
     &            +RPN(I,7)*REFMT(6)
      RNPPN(I,2,2)=RPN(I,2)*REFMT(4)+RPN(I,5)*REFMT(5)                  &
     &            +RPN(I,8)*REFMT(6)
      RNPPN(I,3,2)=RPN(I,3)*REFMT(4)+RPN(I,6)*REFMT(5)                  &
     &            +RPN(I,9)*REFMT(6)
      RNPPN(I,1,3)=RPN(I,1)*REFMT(7)+RPN(I,4)*REFMT(8)                  &
     &            +RPN(I,7)*REFMT(9)
      RNPPN(I,2,3)=RPN(I,2)*REFMT(7)+RPN(I,5)*REFMT(8)                  &
     &            +RPN(I,8)*REFMT(9)
      RNPPN(I,3,3)=RPN(I,3)*REFMT(7)+RPN(I,6)*REFMT(8)                  &
     &            +RPN(I,9)*REFMT(9)
  100 END DO
      RETURN
      END
