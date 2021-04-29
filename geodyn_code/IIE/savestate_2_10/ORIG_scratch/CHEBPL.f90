!$CHEBPL
      SUBROUTINE CHEBPL(CHB,CHBV,DT,NM,LVEL)
!********1*********2*********3*********4*********5*********6*********7**
! CHEBPL           83/07/29            8308.0    PGMR - D. ROWLANDS
!                                                COPIED FROM GEODYN
!
! FUNCTION:  CALCULATE CHEBYCHEV POLYNOMIALS AND (IF REQUESTED) THEIR
!            DERIVATIVES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   CHB      O    A    POLYNOMIALS
!   CHBV     O    A    DERIVATIVES OF POLYNOMIALS
!   DT       I    A    VALUE IN INTERVAL  -1,1  AT WHICH POLYNOMIALS
!                      ARE TO BE EVALUATED.
!   NM       I    S    HIGHEST DEGREE POLYNOMIAL TO BE EVALUATED
!   LVEL     I    L    .TRUE IF DERIVATIVES ARE REQUESTED
!
! COMMENTS:
!
! RESTRICTIONS:  CALLING ROUTINE MUST INITIALIZE CHB(1)&CHBV(2)
!                TO ONE.CHBV(2) MUST BE INTIALIZED TO ZERO
!                IF DERIVATIVES REQUESTED.
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION CHB(25),CHBV(25)
      DATA TWO/2.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      TWOT=DT+DT
      CHB(2)=DT
      DO 100 I=3,NM
      CHB(I)=TWOT*CHB(I-1)-CHB(I-2)
  100 END DO
      IF(.NOT.LVEL) RETURN
      CHBV(3)=TWOT+TWOT
      DO 200 I=4,NM
      CHBV(I)=TWOT*CHBV(I-1)-CHBV(I-2)+TWO*CHB(I-1)
  200 END DO
      RETURN
      END
