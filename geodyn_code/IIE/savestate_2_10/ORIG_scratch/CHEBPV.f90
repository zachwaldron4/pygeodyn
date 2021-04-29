!$CHEBPV
      SUBROUTINE CHEBPV(CHB,CHBV,DT,IOR,NM,TWOT,LVEL)
!********1*********2*********3*********4*********5*********6*********7**
! CHEBPV           83/07/29            8308.0    PGMR - A. MARSHALL
!                                      8901      PGMR - A. MARSHALL
!
! FUNCTION:  CALCULATE CHEBYCHEV POLYNOMIALS AND (IF REQUESTED) THEIR
!            DERIVATIVES IN VECTORIZED FORM
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   CHB      O    A    POLYNOMIALS
!   CHBV     O    A    DERIVATIVES OF POLYNOMIALS
!   DT       I    A    VALUE IN INTERVAL  -1,1  AT WHICH POLYNOMIALS
!                      ARE TO BE EVALUATED.
!   IOR      I    S    HIGHEST DEGREE POLYNOMIAL TO BE EVALUATED
!   NM       I    S    NUMBER OF MEASUREMENTS IN BLOCK
!   TWOT     I    A
!   LVEL     I    L    .TRUE. IF DERIVATIVES ARE REQUESTED
!
! COMMENTS:
!
! RESTRICTIONS:  CALLING ROUTINE MUST INITIALIZE CHB(1)&CHBV(2)
!                TO ONE.CHBV(1) MUST BE INTIALIZED TO ZERO
!                IF DERIVATIVES REQUESTED.
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION CHB(NM,IOR),CHBV(NM,IOR),DT(NM),TWOT(NM)
      DATA TWO/2.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 100 I=1,NM
      CHB(I,2)=DT(I)
      TWOT(I)=DT(I)+DT(I)
  100 END DO
      DO 200 I=3,IOR
      DO 200 J=1,NM
      CHB(J,I)=TWOT(J)*CHB(J,I-1)-CHB(J,I-2)
  200 CONTINUE
      IF(.NOT.LVEL) RETURN
      DO 300 I=1,NM
      CHBV(I,3)=TWOT(I)+TWOT(I)
  300 END DO
      DO 400 I=4,IOR
      DO 400 J=1,NM
      CHBV(J,I)=TWOT(J)*CHBV(J,I-1)-CHBV(J,I-2)+TWO*CHB(J,I-1)
  400 CONTINUE
      RETURN
      END
