!$COEF
      SUBROUTINE COEF(S,IORDER,CIP,CIV)
!********1*********2*********3*********4*********5*********6*********7**
! COEF             82/08/27            8208.0    PGMR - TOM MARTIN
!
! FUNCTION:  COMPUTE THE COWELL INTERPOLATION COEFFICIENTS FOR
!            A SINGLE TIME
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   S        I    S    INTERPOLATION FRACTION
!   IORDER   I    S    ORDER OF COWELL INTEGRATION
!   CIP      O    A    COEFFICIENTS FOR INTERPOLATING POSITION
!   CIV      O    A    COEFFICIENTS FOR INTERPOLATING VELOCITY
!
! COMMENTS:
!
! RESTRICTIONS:  REQUIRES THAT SUBROUTINE COEFIN HAVE BEEN CALLED
!                ONE TIME TO INITIALIZE COMMON/CINTRP/
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION CIP(IORDER),CIV(IORDER)
      COMMON/CINTRP/COMB(21,21),BPZ(21),BPPZ(21)
      DATA ZERO/0.0D0/,ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 I=1,IORDER
      CIP(I)=BPPZ(I)
      CIV(I)=BPZ(I)
 1000 END DO
      B=ONE
      DO 4000 I=2,IORDER
      B=B*(S+(I-2))/(I-1)
      DO 4000 K=I,IORDER
      K1=K-I+1
      CIV(K)=CIV(K)+BPZ(K1)*B
      CIP(K)=CIP(K)+BPPZ(K1)*B
 4000 CONTINUE
      DO 7000 I=3,IORDER
      B=ZERO
      DO 6000 K=I,IORDER
      B=B+COMB(K-2,I-2)*CIP(K)
 6000 END DO
      CIP(I-2)=B
 7000 END DO
      DO 10000 I=2,IORDER
      B=ZERO
      DO 9000 K=I,IORDER
      B=B+COMB(K-1,I-1)*CIV(K)
 9000 END DO
      CIV(I-1)=B
10000 END DO
      RETURN
      END
