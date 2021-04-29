!$XEPINT
      SUBROUTINE XEPINT (H,TI,X,FX)
!********1*********2*********3*********4*********5*********6*********7**
! XEPINT           04/10/92                      PGMR -
!
! FUNCTION   PERFORM HERMITIAN 10 ORDER INTERPOLATION
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   H        I    S    STEP SIZE BETWEEN EPHEMERIS POINTS
!   TI       I    S    INTERPOLATION INTERVAL (T-T0)
!   X        I    A    ARRAY CONTAINING SET OF 10 POSITION AND VELOCITY
!                      POINTS TO BE USED FOR THE INTERPOLATION. THE
!                      TIME OF THE FIRST POINT IS CONSIDERED AS T0
!   FX       O    A    ARRAY CONTAINING THE INTERPOLATED POSITION AND
!                      VELOCITY FOR THE TIME SPECIFIED BY TI
! COMMENTS:
!
!           HERMITE INTERPOLATION(POS & VEL) USED FOR SATELLITE POSITION
!           LAGRANGE POLYNOMIAL INTERPOLATION FOR SATELLITE VELOCITY
!
! DOCUMENTATION:
!
! The Hermite interpolation formula uses osculating
! polynomials of contact order 10. They have the properties:
!       X(tj)  = P(tj)
!       X'(tj) = P'(tj)
!  where
!       X and X' are the satellite position and velocity at time=tj,
!       and P and P' are the corresponding polynomial values.
!
!  the Hermite formula for satellite position:
!
!               10
!       P(tj) = SUM ( Hj * X(tj)  +  H'j * X'(tj) )
!               j=1
!   where
!
!       Hj    = [1 - 2(s-j) (1 + (1/(j-1)) ] * [ Lj (tj)]**2
!                                (0 if j=1)
!       H'j   = h (s - j)                    * [ Lj (tj)]**2
!    and
!       h     = step size between two consecutive X points(60 seconds)
!       s     = (t-t0) /h
!       t     = time of interest
!       t0    = start time of 10 minute interval
!               X points are continuously input so that (t-t0) is kept
!               to around the midpoint (241 - 300 seconds) of the 10
!               minute interval (s1=0,...s10=9)
!       Lj(tj)= Lagrange polynomial for X point time=tj such that:
!                                 10
!             = (-1)**(1-j) (s-0) PRODUCT [s - k - 1] / [ j! (1-j)! ]
!                                 k=1 (k.ne.j)
!
!
!  The Lagrange interpolation formula for satellite velocity (time=tj):
!
!               10
!       V(tj) = SUM ( Lj(tj) * X'(tj) )
!               j=1
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      DIMENSION X(10,6),FX(6),A(20),SJMI(10)
      DATA M/10/
!  NF9= 9 FACTORAL
      DATA NF9/362880.D0/
      DATA C0/0.0D0/,C1/1.0D0/,C2/2.0D0/,C3/3.0D0/,C4/4.0D0/
      DATA C5/5.0D0/,C6/6.0D0/,C7/7.0D0/,C8/8.0D0/,C9/9.0D0/
!
! SJMI CONTAINS THE 10 VALUES FOR J= 0 - 9 IN THE EVALUATION OF THE
! EXPRESSION SJMI=1/(J-I) FOR I=0-9, I.NE.J
!
      DATA SJMI/-2.828968253968254D0,-1.717857142857143D0,              &
     &          -1.092857142857143D0,-0.616666666666666D0,              &
     &          -0.199999999999999D0, 0.199999999999999D0,              &
     &           0.616666666666666D0, 1.092857142857143D0,              &
     &           1.717857142857143D0, 2.828968253968254D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!  zero out the interpolated output array
      DO 300 I=1,6
      FX(I)=C0
  300 END DO
!
      XLS=C1
      FN=M
      S=TI/H
      IS=S
      IF(IS.EQ.S) GO TO 6000
!
      XLS=(S   )*(S-C1)*(S-C2)*(S-C3)*(S-C4)*(S-C5)*(S-C6)*(S-C7)*      &
     &    (S-C8)*(S-C9)
!
      A(1)=-XLS/(NF9*S)
      DO 500 I=2,M
         FIM1=I-1
         SMI1=S-FIM1
!        IF(SMI1.EQ.0.0D0) GO TO 6000
         SMI2=SMI1+1.0D0
         FNI2=FN-FIM1
!        A(I)=-A(I-1)*(SMI2/SMI1)*(FNI2/FIM1)
         A(I)=-A(I-1)*((SMI2*FNI2)/(SMI1*FIM1))
  500 END DO
!
      J=0
      FJ=0.0D0
 1000 ILN=J+1
      SMJ=S-FJ
      COFF=A(J+1)**2
      HX=COFF-2.D0*SMJ*SJMI(J+1)*COFF
      HXB=H*SMJ*COFF
      DO 3000 II=1,3
         FX(II)  =FX(II)   + HX*X(ILN,II) + HXB*X(ILN,II+3)
         FX(II+3)=FX(II+3) + A(J+1)*X(ILN,II+3)
 3000 END DO
      J=J+1
      FJ=J
      IF(J .LT. M) GO TO 1000
!
      RETURN
 6000 CONTINUE
      ILN=IS+1
!
         DO 6500 I=1,3
            FX(I)=X(ILN,I)
            FX(I+3)=X(ILN,I+3)
 6500 END DO
!
      RETURN
      END
