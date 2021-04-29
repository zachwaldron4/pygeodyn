!$COEFV
      SUBROUTINE COEFV(S,B,NM,IORDER,CIPV,CIVV)
!********1*********2*********3*********4*********5*********6*********7**
! COEFV            82/08/27            8208.0    PGMR - TOM MARTIN
!
! FUNCTION:  COMPUTE THE COWELL INTERPOLATION COEFFICIENTS FOR
!            MULTIPLE TIMES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   S        I    A    INTERPOLATION FRACTIONS
!   B        I    A    SCRATCH ARRAY USED IN FORMING VECTOR SETS OF
!                      COEFFICIENTS
!   NM       I    S    NUMBER OF INTERPOLATION TIMES
!   IORDER   I    S    ORDER OF COWELL INTEGRATION
!   CIPV     O    A    COEFFICIENTS FOR INTERPOLATING POSITIONS
!   CIVV     O    A    COEFFICIENTS FOR INTERPOLATING VELOCITIES
!
!
! COMMENTS:
!
! RESTRICTIONS:  REQUIRES THAT SUBROUTINE COEFIN HAVE BEEN CALLED
!                ONE TIME TO INITIALIZE COMMON/CINTRP/
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION S(NM),B(NM),CIPV(NM,IORDER),CIVV(NM,IORDER)
      COMMON/CINTRP/COMB(21,21),BPZ(21),BPPZ(21)
      DATA ZERO/0.0D0/,ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 I=1,IORDER
!
      DO 800 N=1,NM
      CIPV(N,I)=BPPZ(I)
      CIVV(N,I)=BPZ(I)
  800 END DO
!
!
 1000 END DO
!
      DO 1800 N=1,NM
      B(N)=ONE
 1800 END DO
!
!
      DO 4000 I=2,IORDER
      DI2=I-2
      DI1=I-1
!
      DO 2800 N=1,NM
      B(N)=B(N)*(S(N)+DI2)/DI1
 2800 END DO
!
!
      DO 4000 K=I,IORDER
      K1=K-I+1
!
      DO 3800 N=1,NM
      CIVV(N,K)=CIVV(N,K)+BPZ(K1)*B(N)
      CIPV(N,K)=CIPV(N,K)+BPPZ(K1)*B(N)
 3800 END DO
!
!
 4000 CONTINUE
      DO 7000 I=3,IORDER
!
      DO 4800 N=1,NM
      B(N)=ZERO
 4800 END DO
!
!
      DO 6000 K=I,IORDER
!
      DO 5800 N=1,NM
      B(N)=B(N)+COMB(K-2,I-2)*CIPV(N,K)
 5800 END DO
!
!
 6000 END DO
!
      DO 6800 N=1,NM
      CIPV(N,I-2)=B(N)
 6800 END DO
!
!
 7000 END DO
      DO 10000 I=2,IORDER
!
      DO 7800 N=1,NM
      B(N)=ZERO
 7800 END DO
!
!
      DO 9000 K=I,IORDER
!
      DO 8800 N=1,NM
      B(N)=B(N)+COMB(K-1,I-1)*CIVV(N,K)
 8800 END DO
!
!
 9000 END DO
!
      DO 9800 N=1,NM
      CIVV(N,I-1)=B(N)
 9800 END DO
!
!
10000 END DO
      RETURN
      END
