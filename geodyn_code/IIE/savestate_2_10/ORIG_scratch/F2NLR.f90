!$F2NLR
      FUNCTION F2NLR(T,N2,L,IR)
!********1*********2*********3*********4*********5*********6*********7**
! F2NLR            83/08/17            8308.0    PGMR - DEMOS
!                                                        CHRISTODOULIDIS
!                                                       D. ROWLANDS
!                                                        (MOD FOR GII)
!
! FUNCTION:  COMPUTE A FACTOR FOR A
!            DOODSON'S COEFFCIENT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   T        I    S    COSINE SQUARED OF HALF THE ECLIPTIC
!   N2       I    S    2 TIMES THE DEGREE OF THE COEFFICIENT
!   L        I    S    TIDAL EXPANSION ARGUMENT
!   IR       I    S    TIDAL EXPANSION ARGUMENT
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DIMENSION DCOEF(100)
      DATA DCOEF/1.D0,99*0.D0/,ZERO/0.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      N=N2/2
      IORDER=N+IR
      I1=N-L
      I2=N+L
      I3=I2+1
      I3T=I3
      IF(I3T.EQ.1) GO TO 11
      DO 10 I=2,I3
      IEXP=I2-I+2
      DCOEF(I)=-DCOEF(I-1)*IEXP/(I-1)
   10 END DO
   11 CONTINUE
      SUM=ZERO
      IDEG=N2+1
      DO 20 I=1,I3
      IDEG=IDEG-1
      SUM=SUM+DERIV(IORDER,T,IDEG)*DCOEF(I)
   20 END DO
      F2NLR=SUM
      RETURN
      END
