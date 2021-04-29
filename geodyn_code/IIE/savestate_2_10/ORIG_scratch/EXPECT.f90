!$EXPECT
      SUBROUTINE EXPECT(BTWB,BTWD,BDELTA,MEDIM,MEPARM,NEPARM,DELRMS)
!********1*********2*********3*********4*********5*********6*********7**
! EXPECT           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  COMPUTES E-BIAS CORRECTIONS TO RMS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   BTWB    I/O   A    NORMAL MATRIX FOR E-BIAS
!   BTWD    I/O   A    RIGHT HAND SIDE OF E-BIAS SOLUTION
!   BDELTA   I    A    ARRAY OF E-BIAS CORRECTIONS
!   MEDIM    I    S    MEPARM*(MEPARM+1)/2
!   MEPARM   I    S    MAX. # OF E-BIAS PARAMETERS FOR A SINGLE
!                      MEASUREMENT
!   NEPARM   I    S    NUMBER OF E-BIASES FOR THIS MEASUREMENT
!   DELRMS   O    S    CORRECTION TO RMS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION BTWB(MEDIM),BTWD(MEPARM),BDELTA(NEPARM)
      DATA ZERO/0.0D0/,TWO/2.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      SUMFSQ=ZERO
      SUMRF =ZERO
      I0=0
      DO 4000 I=1,NEPARM
      SUMRF =SUMRF +BTWD(I)*BDELTA(I)
      SUM   =ZERO
      DO 1000 J=I,NEPARM
      IJ=I0+J
      SUM=SUM+BTWB(IJ)*BDELTA(J)
 1000 END DO
      IF(I.EQ.1) GO TO 3000
      IL1=I-1
      K0=0
      DO 2000 K=1,IL1
      IK=K0+I
      SUM=SUM+BTWB(IK)*BDELTA(K)
 2000 K0=K0+MEPARM-K
 3000 CONTINUE
      SUMFSQ=SUMFSQ+BDELTA(I)*SUM
 4000 I0=I0+MEPARM-I
      DELRMS=SUMFSQ-TWO*SUMRF
      DO 5000 I=1,MEDIM
      BTWB(I)=ZERO
 5000 END DO
      DO 6000 I=1,MEPARM
      BTWD(I)=ZERO
 6000 END DO
      RETURN
      END
