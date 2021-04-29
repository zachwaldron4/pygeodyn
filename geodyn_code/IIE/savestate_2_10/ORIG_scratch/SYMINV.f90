
!$SYMINV
      SUBROUTINE SYMINV(SUM1,NDIM,NLIM,DELTA)
!********1*********2*********3*********4*********5*********6*********7**
! SYNINV           00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  TO RECURSIVELY FIND INVERSE OF SYMMETRIC MATRIX
!            CALLING SEQUENCE  CALL SYMINV(SUM1,NDIM,NLIM,DELTA)
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   SUM1    I/O        LOWER RECTANGULAR PART OF MATRIX
!    (1)               TO BE INVERTED AS INPUT
!                      LOWER RECTANGULAR PART OF INVERTED MATRIX AS
!                      OUTPUT
!    NDIM    I         DIMENSION OF MATRIX
!    NLIM    I         DIMENSION OF PARTITION TO BE INVERTED
!    DELTA             SCRATCH
!    (1)
!
!
! COMMENTS:
!
! REFERENCES:       'GEODYN SYSTEMS DESCRIPTION'
!                   VOLUME 1 - GEODYN DOCUMENTATION
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DATA ZERO/0.D0/,ONE/1.D0/
      DIMENSION SUM1(1),DELTA(1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! INITIALIZE BY FINDING INVERSE OF 1X1
      SUM1(1)=ONE/SUM1(1)
      IF(NLIM.EQ.1) RETURN
      N1=NDIM-1
! RECURSIVELY FIND INVERSE OF NXN KNOWING INVERSE OF (N-1)X(N-1) UNTIL
! THE INVERSE OF AN NLIM X NLIM SQUARE PARTITION IS FOUND
      DO 400 N=2,NLIM
      NM1=N-1
      IL1=0
      DO 100 IL=1,NM1
      J1=0
      DELTA(IL)=ZERO
      DO 60 J=1,IL
      JL=J1+IL
      JN=J1+N
      DELTA(IL)=DELTA(IL)+SUM1(JL)*SUM1(JN)
   60 J1=J1+NDIM-J
      IF(IL.EQ.NM1) GO TO 100
      ILP1=IL+1
      DO 80 J=ILP1,NM1
      JN=J1+N
      JL=IL1+J
      DELTA(IL)=DELTA(IL)+SUM1(JL)*SUM1(JN)
   80 J1=J1+NDIM-J
  100 IL1=IL1+NDIM-IL
      J1=N
      NN=N1+N
      DO 150 J=1,NM1
      SUM1(NN)=SUM1(NN)-DELTA(J)*SUM1(J1)
  150 J1=J1+NDIM-J
      SUM1(NN)=ONE/SUM1(NN)
      J1=N
      DO 200 J=1,NM1
      SUM1(J1)=-DELTA(J)*SUM1(NN)
  200 J1=J1+NDIM-J
      I1=N
      DO 300 I=1,NM1
      J1=I
      DO 250 J=1,I
      SUM1(J1)=SUM1(J1)-SUM1(I1)*DELTA(J)
  250 J1=J1+NDIM-J
  300 I1=I1+NDIM-I
  400 N1=N1+NDIM-N
      RETURN
      END
