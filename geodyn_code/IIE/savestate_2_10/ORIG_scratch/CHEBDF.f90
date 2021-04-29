!$CHEBDF
      SUBROUTINE CHEBDF(CHB1,CHB2,DELT,NM,IORD)
!********1*********2*********3*********4*********5*********6*********7**
! CHEBDF           00/00/00            8901.0    PGMR - D. ROWLANDS
!                                      8901.0    PGMR - A. MARSHALL
!
! FUNCTION:  CALCULATE CHEBYCHEV POLYNOMIAL DIFFERENCES
!            WITH MORE PRECISION THAN SIMPLE DIFFERENCING
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   CHB1     I    A    VALUE OF CHEBYSHEV POLYNOMIALS AT T1
!   CHB2    I\O   A    VALUE OF CHEBYSHEV POLYNOMIALS AT T2 (INPUT)
!                      VALUE OF CHEBYSHEV POLYNOMIAL DIFFERENCES
!                      (OUTPUT)
!   DELT     I    A    DIFFERENCE IN TIME NORMALIZED FOR -1,+1 INTERVAL.
!                      NOTE!!!!! IT IS IMPORTANT THAT DELT BE CALCULATED
!                      BY DIFFERENCING FSEC ARRAYS BEFORE NORMALIZING.
!                      FORMALLY, DELT(I)=CHB2(I,2)-CHB1(I,2). BUT
!                      SIGNIFICANCE IS LOST IN THAT COMPUTATION SINCE
!                      ELAPSED TIME IS IN (MANY) DAYS.
!   NM       I    S    NUMBER OF TIMES.
!   IORD     I    S    HIGHEST DEGREE POLYNOMIAL TO BE EVALUATED
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION CHB1(NM,IORD),CHB2(NM,IORD),DELT(NM)
      DATA ZERO/0.D0/,TWO/2.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 100 I=1,NM
      CHB2(I,1) = TWO*CHB2(I,2)
      CHB2(I,3)=(CHB1(I,2)+CHB2(I,2))*TWO*DELT(I)
      CHB2(I,2)=DELT(I)
      DELT(I)=TWO*DELT(I)
  100 END DO
      DO 300 I=4,IORD
      DO 200 J=1,NM
      CHB2(J,I)=CHB2(J,1)*CHB2(J,I-1)-CHB2(J,I-2)                       &
     &            +DELT(J)*CHB1(J,I-1)
  200 END DO
  300 END DO
      DO 400 I=1,NM
      CHB2(I,1)=ZERO
  400 END DO
      RETURN
      END
