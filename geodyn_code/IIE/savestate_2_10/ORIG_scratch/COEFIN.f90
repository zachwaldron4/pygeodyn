!$COEFIN
      SUBROUTINE COEFIN
!********1*********2*********3*********4*********5*********6*********7**
! COEFIN           82/08/27            8208.0    PGMR - TOM MARTIN
!
! FUNCTION:  INITIALIZE COMMON/CINTRP/
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CINTRP/COMB(21,21),BPZ(21),BPPZ(21)
      DATA ZERO/0.0D0/,ONE/1.0D0/
      DATA MAX00/20/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! THRU LABEL 1000 COMPUTES BINOMIAL COEFFICIENTS WITH SIGNS ALTERNATING
!    BY MATRIX COLUMN.
! COMB CONTAINS PASCAL'S TRIANGLE WITH ALTERNATING SIGNS BY COLUMN.
      COMB(1,1)=ONE
      COMB(2,1)=ONE
      COMB(2,2)=-ONE
      FACT=ONE
      M1=MAX00+1
      DO 1000 I=3,M1
      COMB(I,1)=ONE
      COMB(I,I)=FACT
      I1=I-1
      DO 500 J=2,I1
  500 COMB(I,J)=COMB(I-1,J)-COMB(I-1,J-1)
      FACT=-FACT
 1000 END DO
! THRU LABEL 1200 COMPUTES TH ADAMS-MOULTON CORRECTOR COEFFICIENTS
!    IN NON-SUMMED DIFFERENCE FORM
      BPZ(1)=ONE
      BPPZ(1)=ONE
      DO 1200 I=2,MAX00
      IL1=I-1
      SUM=ZERO
      DO 1100 K=1,IL1
      SUM=SUM-BPZ(K)/(I-K+1)
 1100 END DO
      BPZ(I)=SUM
 1200 END DO
! THRU LABEL 1400 COMPUTES THE COWELL CORRECTOR COEFFICIENTS IN
!    NON-SUMMED DIFFERENCE FORM
      DO 1400 I=2,MAX00
      SUM=ZERO
      DO 1300 K=1,I
      SUM=SUM+BPZ(K)*BPZ(I-K+1)
 1300 END DO
      BPPZ(I)=SUM
 1400 END DO
      RETURN
      END
