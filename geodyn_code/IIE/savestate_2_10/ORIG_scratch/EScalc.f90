!$ESCALE
      Subroutine EScalc(stdl,SIGMA,ES)
!********1*********2*********3*********4*********5*********6*********7**
! ESCALE           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   stdl     I    S
!    ES      O    A
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!.... EPS ARE STD VARNS FROM NOMINAL VALUES
!.... 0,2,....10  LONG - TERM
!.... 1,3.....,11  SHORT-TERM
      DIMENSION SIG(0:11), EPS(0:11), ES(0:11)
      DATA EPS/12*0.0/
      DO 10 I = 2,10,2
   10 EPS(I) = stdl
      Do 20 I = 3,9,2
   20 EPS(I) = SIGMA
      SIG(0) = 0.25
!.... LONG TERM -FBAR
!.... THESE ARE COEF. OF VARIATION, I.E. SIGMA/MU,    %/100.0
      SIG(1) = 0.0
!.... SHORT TERM-FBAR
      SIG(2) = 0.16
!.... LONG TERM-TINF
      SIG(3) = 0.12
!.... SHORT TERM-TINF
      SIG(4) = 0.40
!.... LONG TERM - FOXY
      SIG(5) = 0.12
!.... SHORT TERM -FOXY
      SIG(6) = 0.0
!.... LONG TERM -AOXY
      SIG(7) = 0.21
!.... SHORT TERM -AOXY
      SIG(8) = 0.045
!.... LONG TERM - ZF
      SIG(9) = 0.0225
!.... SHORT TERM - ZF
      SIG(10) = 0.30
!.... LONG TERM- DZDUST
      SIG(11) = 0.0
!.... SHORT TERM - DZDUST
      DO 100 I = 0, 11
  100 ES(I) = EPS(I) * SIG(I)
      Return
      END
