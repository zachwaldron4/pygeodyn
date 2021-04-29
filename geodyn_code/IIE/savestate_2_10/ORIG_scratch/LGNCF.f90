!$LGNCF
      SUBROUTINE LGNCF(COEFZ,COEFC)
!********1*********2*********3*********4*********5*********6*********7**
! LGNCF            00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  PRECALCULATE THE COEFCIENTS NEEDED TO DO LGENDR
!            RECURSIONS NORMALIZED
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   COEFZ    O    A    COEFZ(N) SQRT(1+1/2N) IS USED FOR THE SECTORAL
!                      RECURSION
!   COEFC    O    A    COEFC(*,1&2) ARE FILLED TO USE IN RECURSION FOR
!                      ZONALS AND TESSERALS
!                      COEFC(*,3) NORMALIZATION FACTOR FOR N,M DEVIDED
!                      BY THE NORMALIZATION FACTOR FOR N,M+1.
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      DIMENSION COEFZ(1),COEFC(NPMAX,3)
      DATA ZERO/0.D0/,HALF/.5D0/,ONE/1.D0/,TWO/2.D0/,THREE/3.D0/,       &
     &     FOUR/4.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 21 I=1,NMAX
   21 COEFZ(I)=SQRT(ONE+HALF/DBLE(I))
      DO 22 I=1,NPMAX
      COEFC(I,1)=ZERO
      COEFC(I,2)=ZERO
      COEFC(I,3)=ZERO
   22 END DO
      COEFC(1,3)=ONE
!
      DO 50 I=2,NMAX
      JMAX=I-1
      ISTRT=JMAX*(JMAX+7)/2+1
      XN2=DBLE(I)*DBLE(I)
      XN241=FOUR*XN2-ONE
      DO 23 J=1,I
   23 COEFC(ISTRT-1+J,1)=XN241/(XN2-DBLE(J-1)*DBLE(J-1))
      X2NP1=TWO*DBLE(I)+ONE
      X2NM3=TWO*DBLE(I)-THREE
      XFACT=X2NP1/X2NM3
      XNM12=DBLE(I-1)**2
      DO 24 J=1,I
      COEFC(ISTRT-1+J,2)=XFACT*(XNM12-DBLE(J-1)*DBLE(J-1))
   24 COEFC(ISTRT-1+J,2)=COEFC(ISTRT-1+J,2)/(XN2-DBLE(J-1)            &
     &                  *DBLE(J-1))
      COEFC(ISTRT,3)=DBLE(I)*DBLE(I+1)*HALF
      DO 25 J=1,JMAX
      IJIJ1=(I-J)*(I+J+1)
   25 COEFC(ISTRT+J,3)=DBLE(IJIJ1)
      DO 40 J=1,I
      COEFC(ISTRT-1+J,1)=SQRT(COEFC(ISTRT-1+J,1))
      COEFC(ISTRT-1+J,2)=SQRT(COEFC(ISTRT-1+J,2))
      COEFC(ISTRT-1+J,3)=SQRT(COEFC(ISTRT-1+J,3))
   40 END DO
   50 END DO
      RETURN
      END
