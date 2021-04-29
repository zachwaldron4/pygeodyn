!$ROTYTR
      SUBROUTINE ROTYTR(MJDSC,FSEC,THETG,NM,THTDX)
!********1*********2*********3*********4*********5*********6*********7**
! DYQN             86/12/24            8701.0    PGMR - D. ROWLANDS
!
!  FUNCTION:  PRECESION (NUTATION) MATRIX IS CALCULATED.
!             FOR A PLANET OTHER THAN EARTH. UNLIKE DEQN (EARTH)
!             THIS IS DONE IN A STRAIGHTFORWARD MANNER.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSC    I    S    INTEGRAL EPHEMERIS SECOND TIME
!   FSEC     I    A    FRACTIONAL REMAINING SECONDS
!   THETG
!   NM       I    S    NUMBER OF OBSERVATIONS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/YODER /RFXM2E(9),XI0MR,XI0MRD,XI00MR,XIMR(9),              &
     &                        TSI0MR,TSIMRD,TSIMR(9),                   &
     &              ALFMAR(9),THTMAR(9),XNPMAR,XMAMR0,XS2000,XRATE0,    &
     &              FI0MR,FI0MRD,FICMR(4),FISMR(4)
      DIMENSION FSEC(NM)
      DIMENSION THETG(NM)
      DIMENSION XLP(4)
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      THTDX=FI0MRD
      TWOPI=2.D0*ACOS(-1.D0)
!
!
!
      DIFF0=DBLE(MJDSC)-XS2000
      DIFFR=DBLE(MJDSC)-XRATE0
!
!
!
!
!
!
!
      DO 1000 I=1,NM
      TIME=DIFF0+FSEC(I)
      TIMR=DIFFR+FSEC(I)
!
      XLP(1)=XMAMR0+TIME*XNPMAR
      XLP(2)=XLP(1)+XLP(1)
      XLP(3)=XLP(2)+XLP(1)
      XLP(4)=XLP(2)+XLP(2)
!
      XINUT=XI00MR
      TSINUT=0.D0
      DO J=1,9
        ANG=ALFMAR(J)*TIME+THTMAR(J)
        XINUT=XINUT+XIMR(J)*COS(ANG)
        TSINUT=TSINUT+TSIMR(J)*SIN(ANG)
      ENDDO
      XI= XI0MR +TIME*XI0MRD+XINUT
!
!
      HOLD=FI0MR+TIMR*FI0MRD
      HOLD=MOD(HOLD,TWOPI)
      THETG(I)=0.D0
      DO J=1,4
        THETG(I)=THETG(I)+FICMR(J)*COS(XLP(J))+FISMR(J)*SIN(XLP(J))
      ENDDO
      THETG(I)=THETG(I)-TSINUT*COS(XI)
      THETG(I)=THETG(I)+HOLD
      THETG(I)=MOD(THETG(I),TWOPI)
!
!
!
!
!
!
 1000 CONTINUE
      RETURN
      END
