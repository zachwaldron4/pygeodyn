!$PMASCE
      SUBROUTINE PMASCE(T,VMASCE,VPMOBE)
!********1*********2*********3*********4*********5*********6*********7
! PMASCE           12/02/92            0000.0    PGMR - S.LUO
!
! FUNCTION: CALCULATE THE DIRECTIONS OF MARS ORBIT ASCENDING NODE
!           AND THE POLE OF ITS ORBIT IN ECLIPTIC FRAME
!
! I/O PARAMETERS
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   T        I         TIME INTERVAL IN DAY FROM J2000.0
!   VMASCE   O    A    THE DIRECTION OF MARS ASCENDING NODE IN
!                          ECLIPTIC
!   VPMOBE   O    A    THE DIRECTION OF THE POLE OF MARS ORBIT IN
!                          ECLIPTIC
!  ....................................................................
!   OMEGA              LONGITUDE OF MARS ORBITAL ASCENSION NODE
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/ELEMTM/W(6),OM(6),DL(4),CI(3),DPERIM
      DIMENSION VMASCE(3),VPMOBE(3)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
! CALCULATE THE LONGITUDE OF MARS ASCENDING NODE
!
      D = T
      D = D/36525.D0
      T2 = D
      T3 = T2 * D
      T4 = T3 * D
      T5 = T4 * D
      T6 = T5 * D
!
      OMEGA = OM(1) + OM(2)*T2 + OM(3)*T3 + OM(4)*T4 + OM(5)*T5         &
     &              + OM(6)*T6
!
      CIT = CI(1) + CI(2)*T2 + CI(3)*T3
!
      WA = W(1) + W(2)*T2 + W(3)*T3 + W(4)*T4 + W(5)*T5
!
      DPERIM = WA - OMEGA
!
      CIA = CIT * DEGRAD
      OMA = OMEGA * DEGRAD
!
      CSOMA = COS(OMA)
      SNOMA = SIN(OMA)
!
      VMASCE(1) = CSOMA
      VMASCE(2) = SNOMA
      VMASCE(3) = 0.D0
!
      CLAMD  = OMEGA - 90.D0
      BET   = 90.D0 - CIT
      CLAMDA =CLAMD * DEGRAD
      BETA  = BET * DEGRAD
!
      CSLA = COS(CLAMDA)
      SNLA = SIN(CLAMDA)
      CSBE = COS(BETA)
      SNBE = SIN(BETA)
!
      VPMOBE(1) = CSBE * CSLA
      VPMOBE(2) = CSBE * SNLA
      VPMOBE(3) = SNBE
!
! DEBUG .....
!
!CC   WRITE(6,100)OMEGA, VMASCE
  100 FORMAT(1X, 'OMEGA=',F10.2,'VMASCE=',3F10.5)
      RETURN
      END
