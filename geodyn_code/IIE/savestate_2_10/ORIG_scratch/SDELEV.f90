

!$SDELEV
      SUBROUTINE SDELEV (LPTS ,ELEVSC,NM    ,ELCUTO,LANGL)
!********1*********2*********3*********4*********5*********6*********7**
! SDELEV           88/07/01            8805.0    PGMR - DESPINA PAVLIS
!
! FUNCTION:  CHECK SIMULATED OBSERVATIONS  FOR  ELEVATION CUTOFF.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   LPTS    I/O   A    LOGICAL EDIT SWITCHES AS INPUT
!                      UPDATED LOGICAL EDIT SWITCHES AS OUTPUT
!   ELEVSC   I    A    ELEVATION ANGLES FOR EACH STATION
!   NM       I    S    NUMBER OF MEASUREMENTS
!   ELCUTO   I    S    ELEVATION CUTOFF ANGLE
!   LANGL    I         TRUE FOR RIGHT ASC/DEC
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      DIMENSION LPTS (NM),ELEVSC(NM,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      FACT=1.D0
      IF(LANGL)THEN
      FACT=DEGRAD
      ENDIF
      DO 1000 N=1,NM
      EL=ELEVSC(N,1)/FACT
      LPTS(N)=EL.GT.ELCUTO
 1000 END DO
      RETURN
      END
