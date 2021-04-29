!$TROPZN
      FUNCTION TROPZN(MTYPE,SINE,COSE,EDOT)
!********1*********2*********3*********4*********5*********6*********7**
! TROPZN           00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  COMPUTE PARTIAL ZENITH TROPOSPHERE CORRECTION WRT ZENITH
!            PATH LENGTH(SCALING FACTOR)
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MTYPE    I         MEASUREMENT TYPE (2=RANGE, 3=RANGE RATE)
!   SINE     I         SINE OF ELEVATION ANGLE
!   COSE     I         COSINE OF ELEVATION ANGLE
!   EDOT     I         TIME DERIVATIVE OF ELEVATION ANGLE
!
!
! COMMENTS:
!
!    CORRECTION = ZENITH PATH /(ELEVATION ANGLE ADJUSTMENT)
!    PARTIAL    =      1      /(ELEVATION ANGLE ADJUSTMENT)
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DATA ONENEG/-1.0D0/
      DATA TRACE1/0.00143D0/,TRACE2/0.045D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!*** DELAY
      TANE=SINE/COSE
      PMDLY =ONENEG / ( SINE + TRACE1/(TANE + TRACE2) )
      TROPZN=PMDLY
      IF (MTYPE.LE.2) RETURN
!***DELAY RATE
      TROPZN= EDOT   * PMDLY**2                                         &
     &               * (COSE - TRACE1/((TANE+TRACE2)*COSE)**2)
      RETURN
      END
