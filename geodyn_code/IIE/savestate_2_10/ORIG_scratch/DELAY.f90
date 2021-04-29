!$DELAY
      SUBROUTINE DELAY (XDELAY,NTERMS,RRATE,NM,DR)
!********1*********2*********3*********4*********5*********6*********7**
! DELAY            85/03/02            8503.0    PGMR - TOM MARTIN
!
! FUNCTION:  COMPUTE CORRECTIONS TO RANGE DUE TO ELECTRONIC
!            TRANSPONDER DELAYS
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XDELAY   I    A    TRANSPONDER DELAY CURVE AS FUNCTION OF
!                      RANGE RATE
!                        XDELAY(1)=BIAS TERM IN METERS
!                        XDELAY(2)=R RATE TERM IN M/(M/S)
!                        XDELAY(3)=R RATE SQUARED TERM IN M/(M/S)**2
!   NTERMS   I    S     NUMBER OF TERMS (1-3)
!   RRATE    I    A     ACCUMULATED RANGE RATES
!   NM       I    S     NUMBER OF RANGE RATES AND RANGE CORRECTIONS
!   DR       O    A     CORRECTIONS TO RANGES
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION XDELAY(3),RRATE(NM),DR(NM)
      DATA ZERO/0.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 N=1,NM
      DR(N)=DR(N)+XDELAY(1)
 1000 END DO
      IF(NTERMS.LT.2) RETURN
      NEXP=1
      DO 2000 I=2,3
      DO 1800 N=1,NM
      DR(N)=DR(N)+XDELAY(I)*RRATE(N)**NEXP
 1800 END DO
      NEXP=NEXP+1
 2000 END DO
      RETURN
      END
