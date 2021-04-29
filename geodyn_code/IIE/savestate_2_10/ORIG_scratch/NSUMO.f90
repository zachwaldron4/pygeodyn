      SUBROUTINE NSUMO(IOL1,SUMXOS,XDDTOS,KNSTEPS,H,TMOLD,TMNEW,SUMXNS)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   PURPOSE:  COMPUTE THE INTEGRATION SUMS FOR A NEW INTEGRATION BUFFER
!
!   I     IOL1    - INTEGRATION ORDER MINUS 1
!   I     SUMXOS  - INTEGRATION SUMS FOR OLD BUFFER
!   I     XDDTOS  - ACCELERTIONS FOR OLD BUFFER
!   I     KNSTEPS - NUMBER OFACCELERTIONS PER BUFFER
!   I     H       - STEP SIZE IN SECONDS
!   I     TMOLD   - TIME TAG OF OLD INTEGRATION SUMS
!   I     TMNEW   - TIME TAG OF NEW INTEGRATION SUMS
!   O     SUMXNS  - INTEGRATION SUMS FOR NEW BUFFER
!
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION SUMXOS(3,2),XDDTOS(3,KNSTEPS)
      DIMENSION SUMXNS(3,2)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO I=1,6
        SUMXNS(I,1)=SUMXOS(I,1)
      ENDDO
      TDIFF=TMNEW-TMOLD
      EPS=ABS(0.1D0*H)
      XSTEP=(ABS(TDIFF/H)+EPS)
      NSTEP=XSTEP
      IF(NSTEP.EQ.0) RETURN
!
! UPDATE SUMS
      DO 150 I=1,NSTEP
      DO 100 J=1,3
      SUMXNS(J,2)=SUMXNS(J,2)                                           &
     &           +XDDTOS(J,IOL1+I)
      SUMXNS(J,1)=SUMXNS(J,2)                                           &
     &           +SUMXNS(J,1)
 100  CONTINUE
 150  CONTINUE
 160  CONTINUE
      RETURN
      END
