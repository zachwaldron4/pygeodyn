      SUBROUTINE NSUMP(NEQN,IOL1,SUMPOS,PDDTOS,KNSTEPS,H,TMOLD,TMNEW,  &
     &                 SUMPNS,NEQNIM)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   PURPOSE:  COMPUTE THE INTEGRATION SUMS FOR A NEW INTEGRATION BUFFER
!
!   I     NEQN    - NUMBER OF INTEGRATED FORCE MODEL PARAMETERS
!   I     IOL1    - INTEGRATION ORDER MINUS 1
!   I     SUMPOS  - INTEGRATION SUMS FOR OLD BUFFER
!   I     PDDTOS  - ACCELERTIONS FOR OLD BUFFER
!   I     KNSTEPS - NUMBER OF ACCELERTIONS PER BUFFER
!   I     H       - STEP SIZE IN SECONDS
!   I     TMOLD   - TIME TAG OF OLD INTEGRATION SUMS
!   I     TMNEW   - TIME TAG OF NEW INTEGRATION SUMS
!   O     SUMPNS  - INTEGRATION SUMS FOR NEW BUFFER
!
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION SUMPOS(NEQN,3,2)
      DIMENSION PDDTOS(NEQN,3,KNSTEPS)
      DIMENSION SUMPNS(NEQNIM,3,2)

!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!

      DO I=1,6
         DO J=1,NEQN
           SUMPNS(J,I,1)=SUMPOS(J,I,1)
!        write(6,*)' dbg in NSUMP ',SUMPNS(J,I,1),J,I,1
         ENDDO
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
      DO  50 K=1,NEQN
      SUMPNS(K,J,2)=SUMPNS(K,J,2)                                       &
     &              +PDDTOS(K,J,IOL1+I)
      SUMPNS(K,J,1)=SUMPNS(K,J,2)                                       &
     &             +SUMPNS(K,J,1)
!     write(6,*)' dbg 2 NSUMP ',SUMPNS(K,J,1),SUMPNS(K,J,2),K,J,I
  50  CONTINUE
 100  CONTINUE
 150  CONTINUE

      do ii=1,NEQN
      do jj=1,3
      do kk=1,2
!     write(6,*)' dbg FINAL SUMPNS ',SUMPNS(ii,jj,kk),ii,jj,kk
      enddo
      enddo
      enddo

      RETURN
      END
