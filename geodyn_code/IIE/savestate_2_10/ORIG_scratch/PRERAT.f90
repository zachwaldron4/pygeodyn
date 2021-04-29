!$PRERAT
      SUBROUTINE PRERAT(IORDER,H,                                       &
     &   IOL1,NRAT,MJDO,FSECO,MJDRS,FSECRS,MJDE,FSECE,NSAVST,ISTPST,    &
     &   NSAVCW,ISTPCW)
!********1*********2*********3*********4*********5*********6*********7**
! PRERAT
!
! FUNCTION: PART OF THE STARTING PROCEDURE FOR MULTIRATE INTEGRATION.
!           FIGURE OUT THE NUMBER OF STEPS THAT NEED TO BE INTEGRATED
!           BACKWARD AT THE SMALL STEPSIZE AND THEN THE NUMBER OF
!           STEPS THAT NEED TO BE INTEGRATED FORWARD AFTER RESTARTING.
!           ALSO FIGURE OUT WHICH STEPS IN THE SMALL STEP STARTING
!           PROCEDURE CAN BE USED AS LARGE STEPS.
!
!           FOR THE CASE OF ORDER 11 AND LS/SS = 3
!
!                         10 EPOCHS FOR BIG STEPS IN STARTER
!            -5    -4    -3    -2    -1     E     1     2     3     4
!                         10 SMALL STEPS BACK AND RESET
!                       A 9 8 7 6 5 4 3 2 1 E
!             |     |     |     |     |     |     |     |     |     |
!             | | | | | | | | | | | | | | | | | | | | | | | | | | | |
!             S S S S S R 1 2 3 4 5 6 7 8 9 A 1 2 3 4 5 6 7 8 9 B 1 2
!                         22 STEPS FORWARD AFTER RESET
!                         28 TOTAL EPOCHS FOR SMALL STEPS  IN STARTER
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IORDER   I    S    ORDER OF COWELL INTEGRATION FOR ORBIT
!   H        I    S    LARGE INTEGRATION STEP SIZE IN SECONDS FOR ORBIT
!   IOL1     I    S    IORDER - 1
!   NRAT     I    S    RATIO OF LARGE STEPSIZE TO SMALL STEPSIZE
!                      ALWAYS AN INTEGER
!   MJDO     I    S    MODIFIED JULIAN DAY INTEGER SECONDS OF ORIGINAL
!                      EPOCH
!   FSECO    I    S    FRACTIOAL REMAINING SECONDS OF ORIGINAL EPOCH
!   MJDRS    O    S    MODIFIED JULIAN DAY INTEGER SECONDS OF EPOCH
!                      TO WHICH NEED TO BACKWARD INTEGRATE AT SMALL STEP
!   FSECRS   O    S    FRACTIONAL RENAINING SECONDS OF EPOCH
!                      TO WHICH NEED TO BACKWARD INTEGRATE AT SMALL STEP
!   MJDE     O    S    MODIFIED JULIAN DAY INTEGER SECONDS OF FINAL
!                      EPOCH IN STARTER
!   FSECE    O    S    FRACTIONAL REMAINING SECONDS OF FINAL EPOCH IN
!                      STARTER
!   NSAVST   O    S    NUMBER OF STEPS IN THE FINAL SMALL STEP STARTER
!                      WHICH CORRESPOND TO LARGE STEPS
!   ISTPST   O    A    STEP NUMBERS IN FINAL SMALL STEP STARTER WHICH
!                      CORRSPOND TO LARGE STEPS
!   NSAVCW   O    S    NUMBER OF STEPS IN THE FINAL SMALL STEP INTEGRATI
!                      FORWARD WHICH CORRESPOND TO LARGE STEPS
!   ISTPCW   O    A    STEP NUMBERS IN SMALL STEP INTEGRATION FORWARD
!                      (TO COMPLETE STARTING PROCESS) WHICH CORRSPOND
!                      TO LARGE STEPS
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION ISTPST(30),ISTPCW(30)
      DATA ZERO/0.D0/,ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      HS=H/DBLE(NRAT)
!
      IOL2=IOL1-1
      IEVEN=MOD(IOL1,2)
      MF=IOL2/2-IEVEN
      MB=MF+1+IEVEN
      MID=MB+1
      MF1=MF+1
!
      NBT=NRAT*MB
      NBTR=NBT-MB
! BACK TIME
      IHB=HS
      HB=HS-DBLE(IHB)
      IHB=IHB*NBTR
      HB=HB*DBLE(NBTR)
      MJDRS=MJDO-IHB
      FSECRS=FSECO-HB
      IF(FSECRS.LT.ZERO) THEN
         XX=ABS(FSECRS)
         IX=XX
         IX=IX+1
         MJDRS=MJDRS-IX
         FSECRS=FSECRS+DBLE(IX)
      ENDIF
! END TIME
      IHB=H
      HB=H-DBLE(IHB)
      IHB=IHB*MF
      HB=HB*DBLE(MF)
      MJDE=MJDO+IHB
      FSECE=FSECO+HB
      IF(FSECE.GT.ONE) THEN
        IX=FSECE
        MJDE=MJDE+IX
        FSECE=FSECE-DBLE(IX)
      ENDIF
! SOME SIDE COMPUTATIONS
      NTSSA=1+(IOL1-1)*NRAT
      NTSSS=NTSSA-(IOL1-1)
!
! FIGURE WHICH STEPS CAN BE SAVED
!
      DO I=1,30
        ISTPST(I)=0
        ISTPCW(I)=0
      ENDDO
!
      NSAVST=0
      DO 100 I=1,IOL1
      ISTEP=1+(I-1)*NRAT
      IF(ISTEP.GT.IOL1) GO TO 110
      NSAVST=NSAVST+1
      ISTPST(NSAVST)=ISTEP
  100 END DO
  110 CONTINUE
!
      NTCWA=NTSSA-IOL1
      NSAVCW=IOL1-NSAVST
      ISTEP=NTCWA
      ICT=0
      DO 200 I=1,NTCWA
      IF(ISTEP.LE.0) GO TO 210
!
      IF(ICT.GE.NSAVCW) THEN
         WRITE(6,6000)
         WRITE(6,6001)
         WRITE(6,6002)
         STOP
      ENDIF
!
      ISTPCW(NSAVCW-ICT)=ISTEP
      ICT=ICT+1
      ISTEP=ISTEP-NRAT
  200 END DO
  210 CONTINUE
      IF(ICT.NE.NSAVCW) THEN
         WRITE(6,6000)
         WRITE(6,6001)
         WRITE(6,6002)
         WRITE(6,6003) IOL1,NRAT,NSAVST,NSAVCW
         WRITE(6,6004) NTSSA,NTCWA,ICT,ISTEP
         STOP
      ENDIF
      RETURN
 6000 FORMAT(' EXECUTION TERMINATING IN PRERAT WHILE DETERMINING')
 6001 FORMAT(' WHICH INTEGRATION STEPS CAN BE SAVED FOR MULTIRATE ')
 6002 FORMAT(' INTEGRATION FROM STARTING PROCEDURE ')
 6003 FORMAT('  IOL1=',I5,' NRAT=',I5,' NSAVST=',I5,' NSAVCW=',I5)
 6004 FORMAT('  NTSSA=',I5,' NTCWA=',I5,' ICT=',I5,' ISTEP=',I5)
      END
