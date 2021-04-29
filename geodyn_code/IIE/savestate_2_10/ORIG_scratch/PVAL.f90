!$PVAL
      SUBROUTINE PVAL(ND1,TM,ACOEF,LD,LDD,P,PD,PDD)
!********1*********2*********3*********4*********5*********6*********7**
! PVAL               00/00/00            0000.0    PGMR -DAVE ROWLANDS
!
!
! FUNCTION: PROCESS DYNAMIC CROSSOVERS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER(MAXND1=21)
!
      DIMENSION ACOEF(ND1)
      DIMENSION SCALE(MAXND1),SCALE2(MAXND1),TP(MAXND1)
!
      DATA SCALE /  1.D0,  2.D0,  3.D0,  4.D0,  5.D0,  6.D0,  7.D0,     &
     &              8.D0,  9.D0, 10.D0, 11.D0, 12.D0, 13.D0, 14.D0,     &
     &             15.D0, 16.D0, 17.D0, 18.D0, 19.D0, 20.D0, 21.D0/
      DATA SCALE2/  2.D0,  6.D0, 12.D0, 20.D0, 30.D0, 42.D0, 56.D0,     &
     &             72.D0, 90.D0,110.D0,132.D0,156.D0,182.D0,210.D0,     &
     &            240.D0,272.D0,306.D0,342.D0,380.D0,420.D0,462.D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      IF(ND1.GT.MAXND1) THEN
       ND=ND1-1
       MD=MAXND1-1
       WRITE(6,6000)
       WRITE(6,6000)
       WRITE(6,6001)
       WRITE(6,6002) ND
       WRITE(6,6003) MD
       STOP
      ENDIF
!
!
!
      TP(1)=1.D0
      P=ACOEF(1)
      PD=0.D0
      PDD=0.D0
!
!
!
      IF(ND1.EQ.1) RETURN
      DO 10 I=2,ND1
      TP(I)=TP(I-1)*TM
      P=P+ACOEF(I)*TP(I)
   10 END DO
!
!
!
      IF(LD) THEN
      DO 20 I=2,ND1
      PD=PD+ACOEF(I)*SCALE(I-1)*TP(I-1)
   20 END DO
      ENDIF
!
!
!
      IF(ND1.LE.2) RETURN
!
!
!
      IF(LDD) THEN
      DO 30 I=3,ND1
      PDD=PDD+ACOEF(I)*SCALE2(I-2)*TP(I-2)
   30 END DO
      ENDIF
!
!
!
      RETURN
 6000 FORMAT(' ')
 6001 FORMAT(' EXECUTION TERMINATING IN PVAL')
 6002 FORMAT(' DEGREE OF INPUT POLYNOMIAL: ',I3)
 6003 FORMAT(' EXCEEDS MAX ALLOWABLE: ',I3)
      END
