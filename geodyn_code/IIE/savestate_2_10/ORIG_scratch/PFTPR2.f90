!$PFTPR2
      SUBROUTINE PFTPR2(NU,ND2AC,ND1,TIME,XX,YY,ZZ,ACOEF)
!********1*********2*********3*********4*********5*********6*********7**
! PFTPAR             00/00/00            0000.0    PGMR -DAVE ROWLANDS
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
      DIMENSION TIME(NU),XX(NU),YY(NU),ZZ(NU)
      DIMENSION ACOEF(ND1,ND2AC,3)
      DIMENSION XPOS(3)
!
!  231=21*22/2=MAXND1*(MAXND1+1)/2
!
      DIMENSION ATA(231),SQMAT(MAXND1,MAXND1)
      DIMENSION ATL(MAXND1,3),SCR(MAXND1)
      DIMENSION SCR2(MAXND1),SCR3(MAXND1),SCR4(MAXND1)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      NU1=NU+1
      IF(ND1.GT.MAXND1) THEN
         NDP=ND1-1
         MDP=MAXND1-1
         WRITE(6,6000)
         WRITE(6,6000)
         WRITE(6,6001)
         WRITE(6,6002) NDP
         WRITE(6,6003) MDP
         WRITE(6,6000)
         STOP
      ENDIF
!
      NZ=(ND1*(ND1+1))/2
      CALL CLEARA(ATA,NZ)
      NZ=ND1
      CALL CLEARA(ATL,NZ)
      CALL CLEARA(ATL(1,2),NZ)
      CALL CLEARA(ATL(1,3),NZ)
      NZ=3*ND1*ND2AC
      CALL CLEARA(ACOEF,NZ)
      SCR(1)=1.D0
!
      DO 300 ITM=1,NU
      ATL(1,1)=ATL(1,1)+XX(ITM)
      ATL(1,2)=ATL(1,2)+YY(ITM)
      ATL(1,3)=ATL(1,3)+ZZ(ITM)
      DO 50 I=2,ND1
      SCR(I)=SCR(I-1)*TIME(ITM)
      ATL(I,1)=ATL(I,1)+XX(ITM)*SCR(I)
      ATL(I,2)=ATL(I,2)+YY(ITM)*SCR(I)
      ATL(I,3)=ATL(I,3)+ZZ(ITM)*SCR(I)
   50 END DO
      IN=0
      NL=ND1
      DO 200 I=1,ND1
      IIN=IN-(I-1)
      DO 100 J=I,ND1
      ATA(IIN+J)=ATA(IIN+J)+SCR(I)*SCR(J)
  100 END DO
      IN=IN+NL
      NL=NL-1
  200 END DO
  300 END DO
      CALL DSINV(ATA,ND1,ND1,SCR)
      IN=0
      NL=ND1
      DO 500 I=1,ND1
      IIN=IN-(I-1)
      DO 400 J=I,ND1
      SQMAT(I,J)=ATA(IIN+J)
      SQMAT(J,I)=SQMAT(I,J)
  400 END DO
      IN=IN+NL
      NL=NL-1
  500 END DO
      DO 800 K=1,1
      SCR4(1)=1.D0
      IF(K.EQ.1) THEN
        FACT=0.D0
        DO 530 I=2,ND1
        SCR4(I)=1.D0
  530   CONTINUE
      ELSE
        FACT=10.D0
        DO 540 I=2,ND1
        SCR4(I)=SCR4(I-1)*TIME(K-1)
  540   CONTINUE
      ENDIF
      DO 550 I=1,ND1
      SCR(I)=ATL(I,1)+FACT*SCR4(I)
      SCR2(I)=ATL(I,2)+FACT*SCR4(I)
      SCR3(I)=ATL(I,3)+FACT*SCR4(I)
  550 END DO
      DO 700 I=1,ND1
      DO 600 J=1,ND1
      ACOEF(I,K,1)=ACOEF(I,K,1)+SQMAT(I,J)*SCR(J)
      ACOEF(I,K,2)=ACOEF(I,K,2)+SQMAT(I,J)*SCR2(J)
      ACOEF(I,K,3)=ACOEF(I,K,3)+SQMAT(I,J)*SCR3(J)
  600 END DO
  700 END DO
  800 END DO
!
      RETURN
 6000 FORMAT(' ')
 6001 FORMAT(' EXECUTION TERMINATING IN SUBROUTINE PFTPR2')
 6002 FORMAT(' DEGREE OF POLYNIMIAL FIT REQUESTED:',I3)
 6003 FORMAT(' LARGER THAN MAX ALLOWABLE:',I3)
      END
