
!$GLOB5L
      FUNCTION GLOB5L(P)
!********1*********2*********3*********4*********5*********6*********7**
! GLOB5L           00/00/00            0000.0    PGMR - A. HEDIN
!
! FUNCTION -  LIMITED PARAMETER VERSION OF GLOBE 9/2/82
!             CALCULATE G(L) FUNCTION FOR MSIS-86/CIRA 1986
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   P
!
! COMMENTS:
!
! REFERENCES:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CSW/SW(25),SWC(25),ISW
      COMMON/LPOLY/PLG(9,4),CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC,    &
     &             DAY,DF,DFA,APD,APDF,APT(4),IYR
!
      DIMENSION P(150),TTT(15)
!
      DATA DR/1.72142D-2/,TTT/15*0.D0/
      DATA DAYL/-1.D0/,P7/-1000.D0/,P9/-1000.D0/,P11/-1000.D0/
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      IF(DAY.NE.DAYL.OR.P7.NE.P(7)) CD7=COS(DR*(DAY-P(7)))
      IF(DAY.NE.DAYL.OR.P9.NE.P(9)) CD9=COS(2.D0*DR*(DAY-P(9)))
      IF(DAY.NE.DAYL.OR.P11.NE.P(11)) CD11=COS(DR*(DAY-P(11)))
      DAYL=DAY
      P7=P(7)
      P9=P(9)
      P11=P(11)
!
      TTT(1)=P(2)*DFA
      TTT(2)=P(4)*PLG(3,1)
      TTT(3)=P(6)*CD7
      TTT(4)=(P(8) )*CD9
      TTT(5)=(P(10)*PLG(2,1)+P(22)*PLG(4,1))*CD11
      TTT(6)=0.D0
      TTT(7)=(P(14)*PLG(2,2)*CTLOC+P(15)*PLG(2,2)*STLOC)
      TTT(8)=(P(16)*PLG(3,3)+P(18)*PLG(5,3)                             &
     &     +(P(20)*PLG(6,3))*CD11                                       &
     &     )*C2TLOC                                                     &
     &     +(P(17)*PLG(3,3)+P(19)*PLG(5,3)                              &
     &     +(P(21)*PLG(6,3))*CD11                                       &
     &     )*S2TLOC
      TTT(14)=(P(12)*C3TLOC                                             &
     &        +P(25)*S3TLOC)*PLG(4,4)
      IF( ABS ( SW(9) - 1.D0 ) .LT. 0.1D0 )                             &
     & TTT(9)=APDF*(P(23)+P(24)*PLG(3,1))
      IF( ABS( SW(9) - (-1.D0)) .LT. 0.1D0 )                            &
     & TTT(9)=(P(3)*APT(3)+P(5)*PLG(3,1)*APT(3))
!       PARMS NOT USED: 13
      TT=0.D0
      DO 50 I=1,14
!   50 TT=TT+ABS(SW(I))*TTT(I)
       TT=TT+TTT(I)
   50 END DO
      GLOB5L=TT
      RETURN
      END
