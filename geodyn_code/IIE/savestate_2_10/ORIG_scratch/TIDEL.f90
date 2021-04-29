!$TIDEL
      SUBROUTINE TIDEL(IPD)
!********1*********2*********3*********4*********5*********6*********7**
! TIDEL            83/08/16            8308.0    PGMR - D. ROWLANDS
!
! FUNCTION:  CONTROL THE GETTING KEPLARIAN ELEMENTS OF THE
!            MOON&SUN (APPARENT MOTION OF SUN)
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IPD                INDICATES THE SOURCE OF MOON/SUN EPHEMERIS
!                      INFORMATION:
!                          IPD=0  THE INFORMATION IS REQUESTED AT
!                                 INTEGRATION STEP TIME AND WILL
!                                 COME FROM COMMON BLOCK MNSNS.
!                          IPD>0  THE INFORMATION IS REQUESTED AT
!                                 A 12 HR NUTATION INTERPOLATION
!                                 ENPOINT TIME AND WILL COME FROM
!                                 COMMON MNSNI.
!
! COMMENTS: OUTPUT IS THROUGH COMMON BLOCK TDELEM
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION XS(6),XM(6)
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/MNSNI/XMNSNI(6,2,4)
      COMMON/MNSNSP/XMNSNS(7,2)
      COMMON/TDELEM/XMEAN(2),XNODE(2),OMEGA(2),A(2),XINCL(2),E(2),      &
     &   EPSLON,RMASS,XLONG,TANPSI,THETG,RHO3,CC(3,3),B(2)
      DATA ZERO/0.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(IPD.LT.1) GO TO 50
!   LOAD XM&XS FOR THE INTERPOLATION INTERVAL CASE
      DO 10 I=1,6
      XM(I)=XMNSNI(I,1,IPD)
      XS(I)=XMNSNI(I,2,IPD)
   10 END DO
      GO TO 150
   50 CONTINUE
      DO 100 I=1,3
      XM(I)=XMNSNS(I,1)*XMNSNS(4,1)
      XM(I+3)=XMNSNS(I+4,1)
      XS(I)=XMNSNS(I,2)*XMNSNS(4,2)
      XS(I+3)=XMNSNS(I+4,2)
  100 END DO
  150 CONTINUE
      CALL TDEELM(XS,RATIOM(2),A(2),EPSLON,XNODE(2),OMEGA(2),XMEAN(2),  &
     &   E(2))
      XINCL(2)=ZERO
      S=SIN(EPSLON)
      C=COS(EPSLON)
      YTEMP=C*XM(2)+S*XM(3)
      XM(3)=-S*XM(2)+C*XM(3)
      XM(2)=YTEMP
      YTEMP=C*XM(5)+S*XM(6)
      XM(6)=-S*XM(5)+C*XM(6)
      XM(5)=YTEMP
      CALL TDEELM(XM,RATIOM(1),A(1),XINCL(1),XNODE(1),OMEGA(1),XMEAN(1),&
     &   E(1))
      RETURN
      END
