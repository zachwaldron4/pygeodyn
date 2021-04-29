!$FINCL
      SUBROUTINE FINCL(NN,KK,HH,FI)
!********1*********2*********3*********4*********5*********6*********7**
! FINCL            83/08/17            8308.0    PGMR - BEN ROSEN
!                                                       D. ROWLANDS
!                                                        (MOD FOR GII)
!
! FUNCTION:  FINCL COMPUTES KAULA'S INCLINATION FUNCTION FOR EACH
!            SOLID EARTH TIDE CONSTITUENT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NN       I    S    DEGREE OF HARMONIC OF TIDAL CONSTITUENT
!   KK       I    A    TIDAL EXPANSION ARGUMENT
!   HH       I    A    TIDAL EXPANSION ARGUMENT
!   FI       O    A    ARRAY CONTAINING VALUES OF INCLINATION FUNCTION
!                      FOR MOON AND SUN
!
! COMMENTS:
!
! RESTRICTIONS:  (1) SUBROUTINE FACBIN MUST BE CALLED FIRST TO
!                    INITIALIZE COMMON CFACT
!                (2) ORDER OF INPUT MUST BE ADJUSTED ET,FOLLOWED BY
!                    ADJUSTED OCEAN TIDES,FOLLOWED BY REST OF ET
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER HH
      DIMENSION KK(1),HH(1),  FI(2,1),PS(31)
      COMMON/CFACT/F21(21),B33(3,3),BN(5,18)
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/TDELEM/XMEAN(2),XNODE(2),OMEGA(2),A(2),XINCL(2),E(2),      &
     &   EPSLON,RMASS,XLONG,TANPSI,THETG,RHO3,CC(3,3),B(2)
      DATA LIMINC/31/
      DATA ZERO/0.D0/,ONE/1.D0/,TWO/2.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!  COMPUTE ALL VALUES OF FI FOR MOON AND THEN FOR SUN
      DO 500 IB=1,2
      C=COS(XINCL(IB))
      S=SQRT(ONE-C*C)
      PS(1)=ONE
      DO 100 N=2,LIMINC
      N1=N-1
      PS(N)=PS(N1)*S
  100 END DO
! LOOP THROUGH ALL EARTH TIDAL CONSTITUENT
      DO 500 ITD1=1,NET
      ITD=ITD1
      IF(ITD1.GT.NETADJ) ITD=ITD1+NOTADJ
      FI(IB,ITD1)=ZERO
      MPO=KK(ITD)+1
      KP2=HH(ITD)+2
      LM=NN-KK(ITD)
      LM2=LM+2
      KC=LM/2
      KC1=KC+1
      ITM=MIN(KC,HH(ITD))+1
      DO 500 IT=1,ITM
      LM2IT=LM2-2*IT
      I2=KP2-IT
      I21=I2+1
      FXS=ZERO
      DO 150 IS=1,MPO
      I1=LM2IT+IS
      IS1=IS-1
      IS2=IS-2
      MISPT=KK(ITD)-IS2
      CPIS1=C**IS1
      FXC=ZERO
      ICM=MIN(I1,I2)
      DO 175 IC=1,ICM
      ITICT=I21-IC
      FACT=ONE
      IF(MOD((IC-KC1),2).NE.0) FACT=-ONE
  175 FXC=FXC+B33(I1,IC)*B33(MISPT,ITICT)*FACT
      FXS=FXS+B33(MPO,IS)*CPIS1*FXC
  150 END DO
      LIT=NN-IT
      IARG1=2*LIT+3
      IARG2=LIT+2
      IARG3=LM2IT+1
      PU2=TWO**(IARG1-1)
      FI(IB,ITD1)=FI(IB,ITD1)+F21(IARG1)*PS(IARG3)*FXS/(F21(IARG2)*     &
     &   PU2*F21(IARG3))
  500 CONTINUE
      RETURN
      END
