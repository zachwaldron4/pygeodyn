!$GEXC
      SUBROUTINE GEXC(NN,HH,JJ,GE)
!********1*********2*********3*********4*********5*********6*********7**
! GEXC             83/08/17            8308.0    PGMR - BEN ROSEN
!                                                PGMR - D. ROWLANDS
!
! FUNCTION:  GEXC COMPUTES KAULA'S ECCENTRICITY FUNCTION FOR EACH
!            SOLID EARTH TIDAL CONSTITUENT.
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NN       I    S    HARMONIC DEGREE OF SOLID EARTH TIDE
!   HH       I    A    TIDAL EXPANSION ARGUMENT
!   JJ       I    A    TIDAL EXPANSION ARGUMENT
!   GE       O    A    ARRAY CONTAINING ECCENTRICITY FUNCTIONS FOR EACH
!                      TIDAL CONSTITUENT FOR BOTH MOON AND SUN
!
! COMMENTS:
!
!
! RESTRICTIONS:  (1) FACBIN MUST HAVE BEEN CALLED  TO INITIALIZE
!                    COMMON CFACT
!                (2) ASSUMES THAT ORDER OF INPUT IS ADJUSTED ET FOLLOWED
!                    BY ADJUSTED OT FOLLOWED BY REST OF ET
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER HH
      DIMENSION  HH(1),JJ(1),GE(2,1)
      COMMON/CFACT/F21(21),B33(3,3),BN(5,18)
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/TDELEM/XMEAN(2),XNODE(2),OMEGA(2),A(2),XINCL(2),E(2),      &
     &   EPSLON,RMASS,XLONG,TANPSI,THETG,RHO3,CC(3,3),B(2)
      DATA ZERO/0.D0/,ONE/1.D0/,TWO/2.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! COMPUTE ECCENTICITY FUNCTIONS FOR MOON, THEN FOR SUN
!
!
      DO 300 IB=1,2
      EARG=E(IB)
      ETA=SQRT(ONE-EARG*EARG)
      BE=EARG/(ONE+ETA)
      L=NN
! LOOP THROUGH ALL EARTH TIDAL CONSTITUENTS
      DO 300 ITD1=1,NET
      ITD=ITD1
      IF(ITD.GT.NETADJ) ITD=ITD1+NOTADJ
      K=L/2
      IF(HH(ITD)-K)10,10,20
   10 KPP=HH(ITD)
      GO TO 30
   20 KPP=L-HH(ITD)
   30 GE(IB,ITD1)=ZERO
      IF(L.EQ.0) GO TO 300
      GSUM=ZERO
      IF(L-2*HH(ITD)+JJ(ITD)) 40,200,40
   40 GCF=(-ONE)**ABS(JJ(ITD))*(ONE+BE**2)**L*BE**ABS(JJ(ITD))
      DO 150 IK=1,9
      P=ZERO
      QX=ZERO
      IF(HH(ITD)-K) 60,60,50
   50 IQPP=-JJ(ITD)
      GO TO 70
   60 IQPP=JJ(ITD)
   70 IF(IQPP) 80,80,90
   80 IH=IK
      IN=IK-IQPP
      GO TO 100
   90 IH=IK+IQPP
      IN=IK
  100 DO 110 IR=1,IH
      JU=2*L-2* KPP+1
      JL=IH-IR+1
      PX=BN(JU,JL)*(-ONE)**(IR-1)/F21(IR)*( DBLE(L-2*KPP+IQPP)         &
     &*EARG/TWO/BE)                                                     &
     &**(IR-1)
      P=P+PX
  110 END DO
      DO 120 IR=1,IN
      JU=2*KPP+1
      JL=IN-IR+1
      QY=BN(JU,JL)/F21(IR)*( DBLE(L-2*KPP+IQPP)*EARG/TWO/BE)**(IR-1)
      QX=QX+QY
  120 END DO
      GSUM=GSUM+P*QX*BE**(2*IK-2)
  150 END DO
      GE(IB,ITD1)=GCF*GSUM
      GO TO 300
  200 DO 250 ID=1,KPP
      IDD=2*ID-2+L-2*KPP
      EID=(E(IB)/TWO)**IDD
      LID=IDD+1
      GID=B33(L,LID)*B33(LID,ID)*EID/ETA**(2*L-1)
      GE(IB,ITD1)=GE(IB,ITD1)+GID
  250 END DO
  300 CONTINUE
      RETURN
      END
