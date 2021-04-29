!$TRKPC2
      SUBROUTINE TRKPC2(XSM,VSM,UI,OFFSET,FSEC,DR,AA,L1ST,LOFFAJ,II,    &
     &                  ISAT,LALT,NDIMRA,ATROT,DWRKDO,U,WORK,LANTSC,    &
     &                  IANTSC,LWNDI,LWNDO,WPU,ISEQ,IUPDN,ANTTAB)
!********1*********2*********3*********4*********5*********6*********7**
! TRKPC2           02/20/90            8912.0    PGMR- T. WILLIAMS
!
! FUNCTION:    COMPUTE CORRECTIONS TO RANGE DUE TO LOCATION OF
!              TRACKING POINT NOT AT S/C C.G.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSM      I    A    EARTH CENTERED INERTIAL POSITION VECTORS
!   UI       I    A    UNIT INERTIAL TOPOCENTRIC S/C POSITION TRUE OF RE
!                      unit vector from Tn to Sm in TOR
!   OFFSET   I    A    BODY CENTERED FIXED TRACKING POINT
!                      LOCATION
!   FSEC     I    A    FRACTIONAL SECONDS OF FIRST OBSERVATION
!   DR       O    A    CORRECTIONS TO RANGES
!   AA       I    A    DYNAMIC ALLOCATION FOR REAL VARIABLES
!   LANTCT   I    A    LOGICAL ANTENNA CUTOFF
!   IANTSC   I    A    ANTENNA CUT SATELLITE IDS
!   ANTTAB   I    A    ANTENNA PHASE VARIATION TABLE
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/APHASE/NANT_sat,NANTPS_sat(999),KANTPS_sat(999),           &
     &              nant_sta,NANTPS_sta(999),KANTPS_sta(999),           &
     &              NANTPT,  NANTPS(999),    KANTPS(999), NANTMT(99),   &
     &              NNUMMT, NXAPHA
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/GPSZEN/ZENGPS(15,50)
      COMMON/IZNSAT/IZENID(50)
      COMMON/CANTEN/NCUT,NXANTE
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CORI04/KNMP  ,KINDP ,KNDPAR,KNPVCT,KISATN,KISET ,KIANTO,   &
     &              KISATO,KIANTD,KISATD,KISTAD,KISATC,KMJDCG,KISTAT,   &
     &              KMJDEB,KMJDBN,KNDBIN,KNWTDB,KIXPAR,KNDBUF,KSSTNM,   &
     &              KSSTNA,KMBSAT,KMBSTA,KXKEY ,KXVKEY,KXFLAG,KIPNTF,   &
     &              KIPNTS,KNCON ,KKF   ,KIDATB,KNSTLV,KSLVID,KLLBIA,   &
     &              KTMRA1,KTMRA2,KIND1 ,KTARID,KATARD,KYAWID,KXOBLK,   &
     &              KDSCWV,KATRSQ,KATSAT,KKVLAS,KPARTP,KLTMSC,KLTASC,   &
     &              KCTBST,KSTBST,KANCUT,KANGPS,KANTYP,                 &
     &              KANTOF,                                             &
     &              KYSAT, KMBDEG,                                      &
     &              KMBNOD,KMBSST,KBSPLN,KSSPLN,KXIOBS,KIDLAS,KIAVP ,   &
     &              KXNAVP,KNEXCG,KIDEXC,KNREXC,KTELEO,KIKEY ,KIMKEY,   &
     &              KIMBLK,KIMPRT,KCN110,KCN111,KC110,KC111,KTDSAT,     &
     &              KTDANT,NXCI04
      COMMON/CREFMT/REFMT(9)
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)
      COMMON/SPOLY/XSSAT,SNPOLY,XSSATD(200),XSPOLY(6,2000)
      COMMON/YAWNFO/YAWRP(200),YAWR(200),YAWRR(200),YAWBTP(200),        &
     &              YAWBID(200)
!
      DIMENSION XSM(MINTIM,3),UI(NDIMRA,3),OFFSET(3,2),                 &
     &   U(MINTIM,3,3),WORK(MINTIM,5),DR(NM),FSEC(NM)
      DIMENSION UV(3,3)
      DIMENSION SUNXYZ(3),AA(1),II(1)
      DIMENSION OXYZ(3,3,200),VSM(MINTIM,3)
      DIMENSION BETA(200),OA(200),BBIAS(200)
      DIMENSION LPOLY(200),TSTART(200),A0(200),A1(200),A2(200)
      DIMENSION ATROT(3,3,NM),TEMP(3,3)
      DIMENSION DWRKDO(NM,3)
      DIMENSION LANTCT(1),IANTSC(1)
      DIMENSION WPU(NM,3,2),VWND(3,2)
      DIMENSION ANTTAB(1)
!
      DATA ZERO/0.0D0/,ONE/1.D0/
      DATA CUT/.99863D0/
      DATA L1STE/.TRUE./

      LOGICAL :: L_sat_phc
      LOGICAL :: L_ant_phc
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!         print*, 'dbg in trkpc2'
      IF(L1STE.AND.IUPDN.GT.2) THEN
        L1STE=.FALSE.
        WRITE(6,60000)
        WRITE(6,60001)
        WRITE(6,60002)
        STOP
      ENDIF
      LWNDO=LWNDI
       IF(LWNDI) THEN
          IF(IUPDN.EQ.1) THEN
            VWND(1,1)=VHIGH(1,1,ISEQ)
            VWND(2,1)=VHIGH(2,1,ISEQ)
            VWND(3,1)=VHIGH(3,1,ISEQ)
            VWND(1,2)=VHIGH(1,2,ISEQ)
            VWND(2,2)=VHIGH(2,2,ISEQ)
            VWND(3,2)=VHIGH(3,2,ISEQ)
          ELSE
            VWND(1,1)=VLOW(1,1,ISEQ)
            VWND(2,1)=VLOW(2,1,ISEQ)
            VWND(3,1)=VLOW(3,1,ISEQ)
            VWND(1,2)=VLOW(1,2,ISEQ)
            VWND(2,2)=VLOW(2,2,ISEQ)
            VWND(3,2)=VLOW(3,2,ISEQ)
          ENDIF
       ENDIF
! INITIALIZE THE U ARRAY
      DO 100 I=1,MINTIM
      DO 100 J=1,3
      DO 100 K=1,3
      U(I,J,K)=0.D0
  100 CONTINUE
      ISATID=II(KISATN-1+ISAT)
      IGPSBK=MOD(ISATID,10)
      !if( igpsbk == 3 ) igpsbk = 2   ! tct 20151116
      !if( igpsbk >= 3 ) igpsbk = 3   ! jjm 20130617
      LSPEC=.FALSE.
      TESTB=ABS(YAWBTP(ISAT))
      IF(TESTB.GT..01D0) LSPEC=.TRUE.
      !IF(IGPSBK.EQ.3) LSPEC=.TRUE.
      IF(IGPSBK.EQ.4 .OR. IGPSBK.EQ.5) LSPEC=.TRUE.
      TESTB=YAWBTP(ISAT)
      DO 4 I=1,NM
      LPOLY(I)=.FALSE.
    4 END DO
!
!
      IF(LSPEC.AND.XSSATD(ISAT).GT.0.D0) THEN
       IS1=0
        NPOLY=SNPOLY+.05D0
        DO 6 I=1,NPOLY
        JSATID=XSPOLY(1,I)+.05D0
        IF(JSATID.EQ.ISATID) THEN
          IF(IS1.EQ.0) IS1=I
        ELSE
          IF(IS1.GT.0) THEN
            IS2=I-1
            GO TO 7
           ENDIF
        ENDIF
    6   CONTINUE
        IF(IS1.GT.0) IS2=NPOLY
    7   CONTINUE
        IF(IS1.EQ.0) GO TO 14
!
        JL=IS1
        DO 10 I=1,NM
        TIME=DBLE(MJDSBL)+FSEC(I)
        JS1=JL
        DO 9 J=JS1,IS2
        IF(TIME.GE.XSPOLY(2,J).AND.TIME.LE.XSPOLY(3,J)) THEN
          JL=JS1
          LPOLY(I)=.TRUE.
          TSTART(I)=XSPOLY(2,J)
          A0(I)=XSPOLY(4,J)
          A1(I)=XSPOLY(5,J)
          A2(I)=XSPOLY(6,J)
          GO TO 10
         ENDIF
    9   CONTINUE
   10   CONTINUE
!
      ENDIF
!
!
!
   14 CONTINUE
      IOFFPT=1
      I1=1
!
! SAT SUN VECTOR INTO WORK
!
      DO 15 I=1,NM
      RX=SQRT(XSM(I,1)*XSM(I,1)+XSM(I,2)*XSM(I,2)+XSM(I,3)*XSM(I,3))
      OXYZ(1,1,I)=XSM(I,1)/RX
      OXYZ(2,1,I)=XSM(I,2)/RX
      OXYZ(3,1,I)=XSM(I,3)/RX
      OXYZ(1,3,I)=XSM(I,2)*VSM(I,3)-XSM(I,3)*VSM(I,2)
      OXYZ(2,3,I)=XSM(I,3)*VSM(I,1)-XSM(I,1)*VSM(I,3)
      OXYZ(3,3,I)=XSM(I,1)*VSM(I,2)-XSM(I,2)*VSM(I,1)
      RZ=SQRT(OXYZ(1,3,I)*OXYZ(1,3,I)+OXYZ(2,3,I)*OXYZ(2,3,I)          &
     &        +OXYZ(3,3,I)*OXYZ(3,3,I))
      OXYZ(1,3,I)=OXYZ(1,3,I)/RZ
      OXYZ(2,3,I)=OXYZ(2,3,I)/RZ
      OXYZ(3,3,I)=OXYZ(3,3,I)/RZ
      OXYZ(1,2,I)=OXYZ(2,3,I)*OXYZ(3,1,I)-OXYZ(3,3,I)*OXYZ(2,1,I)
      OXYZ(2,2,I)=OXYZ(3,3,I)*OXYZ(1,1,I)-OXYZ(1,3,I)*OXYZ(3,1,I)
      OXYZ(3,2,I)=OXYZ(1,3,I)*OXYZ(2,1,I)-OXYZ(2,3,I)*OXYZ(1,1,I)
      CALL BUFEAR(MJDSBL,FSEC(I),MJDSBL,FSEC(I),I1,MJDR,FSECR,LRECAL,AA,&
     &              II)
      CALL PLANPO(MJDSBL,FSEC(I),.FALSE.,.FALSE.,AA,II)
      SUNXYZ(1)=REFMT(1)*BDSTAT(1,8)+REFMT(2)*BDSTAT(2,8)               &
     &         +REFMT(3)*BDSTAT(3,8)
      SUNXYZ(2)=REFMT(4)*BDSTAT(1,8)+REFMT(5)*BDSTAT(2,8)               &
     &         +REFMT(6)*BDSTAT(3,8)
      SUNXYZ(3)=REFMT(7)*BDSTAT(1,8)+REFMT(8)*BDSTAT(2,8)               &
     &         +REFMT(9)*BDSTAT(3,8)
!
      RSX2=SUNXYZ(1)*SUNXYZ(1)+SUNXYZ(2)*SUNXYZ(2)+SUNXYZ(3)*SUNXYZ(3)
      RSX=SQRT(RSX2)
      COSBC=(OXYZ(1,3,I)*SUNXYZ(1)+OXYZ(2,3,I)*SUNXYZ(2)                &
     &      +OXYZ(3,3,I)*SUNXYZ(3))/RSX
      BETA(I)=.5D0*ACOS(-1.D0)-ACOS(COSBC)
!
! PROJECT SUN INTO XY PLANE OF OZYZ SYSTEM
! SAT POS VECTOR IS (1,0,0) IN THIS SYSTEM
      SUN1=OXYZ(1,1,I)*SUNXYZ(1)+OXYZ(2,1,I)*SUNXYZ(2)                  &
     &    +OXYZ(3,1,I)*SUNXYZ(3)
      SUN2=OXYZ(1,2,I)*SUNXYZ(1)+OXYZ(2,2,I)*SUNXYZ(2)                  &
     &    +OXYZ(3,2,I)*SUNXYZ(3)
! GET UNIT MIDNIGHT VECTOR
      RSX=SQRT(SUN1*SUN1+SUN2*SUN2)
      SUN1=-SUN1/RSX
      SUN2=-SUN2/RSX
! GET ORBIT ANGLE
      IF(SUN2.GT.0.D0) THEN
         OA(I)=2.D0*ACOS(-1.D0)-ACOS(SUN1)
      ELSE
         OA(I)=ACOS(SUN1)
      ENDIF
!
      WORK(I,1)=SUNXYZ(1)-XSM(I,1)
      WORK(I,2)=SUNXYZ(2)-XSM(I,2)
      WORK(I,3)=SUNXYZ(3)-XSM(I,3)
      RD1=SQRT(WORK(I,1)*WORK(I,1)+WORK(I,2)*WORK(I,2)                 &
     &         +WORK(I,3)*WORK(I,3))
      RD2=SQRT(XSM(I,1)*XSM(I,1)+XSM(I,2)*XSM(I,2)                     &
     &         +XSM(I,3)*XSM(I,3))
!
      COSE=-WORK(I,1)*XSM(I,1)-WORK(I,2)*XSM(I,2)-WORK(I,3)*XSM(I,3)
      COSE=COSE/(RD1*RD2)
      SINE=SIN(ACOS(COSE))
! FORMULA BELOW (INVOLVING ARCSIN) IS SINGULAR WHEN ANGLE E
! IS LESS THAN .5013 DEGREES (OR SINE IS LESS THAN .00875)
      IF(SINE.LT..00876D0) SINE=.00876D0
!


      IF(.NOT.LSPEC) GO TO 15
!
! FOR YAW BIAS NOT 0,-1,1,-2,2 THEN BIAS IS BIAS VALUE
      XBIAS=TESTB
!
! BIAS OF 1 IMPLIES NORMAL BIAS, -1 IMPLIES ANTI-NORMAL BIAS
      IF(TESTB.LT.1.01D0.AND.TESTB.GT.0.99D0) XBIAS=SIGN(0.5D0,BETA(I))
      RNBETA=-BETA(I)
      IF(TESTB.GT.-1.01D0.AND.TESTB.LT.-0.99D0) XBIAS=SIGN(0.5D0,RNBETA)
!
! BIAS OF +-1 IMPLIES BIAS OF SAME SIGN REGARDLESS OF BETA
      IF(TESTB.LT.-1.99D0.AND.TESTB.GT.-2.01D0) XBIAS=-0.5D0
      IF(TESTB.GT.1.99D0.AND.TESTB.LT.2.01D0) XBIAS=0.5D0
!
      BBIAS(I)=ASIN(.0175D0*XBIAS/SINE)

   15 END DO
!
!
!
      DO 20 I=1,NM
! NORMALIZE SAT-SUN VECTOR
      WORK(I,4)=SQRT(WORK(I,1)*WORK(I,1)+WORK(I,2)*WORK(I,2)           &
     &               +WORK(I,3)*WORK(I,3))
      WORK(I,1)=WORK(I,1)/WORK(I,4)
      WORK(I,2)=WORK(I,2)/WORK(I,4)
      WORK(I,3)=WORK(I,3)/WORK(I,4)
! FILL Z VECTOR
      WORK(I,4)=SQRT(XSM(I,1)*XSM(I,1)+XSM(I,2)*XSM(I,2)               &
     &               +XSM(I,3)*XSM(I,3))
      U(I,1,3)=-XSM(I,1)/WORK(I,4)
      U(I,2,3)=-XSM(I,2)/WORK(I,4)
      U(I,3,3)=-XSM(I,3)/WORK(I,4)
! GET COS OF SUN-SAT-EARTH ANGLE
      WORK(I,5)=ABS(WORK(I,1)*U(I,1,3)+WORK(I,2)*U(I,2,3)              &
     &              +WORK(I,3)*U(I,3,3))
! FILL Y VECTOR
      U(I,1,2)=U(I,2,3)*WORK(I,3)-U(I,3,3)*WORK(I,2)
      U(I,2,2)=U(I,3,3)*WORK(I,1)-U(I,1,3)*WORK(I,3)
      U(I,3,2)=U(I,1,3)*WORK(I,2)-U(I,2,3)*WORK(I,1)
      WORK(I,4)=SQRT(U(I,1,2)*U(I,1,2)+U(I,2,2)*U(I,2,2)               &
     &               +U(I,3,2)*U(I,3,2))
      U(I,1,2)=U(I,1,2)/WORK(I,4)
      U(I,2,2)=U(I,2,2)/WORK(I,4)
      U(I,3,2)=U(I,3,2)/WORK(I,4)
! FILL X VECTOR
      U(I,1,1)=U(I,2,2)*U(I,3,3)-U(I,3,2)*U(I,2,3)
      U(I,2,1)=U(I,3,2)*U(I,1,3)-U(I,1,2)*U(I,3,3)
      U(I,3,1)=U(I,1,2)*U(I,2,3)-U(I,2,2)*U(I,1,3)
!
!  REDO THE X AND Y VECTORS IF UNDER NEW YAW ALG
!
      IF(LSPEC) THEN
!
         IF(.NOT.LPOLY(I)) THEN
            YAB1=ATAN2(-TAN(BETA(I)),SIN(OA(I)))
            YAB2=YAB1+BBIAS(I)
         ELSE
            TL=DBLE(MJDSBL)+FSEC(I)-TSTART(I)
            YAB2=A0(I)+A1(I)*TL+A2(I)*TL*TL
         ENDIF
!
         YAB1R=YAB2

!
! FILL Y MATRIX
! 1ST COLUMN YAW ORIGIN IS O2
! 3RD COLUMN IS U3
! 2ND COLUMN IS ZxX
         Y1=U(I,2,3)*OXYZ(3,2,I)-U(I,3,3)*OXYZ(2,2,I)
         Y2=U(I,3,3)*OXYZ(1,2,I)-U(I,1,3)*OXYZ(3,2,I)
         Y3=U(I,1,3)*OXYZ(2,2,I)-U(I,2,3)*OXYZ(1,2,I)
!
!
         COSY=COS(YAB1R)
         SINY=SIN(YAB1R)
! REFILL X & Y VECTORS
!
          U(I,1,1)=OXYZ(1,2,I)*COSY - OXYZ(1,3,I)*SINY
          U(I,2,1)=OXYZ(2,2,I)*COSY - OXYZ(2,3,I)*SINY
          U(I,3,1)=OXYZ(3,2,I)*COSY - OXYZ(3,3,I)*SINY
!
          U(I,1,2)=-OXYZ(1,2,I)*SINY - OXYZ(1,3,I)*COSY
          U(I,2,2)=-OXYZ(2,2,I)*SINY - OXYZ(2,3,I)*COSY
          U(I,3,2)=-OXYZ(3,2,I)*SINY - OXYZ(3,3,I)*COSY
!
          U(I,1,3)=-OXYZ(1,1,I)
          U(I,2,3)=-OXYZ(2,1,I)
          U(I,3,3)=-OXYZ(3,1,I)
!
          !IF(IGPSBK.EQ.3) THEN
          IF(IGPSBK.EQ.4 .OR. IGPSBK.EQ.5) THEN
             U(I,1,1)=-U(I,1,1)
             U(I,2,1)=-U(I,2,1)
             U(I,3,1)=-U(I,3,1)
!
             U(I,1,2)=-U(I,1,2)
             U(I,2,2)=-U(I,2,2)
             U(I,3,2)=-U(I,3,2)
          ENDIF
!
!
      ENDIF
!
!  END NEW ALG
!
! CORRECT HERE FOR ROLL PITCH AND YAW
      DO 7320 IJ=1,3
      DO 7310 J=1,3
      TEMP(IJ,J)=U(I,IJ,J)
 7310 END DO
 7320 END DO
      CALL MATPRD(TEMP,ATROT(1,1,I),UV,3,3,3)
      DO 7330 IL=1,3
      DO 7340 IM=1,3
      U(I,IL,IM)=UV(IL,IM)
 7340 END DO
 7330 END DO
!
! FILL THE WORKING OFFSET ARRAY
      WORK(I,1)=0.D0
      WORK(I,2)=0.D0
      WORK(I,3)=0.D0
   20 END DO
!
! FILL X AND Y COMPONENTS OF WORKING OFFSET IF SUN-SAT-EARTH ANG>3 DEG
!
      DO 30 I=1,NM
      IF(WORK(I,5).GT.CUT) GO TO 30
      WORK(I,1)=OFFSET(1,IOFFPT)
      WORK(I,2)=OFFSET(2,IOFFPT)
      WORK(I,3)=ONE
   30 END DO
      IF(LOFFAJ) THEN
       IF(L1ST) THEN
          DO 35 I=1,NM
          DWRKDO(I,1)=UI(I,1)*U(I,1,1)*WORK(I,3)                        &
     &               +UI(I,2)*U(I,2,1)*WORK(I,3)                        &
     &               +UI(I,3)*U(I,3,1)*WORK(I,3)
          DWRKDO(I,2)=UI(I,1)*U(I,1,2)*WORK(I,3)                        &
     &               +UI(I,2)*U(I,2,2)*WORK(I,3)                        &
     &               +UI(I,3)*U(I,3,2)*WORK(I,3)
          DWRKDO(I,3)=UI(I,1)*U(I,1,3)                                  &
     &               +UI(I,2)*U(I,2,3)                                  &
     &               +UI(I,3)*U(I,3,3)
   35     CONTINUE
       ELSE
         DO 40 I=1,NM
          DWRKDO(I,1)=-UI(I,1)*U(I,1,1)*WORK(I,3)                       &
     &                -UI(I,2)*U(I,2,1)*WORK(I,3)                       &
     &                -UI(I,3)*U(I,3,1)*WORK(I,3)
          DWRKDO(I,2)=-UI(I,1)*U(I,1,2)*WORK(I,3)                       &
     &                -UI(I,2)*U(I,2,2)*WORK(I,3)                       &
     &                -UI(I,3)*U(I,3,2)*WORK(I,3)
          DWRKDO(I,3)=-UI(I,1)*U(I,1,3)                                 &
     &                -UI(I,2)*U(I,2,3)                                 &
     &                -UI(I,3)*U(I,3,3)
   40     CONTINUE
       ENDIF
      ENDIF
       IF(LWNDI) THEN
          DO I=1,NM
          DO JJ=1,2
          DO III=1,3
            WPU(I,III,JJ)=U(I,III,1)*VWND(1,JJ)                         &
     &                   +U(I,III,2)*VWND(2,JJ)                         &
     &                   +U(I,III,3)*VWND(3,JJ)
          ENDDO
          ENDDO
          ENDDO
       ENDIF
!
      IF(LALT) THEN
        DO 7400 N=1,NM
        UI(N,1)=U(N,1,3)
        UI(N,2)=U(N,2,3)
        UI(N,3)=U(N,3,3)
 7400   CONTINUE
       RETURN
      ENDIF
! COMPUTE OFFSET CORRECTION TO RANGE
      DO 45 I=1,NM
      WORK(I,3)=U(I,1,1)*WORK(I,1)+U(I,1,2)*WORK(I,2)                   &
     &         +U(I,1,3)*OFFSET(3,IOFFPT)
      WORK(I,4)=U(I,2,1)*WORK(I,1)+U(I,2,2)*WORK(I,2)                   &
     &         +U(I,2,3)*OFFSET(3,IOFFPT)
      WORK(I,5)=U(I,3,1)*WORK(I,1)+U(I,3,2)*WORK(I,2)                   &
     &         +U(I,3,3)*OFFSET(3,IOFFPT)
      DR(I)=UI(I,1)*WORK(I,3)+UI(I,2)*WORK(I,4)+UI(I,3)*WORK(I,5)
   45 END DO
       !print*, 'ddd in trkpc2: ui',ui(1,1),ui(1,2),ui(1,3)


      !--------------------------------------------------------------

      ! CALCULATE THE PHASE CENTER CORRECTIONS FOR GPS SATELLITES
      CALL DRVGPSPHC(AA, II, NM, ISEQ, ISATID, L1ST, UI, XSM, DR)

      RETURN

!------------------------------------------------------------------------------
! NO EXTERNAL PHC TABLE WAS FOUND FOR THIS GPS SATELLITE. USE THE
! HARD CODED INTERNAL NUMBERS

!!! 105  CONTINUE
!!!      write(6,*)'trkpc2: aft 105  HARD CODED PHC '
!!!!       print*, 'ddd in trkpc2'
!!!      DO 110 I=1,50
!!!      IPT=I
!!!      IF(ISATID.EQ.IZENID(I)) GO TO 200
!!! 110  CONTINUE
!!!      RETURN
!!! 200  CONTINUE
!!!      DO 300 I=1,NM
!!!      R1=XSM(I,1)*XSM(I,1)+XSM(I,2)*XSM(I,2)+XSM(I,3)*XSM(I,3)
!!!      R1=SQRT(R1)
!!!      R2=UI(I,1)*UI(I,1)+UI(I,2)*UI(I,2)+UI(I,3)*UI(I,3)
!!!      R2=SQRT(R2)
!!!      DOT=XSM(I,1)*UI(I,1)+XSM(I,2)*UI(I,2)+XSM(I,3)*UI(I,3)
!!!      DOT=ABS(DOT/(R1*R2))
!!!      ANG=ACOS(DOT)/DEGRAD
!!!      IANG=ANG
!!!      IANG1=IANG+1
!!!      IANG2=IANG+2
!!!      IF(IANG2.GT.15) THEN
!!!        DEL=ZENGPS(15,IPT)
!!!        GO TO 290
!!!      ENDIF
!!!      FRAC=ANG-DBLE(IANG)
!!!      DEL=ZENGPS(IANG1,IPT)+FRAC*(ZENGPS(IANG2,IPT)-ZENGPS(IANG1,IPT))
!!! 290  CONTINUE
!!!      IF(L1ST) THEN
!!!         DR(I)=DR(I)+DEL
!!!      ELSE
!!!         DR(I)=DR(I)-DEL
!!!      ENDIF
!!!      if( i == 1)write(6,*)'trkpc2: HARD CODED PHC DEL,DR(1) ',DEL,DR(1)
!!! 300  CONTINUE
!------------------------------------------------------------------------------

      RETURN

60000 FORMAT(' EXECUTION TERMINATING')
60001 FORMAT(' TRKPC2 CALLED FOR A LOW USER SATELLITE')
60002 FORMAT(' THIS WILL MESS UP THE PHASE WIND UP CORRECTION')
      END
