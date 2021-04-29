!$TRKMO
      SUBROUTINE TRKMO(XSM,VSM,UI,OFFSET,FSECRA,RA,DR,ANTENA,AA,II,     &
     &                 LALT,NDIMRA,ATROT,LANTCT,IANTSC,ISATID,DWRKDO,   &
     &                 LOFFAJ)
!********1*********2*********3*********4*********5*********6*********7**
! TRKMO            93/03/30            9212.4    PGMR - ANDREW MARSHALL
!
!
! FUNCTION:  COMPUTE CORRECTIONS TO RANGE DUE TO LOCATION OF
!            TRACKING POINT AND S/C C.G. NOT AT BODY-FIXED
!            ORIGIN FOR MO
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSM      I         MARS CENTERED INERTIAL POSITION VECTORS
!   VSM      I         MARS CENTERED INERTIAL VELOCITY VECTORS
!   UI       I         UNIT TOR TRACKING STATION TO S/C VECTOR
!   OFFSET   I         BODY CENTERED FIXED TRACKING POINT LOCATION
!   FSECRA   I    A    ANTENNA CORR. TIME SINCE BLOCK START
!   RA            A    OFFSET LOCATION MAPPED INTO INERTIAL COORD.
!   DR       O         CORRECTIONS TO RANGES
!   ANTENA   I    A    ANTENNA PHASE CENTER VECTOR IN ANTENNA COORD. SYS
!   LANTCT   I    A    LOGICAL ANTENNA CUTOFF
!   IANTSC   I    A    ANTENNA CUT SATELLITE IDS
!   ISATID   I    S    SATELLITE ID
!
!
! REFERENCES:
!          Moyer, T.D., "Offset from Center of Mass to High Gain Antenna
!          Phase Center for Mars Observer", JPL Interoffice Memorandum
!          314.5-1617, May 1, 1992.
!
!          Sirlin, S.W. "An algorithm for Correcting Mars Observer Doppl
!          Measurements, JPL Engineering Memo EM 343-1223, March 19, 199
!
! COMMENTS:
!            TOD = GEODYN TRUE OF DATE INERTIAL FRAME
!                  rotate to satellite alongtrack, crosstrack, radial sy
!            SBF = SATELLITE BODY-FIXED FRAME
!                  rotate to gimbal frame (assumes nominal HGA deploymen
!                  gimang=45deg)
!            GCS = GIMBAL COORDIANTE SYSTEM
!                  apply rotations about both gimbal axes
!            ACS = ANTENNA COORDINATE SYSTEM
!
!            SA  = SOLAR ARRAY BODY-FIXED FRAME
!
! Note that this model assumes that the spacecraft is geocentrically
! pointing.  In reality, however, the s/c points geodetically based on t
! horizon sensors.  If one assumes as much a 0.1 deg pointing error and
! uses the 6m moment arm, this results in a 1cm error in the location of
! HGA.  For a Doppler measurement, this effect can be ignored.
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
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
      COMMON/COFFST/JXYZOF(2),JSADLY(2),JSTDLY(2),JXYZCG(2,2),          &
     &              MJDFCG(2,2),JXYOF2(2),JEXTOF(2),JXYOF3(2)
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CORA01/KFSEC0,KFSECB,KFSEC ,KFSECV,KH    ,KHV   ,KCTOL ,   &
     &              KRSQ  ,KVMATX,KCPP  ,KCPV  ,KCCP  ,KCCV  ,KCCPV ,   &
     &              KCCVV ,KXPPPP,KX    ,KPX   ,KSUMX ,KXDDOT,KSUMPX,   &
     &              KPXDDT,KAB   ,KPN   ,KAORN ,KSINLM,KCOSLM,KTANPS,   &
     &              KCRPAR,KVRARY,KXM   ,KXNP1 ,KXPRFL,KXM2  ,KXNNP1,   &
     &              KWRK  ,KFI   ,KGE   ,KB0DRG,KBDRAG,KAPGM ,KAPLM ,   &
     &              KCN   ,KSN   ,KSTID ,KTIDE ,KSTDRG,KSTSRD,KSTACC,   &
     &              KLGRAV,KGM   ,KAE   ,KFPL  ,KFEQ  ,KPLNPO,KPLNVL,   &
     &              KXEPOC,KCD   ,KCDDOT,KCR   ,KGENAC,KACN  ,KASN  ,   &
     &              KTHDRG,KCKEP ,KCKEPN,KXNRMZ,KXNRMC,KFSCEP,KFSCND,   &
     &              KAREA ,KXMASS,KRMSPO,KTCOEF,KTXQQ ,KTIEXP,KTXMM ,   &
     &              KTXLL1,KTXSN1,KTS2QQ,KT2M2H,KT2MHJ,KTXKK ,KTSCRH,   &
     &              KPXPK ,KAESHD,KCSAVE,KSSAVE,KCGRVT,KSGRVT,KXDTMC,   &
     &              KDNLT ,KTXSN2,KTNORM,KTWRK1,KTWRK2,KUNORM,KAERLG,   &
     &              KSINCO,KPARLG,KCONST,KBFNRM,KTDNRM,KCSTHT,KTPSTR,   &
     &              KTPSTP,KTPFYW,KPLMGM,KTPXAT,KEAQAT,KEAFSS,KEAINS,   &
     &              KACS  ,KECS  ,KSOLNA,KSOLNE,KSVECT,KSFLUX,KFACTX,   &
     &              KFACTY,KADIST,KGEOAN,KPALB ,KALBCO,KEMMCO,KCNAUX,   &
     &              KSNAUX,KPPER ,KACOSW,KBSINW,KACOFW,KBCOFW,KANGWT,   &
     &              KWT   ,KPLNDX,KPLANC,KTGACC,KTGDRG,KTGSLR,KWTACC,   &
     &              KWTDRG,KWTSLR,KTMACC,KTMDRG,KTMSLR,KATTUD,KDYACT,   &
     &              KACCBT,KACPER,KXDDNC,KXDDAO,KXNC  ,KXPPNC,KSMXNC,   &
     &              KXDDTH,KPDDTH,KXSSBS,KCPPNC,KEXACT,KXACIN,KXACOB,   &
     &              KPXHDT,KTPXTH,KPACCL,KTXSTA,KDELXS,KSMRNC,KPRX  ,   &
     &              KSMRNP,KDSROT,KXUGRD,KYUGRD,KZUGRD,KSUMRC,KXDDRC,   &
     &              KTMOS0,KTMOS, KTMOSP,KSMXOS,KSGTM1,KSGTM2,KSMPNS,   &
     &              KXGGRD,KYGGRD,KZGGRD,KXEGRD,KYEGRD,KZEGRD,KSSDST,   &
     &              KSDINS,KSDIND,KSSDSR,KSSDDG,KTATHM,KTAINS,KTAFSS,   &
     &              KSRAT ,KTRAT ,KHLDV ,KHLDA1,KHLDA4,KHLDA7,KQAST1,   &
     &              KQAST2,KQAST3,KQAST4,KQAST5,KQAST6,NXCA01
      COMMON/CORI01/KMJDS0,KMJDSB,KMJDSC,KMJDSV,KIBACK,KIBAKV,KIORDR,   &
     &              KIORDV,KNOSTP,KNOCOR,KNSAT ,KN3   ,KNEQN ,KNEQN3,   &
     &              KNH   ,KNHV  ,KNSTPS,KNSTPV,KICPP ,KICPV ,KICCP ,   &
     &              KICCV ,KICCPV,KICCVV,KISUMX,KIXDDT,KISMPX,KIPXDD,   &
     &              KNMAX ,KNTOLD,KNTOLO,KICNT ,KISNT ,KMM   ,KKK   ,   &
     &              KJJ   ,KHH   ,KIBDY ,KSIGN1,KSIGN2,KLL   ,KQQ   ,   &
     &              KIORFR,KIPDFR,KITACC,KJSAFR,KJSPFR,KMJDEP,KMJDND,   &
     &              KNVSTP,KNSTRN,KNDARK,KTIPPT,KTJBDY,                 &
     &              KICNTA,KISNTA,KISHDP,KIPTC ,KIPTS ,KIGTSR,KIXTMC,   &
     &              KTIPTT,KTNOSD,KTQNDX,KTLL1 ,KTJJBD,KTITDE,KTCNTR,   &
     &              KTNN  ,KITACX,KNMOVE,KPANEL,KPLPTR,KNADAR,KNADSP,   &
     &              KNADDF,KNADEM,KNADTA,KNADTC,KNADTD,KNADTF,KNADTX,   &
     &              KITPMD,KSCATT,KITPAT,KILTPX,KEASBJ,KEANMP,KEANAN,   &
     &              KEAPMP,KEAPAN,KEAMJS,KEAPPP,KEAAAA,KICNTT,KISNTT,   &
     &              KTPGRC,KTPGRS,KTPC  ,KTPS  ,KALCAP,KEMCAP,KNSEG ,   &
     &              KICNTP,KISNTP,KNRDGA,KNRDDR,KNRDSR,KIRDGA,KIRDRG,   &
     &              KIRSLR,KSTRTA,KSTRTD,KSTRTS,KDYNPE,KACCPE,KIBCKN,   &
     &              KNRAT ,KIXDDN,KISMXN,KDXDDN,KDSMXN,KICPPN,KACSID,   &
     &              KNEQNH,KHRFRC,KPTFBS,KPTFSB,KIPXDA,KIACCP,KXSTAT,   &
     &              KPXST ,KSALST,KMAPLG,KNMBUF,KSTEPS,KSGMNT,KSATIN,   &
     &              KMEMST,KNEQNI,KBUFIN,KWEMGA,KWEMDR,KTPATS,KTANMP,   &
     &              KTAPPP,KTAMJS,KTASID,KGPSID,KNSSVA,KPNALB,KBRAX1,   &
     &              KBRAX2,KBRAX3,NXCI01
      COMMON/CREFMT/REFMT(9)
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)
      DIMENSION XSM(MINTIM,3),VSM(MINTIM,3),OFFSET(3,2),                &
     &   RA(NDIMRA,3),DR(NM),FSECRA(NM),UI(NDIMRA,3)
      DIMENSION BDFOFF(3)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3)
      DIMENSION GCS(3,3), ACS(3,3),TEMP(3,3),ANTOFF(3),ANTENA(3)
      DIMENSION AA(1),II(1)
      DIMENSION ATROT(3,3,NM),TOTROT(3,3)
      DIMENSION EARTH(3),SUNV(3),XSAT(3),VSAT(3),IDUM(1),DUM(1)
      DIMENSION LANTCT(1),IANTSC(1)
      DIMENSION DWRKDO(NM,3)
! PARAMETER STATEMENT NOT VALID FOR ARRAY IN SUBROUTINE ARG. LIST.
! IT REMAINS HERE TO SHOW THAT THIS VALUE IS SET IN THIS ROUTINE IN THIS
! RELEASE.  FUTURE VERSIONS WILL GET THIS INFO FROM 2S INPUT
      DATA ZERO/0.0D0/
      DATA ONE/1.0D0/
      DATA L1ST/.TRUE./
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
      IF(L1ST.AND.XGPSTB.GT.0.D0) THEN
        L1ST=.FALSE.
        WRITE(6,60000)
        WRITE(6,60001)
        WRITE(6,60002)
        WRITE(6,60003)
        WRITE(6,60004)
        WRITE(6,60005)
        WRITE(6,60006)
      ENDIF
!
      LGIMB=.FALSE.
      AD2=ANTENA(1)*ANTENA(1)+ANTENA(2)*ANTENA(2)+ANTENA(3)*ANTENA(3)
      IF(AD2.GT..01D0) LGIMB=.FALSE.
! ROTATE ANTENNA PHASE CENTER VECTOR(r(HA)) TO BODY-FIXED COOR
      DO 300 I=1,NM
!
! COMPUTE THE ROTATION FROM THE SBF TO INERTIAL FRAME WHERE
!         ZTOD = INERTIAL VECTOR FROM S/C TO CENTER OF MARS
!         YTOD = INERTIAL VECTOR NORMAL TO  ORBIT PLANE (X = Z X VSAT)
!
      CALL PLANPO(MJDSBL,FSECRA(I),.FALSE.,.FALSE.,AA,II)
      EARTH(1)=REFMT(1)*BDSTAT(1,9)+REFMT(2)*BDSTAT(2,9)                &
     &        +REFMT(3)*BDSTAT(3,9)
      EARTH(2)=REFMT(4)*BDSTAT(1,9)+REFMT(5)*BDSTAT(2,9)                &
     &        +REFMT(6)*BDSTAT(3,9)
      EARTH(3)=REFMT(7)*BDSTAT(1,9)+REFMT(8)*BDSTAT(2,9)                &
     &        +REFMT(9)*BDSTAT(3,9)
      SUNV(1)=REFMT(1)*BDSTAT(1,8)+REFMT(2)*BDSTAT(2,8)             &
     &      +REFMT(3)*BDSTAT(3,8)
      SUNV(2)=REFMT(4)*BDSTAT(1,8)+REFMT(5)*BDSTAT(2,8)             &
     &      +REFMT(6)*BDSTAT(3,8)
      SUNV(3)=REFMT(7)*BDSTAT(1,8)+REFMT(8)*BDSTAT(2,8)             &
     &      +REFMT(9)*BDSTAT(3,8)
      XSAT(1)=XSM(I,1)
      XSAT(2)=XSM(I,2)
      XSAT(3)=XSM(I,3)
      VSAT(1)=VSM(I,1)
      VSAT(2)=VSM(I,2)
      VSAT(3)=VSM(I,3)
      CALL MGSAT1(MJDSBL,FSECRA(I),XSAT,VSAT,DUM,DUM,DUM,DUM,           &
     &            DUM,DUM,DUM,1,0,.FALSE.,IDUM,DUM,1,IDUM,DUM,          &
     &            AA,1,EARTH,SUNV,TOTROT)
!  ADD IN THE EFFECT OF ATTITUDE PARAMTERS
      DO J=1,3
!      TEMP(1,J)=XTOD(J)
!      TEMP(2,J)=YTOD(J)
!      TEMP(3,J)=ZTOD(J)
      TEMP(1,J)=TOTROT(J,1)
      TEMP(2,J)=TOTROT(J,2)
      TEMP(3,J)=TOTROT(J,3)
      ENDDO
      CALL MATPRD(TEMP,ATROT(1,1,I),TOTROT,3,3,3)
      DO J=1,3
      XTOD(J)=TOTROT(1,J)
      YTOD(J)=TOTROT(2,J)
      ZTOD(J)=TOTROT(3,J)
      ENDDO

      IF(LOFFAJ) THEN
         DWRKDO(I,1)=UI(I,1)*TOTROT(1,1)                                &
     &              +UI(I,2)*TOTROT(2,1)                                &
     &              +UI(I,3)*TOTROT(3,1)
         DWRKDO(I,2)=UI(I,1)*TOTROT(1,2)                                &
     &              +UI(I,2)*TOTROT(2,2)                                &
     &              +UI(I,3)*TOTROT(3,2)
         DWRKDO(I,3)=UI(I,1)*TOTROT(1,3)                                &
     &              +UI(I,2)*TOTROT(2,3)                                &
     &              +UI(I,3)*TOTROT(3,3)
      ENDIF



!
      IF(LALT) THEN
      UI(I,1)=-ZTOD(1)
      UI(I,2)=-ZTOD(2)
      UI(I,3)=-ZTOD(3)
       RA(I,1)= XTOD(1)*OFFSET(1,2)+YTOD(1)*OFFSET(2,2)                 &
     &          +ZTOD(1)*OFFSET(3,2)
        RA(I,2)= XTOD(2)*OFFSET(1,2)+YTOD(2)*OFFSET(2,2)                &
     &          +ZTOD(2)*OFFSET(3,2)
        RA(I,3)= XTOD(3)*OFFSET(1,2)+YTOD(3)*OFFSET(2,2)                &
     &          +ZTOD(3)*OFFSET(3,2)
       GO TO 300
      ENDIF
      IF(.NOT.LGIMB) GO TO 290
!
! COMPUTE THE ROTATION FROM THE GCS TO SBF FRAME
! ***NOTE: NOMINAL GIMBAL DEPLOYMENT ANGLE IS 45 deg (21+24, from Ref 2,
! ***Fig 4) THIS MUST BE CHECKED AGAINST ACTUAL DEPLOYMENT
         GIMANG =  45.0D0 * DEGRAD
         COSGIM = COS(GIMANG)
         SINGIM = SIN(GIMANG)
         GCS(1,1) = -ONE
         GCS(2,1) = ZERO
         GCS(3,1) = ZERO
         GCS(1,2) = ZERO
         GCS(2,2) = -COSGIM
         GCS(3,2) = SINGIM
         GCS(1,3) = ZERO
         GCS(2,3) = SINGIM
         GCS(3,3) = COSGIM
!
! COMPUTE ROTATION FROM INERTIAL TO GSC (GSCT*SBFT)
         TEMP(1,1) = -XTOD(1)
         TEMP(1,2) = -XTOD(2)
         TEMP(1,3) = -XTOD(3)
         TEMP(2,1) = -COSGIM*YTOD(1)+SINGIM*ZTOD(1)
         TEMP(2,2) = -COSGIM*YTOD(2)+SINGIM*ZTOD(2)
         TEMP(2,3) = -COSGIM*YTOD(3)+SINGIM*ZTOD(3)
         TEMP(3,1) =  SINGIM*YTOD(1)+COSGIM*ZTOD(1)
         TEMP(3,1) =  SINGIM*YTOD(2)+COSGIM*ZTOD(2)
         TEMP(3,1) =  SINGIM*YTOD(3)+COSGIM*ZTOD(3)
!
! ROTATE MO-EARTH VECTOR INTO GSC FROM INERTIAL (GSCT*SBFT)
         GROMEX =-TEMP(1,1)*UI(I,1)-TEMP(1,2)*UI(I,2)-TEMP(1,3)*UI(I,3)
         GRMOEY =-TEMP(2,1)*UI(I,1)-TEMP(2,2)*UI(I,2)-TEMP(2,3)*UI(I,3)
         GRMOEZ =-TEMP(3,1)*UI(I,1)-TEMP(3,2)*UI(I,2)-TEMP(3,3)*UI(I,3)
         GRMAG  = SQRT(GRMOEX**2+GRMOEY**2+GRMOEZ**2)
         GRMOEX = GRMOEX/GRMAG
         GRMOEY = GRMOEY/GRMAG
         GRMOEZ = GRMOEZ/GRMAG
!
!   COMPUTE GIMBAL ANGLES ALPHA & BETA (REF #1, EQN 11a & 12a)
         SINB = -GRMOEZ
         COSB =  SQRT(ONE+SINB**2)
         SINA =  GRMOEX/COSB
         COSA = -GRMOEY/COSB
!
! COMPUTE ROTATION FROM ANTENNA COORD TO GIMBAL COORD.
         ACS(1,1) =  COSA
         ACS(1,2) = -SINA*COSB
         ACS(1,3) =  SINA*SINB
         ACS(2,1) =  SINA
         ACS(2,2) =  COSA*COSB
         ACS(2,3) = -COSA*SINB
         ACS(3,1) =  ZERO
         ACS(3,2) =  SINB
         ACS(3,3) =  COSB
!
! COMPUTE ROTATION FROM ANTENNA COORD TO BODY-FIXED COORD
!
        DO 200 J=1,3
        TEMP(J,1) =     GCS(J,1)*ACS(1,1)                               &
     &                + GCS(J,2)*ACS(2,1)                               &
     &                + GCS(J,3)*ACS(3,1)
        TEMP(J,2) =     GCS(J,1)*ACS(1,2)                               &
     &                + GCS(J,2)*ACS(2,2)                               &
     &                + GCS(J,3)*ACS(3,2)
        TEMP(J,3) =     GCS(J,1)*ACS(1,3)                               &
     &                + GCS(J,2)*ACS(2,3)                               &
     &                + GCS(J,3)*ACS(3,3)
  200 END DO
!
! ROTATE ANTENNA VECTOR IN ACS FRAME TO SBF FRAME (PT. H to A)
           ANTOFF(1) =                 TEMP(1,1)*ANTENA(1)              &
     &                               + TEMP(1,2)*ANTENA(2)              &
     &                               + TEMP(1,3)*ANTENA(3)
           ANTOFF(2) =                 TEMP(2,1)*ANTENA(1)              &
     &                               + TEMP(2,2)*ANTENA(2)              &
     &                               + TEMP(2,3)*ANTENA(3)
           ANTOFF(3) =                 TEMP(3,1)*ANTENA(1)              &
     &                               + TEMP(3,2)*ANTENA(2)              &
     &                               + TEMP(3,3)*ANTENA(3)
  290 CONTINUE
!
! COMPUTE TOTAL BODY-FIXED OFFSET VECTOR (ANTENNA + OFFSET)
      BDFOFF(1) = ANTOFF(1) + OFFSET(1,2)
      BDFOFF(2) = ANTOFF(2) + OFFSET(2,2)
      BDFOFF(3) = ANTOFF(3) + OFFSET(3,2)
!
! ROTATE TOTAL OFFSET VECTOR FROM SBF TO INERTIAL FRAME
      OFFTDX = XTOD(1)*BDFOFF(1)+YTOD(1)*BDFOFF(2)+ZTOD(1)*BDFOFF(3)
      OFFTDY = XTOD(2)*BDFOFF(1)+YTOD(2)*BDFOFF(2)+ZTOD(2)*BDFOFF(3)
      OFFTDZ = XTOD(3)*BDFOFF(1)+YTOD(3)*BDFOFF(2)+ZTOD(3)*BDFOFF(3)
! COMPUTE INERTIAL OFFSET VECTOR
! COMPUTE INERTIAL OFFSET VECTOR
      RA(I,1) = OFFTDX
      RA(I,2) = OFFTDY
      RA(I,3) = OFFTDZ
  300 END DO
      IF(LALT) RETURN
!
!...RANGE CORRECTION IS DOT PRODUCT OF INERTIAL OFFSET AND
!...UNIT INERTIAL TOPOCENTRIC VECTORS
      CALL DOTPRD(RA,UI,DR,NM,NM,NM,3)
      RETURN
60000 FORMAT(' WARNING !!!!!!!!!!!!!!!!')
60001 FORMAT(' TRKMO CALLED FOR GPS MEASUREMENTS:')
60002 FORMAT(' THIS ROUTINE DOES NOT COMPUTE:')
60003 FORMAT('   (1) PHASE WIND UP CORRECTION')
60004 FORMAT('   (2) ANTENNA MAP CORRECTIOINS')
60005 FORMAT('   (3) ANTENNA FRAME EDITING (ANTCUT)')
60006 FORMAT(' TO ADD THESE FEATURES SEE TRKEXT OR TRKTOP')
      END
