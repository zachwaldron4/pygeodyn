!$TRMMAT
      SUBROUTINE TRMMAT(MJDS,FSEC,XSAT,VSAT,BFNRM1,BFNRM2,BFNRM3,       &
     &                  TDNRM1,TDNRM2,TDNRM3,CTHETA,NFACE,NMOVE,        &
     &                  LFORCE,IDSATS,IDATTB,SABIAS,TIMBI1,TIMBI2,      &
     &                  VLOUVS,NSTLOV,ISLVID,TSLOUV,AA,II,ISATID,TEMP)
!*******************************************************************
!  ROUTINE NAME:   TRMMAT   DATE: 03/10/98      PGMR: C.M. Cox
!
!   FUNCTION - TO COMPUTE ROTATION FROM TRMM BODY-FIXED FRAME TO THE
!              GEODYN TRUE OF REFERENCE FRAME, BASED ON THE TRMM
!              ATTITUDE CONTROL LAWS.  ALSO, COMPUTE PARTIAL OF SPF
!              ROTATION MATRIX (HCL TO XYZ) WRT SAT STATE AS NEEDED
!              FOR VMATR PARTIALS
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   MJDS     I      S    INTEGER EPHEMERIS SECONDS SINCE GEODYN REF.TIME
!   FSEC     I      S    FRACTIONAL REMAINING SECONDS
!   XSAT     A      I    SPACECRAFT POSITION IN TRUE OF DATE SYSTEM (M)
!   VSAT     A      I    SPACECRAFT VELOCITY IN TRUE OF DATE SYSTEM (M/S
!   BFNRM1   A      I    SPACECRAFT FLAT PLATE UNIT NORMAL VECTORS IN
!                        SPACECRAFT BODY-FIXED SYSTEM (X COMP.)
!   BFNRM2   A      I    SPACECRAFT FLAT PLATE UNIT NORMAL VECTORS IN
!                        SPACECRAFT BODY-FIXED SYSTEM (Y COMP.)
!   BFNRM3   A      I    SPACECRAFT FLAT PLATE UNIT NORMAL VECTORS IN
!                        SPACECRAFT BODY-FIXED SYSTEM (Z COMP.)
!   TDNRM1   A      O    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (X COMP.)
!   TDNRM2   A      O    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (Y COMP.)
!   TDNRM3   A      O    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (Z COMP.)
!   CTHETA   A      O    COS OF ANGLE BETWEEN TOD PLATE NORMAL AND
!                        SATELLITE-SUN VECTOR
!   NFACE    S      I    NUMBER OF FLAT PLATES USED TO MODEL SATELLITE S
!   NMOVE    S      I    NUMBER OF MOVEABLE FLAT PLATES USED
!
!************ TRMMAT LOCAL VARIABLE DEFFINITIONS************************
!   OFFANG   S      W    OFFSET ANGLE TO OBTAIN LOCAL VERTICAL POINTING
!                        TOPEX REFERENCE ELLIPSOID
!   SAROT    A      W    SOLAR ARRAY TO SBF ROTATION MATRIX
!   SBF      A      W    SBF TO SBF' ROTATION MATRIX
!   SPF      A      W    SPF TO TOD ROTATION MATRIX
!   XTOD     A      W    TOD VECTOR TANGENT TO ORBIT PLANE
!   YTOD     A      W    TOD VECTOR NORMAL TO ORBIT PLANE
!   ZTOD     A      W    TOD VECTOR FROM SPACECRAFT TO EARTH CENTER
!   VORB0    A      W    VECTOR DEFINING LOCATION OF 0 DEG ORBIT ANGLE I
!   SOMEGA   S           SPACECRAFT ORBIT ANGLE
!   BETAP    S           BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE
!   SBETAP   S           SIGN OF BETAPRIME ANGLE
!   SGAMMA   S           SOLAR ARRAY PITCH ANGLE
!
!***********************************************************************
!
! NOTES:
!            TOD = GEODYN TRUE OF DATE INERTIAL FRAME
!                  rotate to satellite alongtrack, crosstrack, radial sy
!            SPF = SATELLITE ALONGTRACK, CROSSTRACK, RADIAL FRAME
!                  rotate to satellite body-fixed frame (x,y,z)
!
! Note: in the TRMM case we consider the x to be along track, the y to b
!       cross track, and the z to be nadir or opposite radial.  Therefor
!       the SBF is the same as the SPF.
!
!            SBF = SATELLITE BODY-FIXED FRAME
!                  apply solar array angle rotations
!            SA  = SOLAR ARRAY BODY-FIXED FRAME
!
!         1) SATELLITE POINTS GEOCENTRICALLY
!         2) SATELLLITE BODY FIXED FRAME:
!             X = Approximately the same as the velocity direction (YxZ)
!             Y = Normal to orbit (ZxVsat), same as the solar array
!                  direction
!             Z = Along nadir, in direction of geocenter
!
! REFERENCES:
!            TRMM Dimensional Control Layout Drawings, GSFC
!
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/BWREAL/SHADOW(2),SUNPCT,SCAREA,SCMASS,BWMEAN
      COMMON/CBDTRU/BDTRUE(7,999)
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
      COMMON/CORA03/KRFMT ,KROTMT,KSRTCH,KCFSC ,KTHG  ,KCOSTG,KSINTG,   &
     &              KDPSI ,KEPST ,KEPSM ,KETA  ,KZTA  ,KTHTA ,KEQN  ,   &
     &              KDPSR ,KSL   ,KH2   ,KL2   ,KXPUT ,KYPUT ,KUT   ,   &
     &              KSPCRD,KSPSIG,KSTAIN,KSXYZ ,KSPWT ,KDXSDP,KDPSDP,   &
     &              KXLOCV,KSTNAM,KA1UT ,KPLTIN,KPLTVL,KPLTDV,KPUTBH,   &
     &              KABCOF,KSPEED,KANGFC,KSCROL,KSTVEL,KSTVDV,KTIMVL,   &
     &              KSTEL2,KSTEH2,KSL2DV,KSH2DV,                        &
     &              KAPRES, KASCAL, KASOFF,KXDOTP,KSAVST,               &
     &              KANGT , KSPDT , KCONAM,KGRDAN,KUANG ,KFAMP ,        &
     &              KOFACT, KOTSGN,KWRKRS,KANFSD,KSPDSD,KPRMFS,KSCRFR,  &
     &              KCOSAS,KSINAS,KDXTID,KALCOF,KSTANF,KNUTIN,KNUTMD,   &
     &              KPDPSI,KPEPST,KDXDNU,KDPSIE,KEPSTE,NXCA03
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
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/LSTRT/LSTART
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
!
      DIMENSION BFNRM1(NFACE),BFNRM2(NFACE),BFNRM3(NFACE)
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION XSAT(3),VSAT(3)
      DIMENSION CTHETA(NFACE),UNTSUN(3)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3),VORB0(3),VEC1(3),VEC2(3)
      DIMENSION SPF(3,3),SAROT(3,3),TOTROT(3,3),                        &
     &          TEMP(3,3),SBF(3,3),AA(1),II(1),QAT(4)
      DIMENSION SABIAS(NTPBIA),TIMBI1(NTPBIA),TIMBI2(NTPBIA),           &
     &          IDATTB(NTPBIA)
      DIMENSION VLOUVS(3,NSTLVS)
      DIMENSION NSTLOV(NISLV),ISLVID(NISLV)
      DIMENSION TSLOUV(NSTLVS,3)
      dimension xhelio(3)
!
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,CM999/-999.0D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
! INITIALIZE
!
      PID2=PI/TWO
      TIME=MJDS+FSEC
!
! COMPUTE TOD EARTH TO SUN UNIT VECTOR
!
      SUNMAG = SQRT(BDTRUE(1,8)**2+BDTRUE(2,8)**2+BDTRUE(3,8)**2)
      UNTSUN(1)=BDTRUE(1,8)/SUNMAG
      UNTSUN(2)=BDTRUE(2,8)/SUNMAG
      UNTSUN(3)=BDTRUE(3,8)/SUNMAG
!
! COMPUTE THE ROTATION FROM THE SPF TO TOD FRAME WHERE
!         ZTOD = TOD VECTOR FROM S/C TO CENTER OF EARTH
!         YTOD = TOD VECTOR NORMAL TO  ORBIT PLANE (Y = Z X VSAT)
!         XTOD = TOD VECTOR TANGENT TO ORBIT PLANE (X = Y X Z)
!
! ZAXIS ROTATION
!
      ZMAG = SQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
      ZTOD(1) =-XSAT(1)/ZMAG
      ZTOD(2) =-XSAT(2)/ZMAG
      ZTOD(3) =-XSAT(3)/ZMAG
      SPF(1,3)= ZTOD(1)
      SPF(2,3)= ZTOD(2)
      SPF(3,3)= ZTOD(3)
!
! YAXIS ROTATION
!
      YTOD(1) = ZTOD(2)*VSAT(3)-ZTOD(3)*VSAT(2)
      YTOD(2) = ZTOD(3)*VSAT(1)-ZTOD(1)*VSAT(3)
      YTOD(3) = ZTOD(1)*VSAT(2)-ZTOD(2)*VSAT(1)
      YMAG = SQRT(YTOD(1)**2+YTOD(2)**2+YTOD(3)**2)
      YTOD(1) = YTOD(1)/YMAG
      YTOD(2) = YTOD(2)/YMAG
      YTOD(3) = YTOD(3)/YMAG
      SPF(1,2)= YTOD(1)
      SPF(2,2)= YTOD(2)
      SPF(3,2)= YTOD(3)
!
! XAXIS ROTATION
!
      XTOD(1) = YTOD(2)*ZTOD(3)-YTOD(3)*ZTOD(2)
      XTOD(2) = YTOD(3)*ZTOD(1)-YTOD(1)*ZTOD(3)
      XTOD(3) = YTOD(1)*ZTOD(2)-YTOD(2)*ZTOD(1)
      XMAG = SQRT(XTOD(1)**2+XTOD(2)**2+XTOD(3)**2)
      XTOD(1) = XTOD(1)/XMAG
      XTOD(2) = XTOD(2)/XMAG
      XTOD(3) = XTOD(3)/XMAG
      SPF(1,1)= XTOD(1)
      SPF(2,1)= XTOD(2)
      SPF(3,1)= XTOD(3)
!
!
! COMPUTE UNIT VECTOR DEFINING ZERO ORBIT ANGLE (YTOD X SUN)
!
      VORB0(1) = YTOD(2)*BDTRUE(3,8)-YTOD(3)*BDTRUE(2,8)
      VORB0(2) = YTOD(3)*BDTRUE(1,8)-YTOD(1)*BDTRUE(3,8)
      VORB0(3) = YTOD(1)*BDTRUE(2,8)-YTOD(2)*BDTRUE(1,8)
      VORBMG = SQRT(VORB0(1)**2+VORB0(2)**2+VORB0(3)**2)
      VORB0(1) = VORB0(1)/VORBMG
      VORB0(2) = VORB0(2)/VORBMG
      VORB0(3) = VORB0(3)/VORBMG
!
! COMPUTE NEW ORBIT ANGLE
!     POSITIVE IN COUNTER-CLOCKWISE DIRECTION WHEN LOOKING DOWN ON ORBIT
!     RELATIVE TO VORB0
      VEC1(1) = VORB0(2)*ZTOD(3)-VORB0(3)*ZTOD(2)
      VEC1(2) = VORB0(3)*ZTOD(1)-VORB0(1)*ZTOD(3)
      VEC1(3) = VORB0(1)*ZTOD(2)-VORB0(2)*ZTOD(1)
      SINOMG  = VEC1(1)*YTOD(1)+VEC1(2)*YTOD(2)+VEC1(3)*YTOD(3)
      COSOMG  = -ZTOD(1)*VORB0(1)-ZTOD(2)*VORB0(2)-ZTOD(3)*VORB0(3)
      SOMEGA = ATAN2(SINOMG,COSOMG)
!
!  COMPUTE BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE)
!
      VEC1(1) = VORB0(2)*YTOD(3)-VORB0(3)*YTOD(2)
      VEC1(2) = VORB0(3)*YTOD(1)-VORB0(1)*YTOD(3)
      VEC1(3) = VORB0(1)*YTOD(2)-VORB0(2)*YTOD(1)
      VEC1MG = SQRT(VEC1(1)**2+VEC1(2)**2+VEC1(3)**2)
      VEC1(1) = VEC1(1)/VEC1MG
      VEC1(2) = VEC1(2)/VEC1MG
      VEC1(3) = VEC1(3)/VEC1MG
      VEC2(1) = VEC1(2)*BDTRUE(3,8)-VEC1(3)*BDTRUE(2,8)
      VEC2(2) = VEC1(3)*BDTRUE(1,8)-VEC1(1)*BDTRUE(3,8)
      VEC2(3) = VEC1(1)*BDTRUE(2,8)-VEC1(2)*BDTRUE(1,8)
      SINBET  = VEC2(1)*VORB0(1)+VEC2(2)*VORB0(2)+VEC2(3)*VORB0(3)
      COSBET=VEC1(1)*BDTRUE(1,8)+VEC1(2)*BDTRUE(2,8)+VEC1(3)*BDTRUE(3,8)
      BETAP  = ATAN2(SINBET,COSBET)
!
! COMPUTE ROTATION FROM SBF TO SPF FRAMES
!
      SBF(1,1) = ONE
      SBF(1,2) = ZERO
      SBF(1,3) = ZERO
      SBF(2,1) = ZERO
      SBF(2,2) = ONE
      SBF(2,3) = ZERO
      SBF(3,1) = ZERO
      SBF(3,2) = ZERO
      SBF(3,3) = ONE
!
! COMPUTE SOLAR ARRAY PITCH ANGLE (Array pitch angle rotating positive a
! SBF y axis which is also YTOD. Zero is when array x or normal is paral
! to SBF x)
!
      SABTMP=0.D0
      DO 90 I=1,NTPBIA
        IF((IDSATS.EQ.IDATTB(I)).AND.                                   &
     &  (TIME.GE.TIMBI1(I)).AND.(TIME.LE.TIMBI2(I)))SABTMP=SABIAS(I)
   90 END DO
!
      SGAMMA = SOMEGA + SABTMP
!
! Check to see if the spacecraft is in shadow. If so, feather the solar
! Feathering is defined as where the arrays would be at sgamma=+/-1.5pi
! Depending on beta prime, the arrays may be flipped.  This is important
! for thermal considerations, including the thermal effects of drag.
!
      r=zmag
      rsq=zmag*zmag
      RATIO=ONE
      XHELIO(1)=xsat(1)-BDTRUE(1,8)
      XHELIO(2)=xsat(2)-BDTRUE(2,8)
      XHELIO(3)=xsat(3)-BDTRUE(3,8)
      RSSQ=XHELIO(1)**2+XHELIO(2)**2+XHELIO(3)**2
      RS=SQRT(RSSQ)
      IF( ICBDGM .EQ. 11 ) THEN
         RATIO = ONE
      ELSE
         CALL SOLRAT(AA(KAESHD),II(KISHDP),FE,RS,RSSQ,R,RSQ,xsat,RATIO)
      ENDIF
!     write(6,*) 'TRMMAT: somega,ratio',somega,ratio
      IF(ratio.lt..99D0) sgamma=1.5D0*pi
!
! Flip the s/c about the z axis if beta < 0.  The +Y side of the s/c nee
! to remain cold, and the sun sensors are on the -Y side.  The SA will a
! need to be flipped, and is driven backwards, so add 180 degrees to min
!
!     dtemp=ytod(1)*untsun(1)+ytod(2)*untsun(2)+ytod(3)*untsun(3)
!     write(6,*) 'TRMMAT: dtemp,betap,sgamma',dtemp,betap,sgamma
      if(betap.lt.0.D0) then
         spf(1,1)=-spf(1,1)
         spf(2,1)=-spf(2,1)
         spf(3,1)=-spf(3,1)
         spf(1,2)=-spf(1,2)
         spf(2,2)=-spf(2,2)
         spf(3,2)=-spf(3,2)
         sgamma=pi-sgamma
      endif
      COSGAM = COS(SGAMMA)
      SINGAM = SIN(SGAMMA)
!
! COMPUTE ROTATION FOR SOLAR ARRAY (To rotate from the solar array frame
! the SBF frame, rotate about the SBF y axis by the orbit angle above. N
! the solar array x axis is assumed to be the sun tracking side.  This i
! important when developing the PANEL cards)
!
      SAROT(1,1) = COSGAM
      SAROT(1,2) = ZERO
      SAROT(1,3) = SINGAM
      SAROT(2,1) = ZERO
      SAROT(2,2) = ONE
      SAROT(2,3) = ZERO
      SAROT(3,1) = -SINGAM
      SAROT(3,2) = ZERO
      SAROT(3,3) = COSGAM
!
! COMPUTE TOTAL ROTATION MATRIX FROM SBF TO TOD FRAME
!
      DO 100 I=1,3
         TOTROT(I,1) = ZERO
         TOTROT(I,2) = ZERO
         TOTROT(I,3) = ZERO
  100 END DO
!
      DO 150 I=1,NFACE
         TDNRM1(I) = ZERO
         TDNRM2(I) = ZERO
         TDNRM3(I) = ZERO
  150 END DO
!
      DO 200 I=1,3
      TOTROT(I,1) =               SPF(I,1)*SBF(1,1)                     &
     &                          + SPF(I,2)*SBF(2,1)                     &
     &                          + SPF(I,3)*SBF(3,1)
      TOTROT(I,2) =               SPF(I,1)*SBF(1,2)                     &
     &                          + SPF(I,2)*SBF(2,2)                     &
     &                          + SPF(I,3)*SBF(3,2)
      TOTROT(I,3) =               SPF(I,1)*SBF(1,3)                     &
     &                          + SPF(I,2)*SBF(2,3)                     &
     &                          + SPF(I,3)*SBF(3,3)
  200 END DO
!
!
! ROTATE SBF UNIT NORMAL VECTORS(NON-MOVING PLATES) TO TOD FRAME
!
      DO 500 I=1,NFACE-NMOVE
         TDNRM1(I) =                 TOTROT(1,1)*BFNRM1(I) +            &
     &                               TOTROT(1,2)*BFNRM2(I) +            &
     &                               TOTROT(1,3)*BFNRM3(I)
         TDNRM2(I) =                 TOTROT(2,1)*BFNRM1(I) +            &
     &                               TOTROT(2,2)*BFNRM2(I) +            &
     &                               TOTROT(2,3)*BFNRM3(I)
         TDNRM3(I) =                 TOTROT(3,1)*BFNRM1(I) +            &
     &                               TOTROT(3,2)*BFNRM2(I) +            &
     &                               TOTROT(3,3)*BFNRM3(I)
  500 END DO
!
! IF MOVEABLE PLATES EXIST COMPUTE THEIR ROTATIONS (Movable plates in th
! model are the two SA panels!)
!

      DO 600 I=1,3
      TEMP(I,1) = TOTROT(I,1)
      TEMP(I,2) = TOTROT(I,2)
      TEMP(I,3) = TOTROT(I,3)
  600 END DO

      IF(NMOVE.LE.0) GOTO 850
!
      DO 700 I=1,3
      TOTROT(I,1) =               TEMP(I,1)*SAROT(1,1)                  &
     &                          + TEMP(I,2)*SAROT(2,1)                  &
     &                          + TEMP(I,3)*SAROT(3,1)
      TOTROT(I,2) =               TEMP(I,1)*SAROT(1,2)                  &
     &                          + TEMP(I,2)*SAROT(2,2)                  &
     &                          + TEMP(I,3)*SAROT(3,2)
      TOTROT(I,3) =               TEMP(I,1)*SAROT(1,3)                  &
     &                          + TEMP(I,2)*SAROT(2,3)                  &
     &                          + TEMP(I,3)*SAROT(3,3)
  700 END DO
!
! ROTATE SBF UNIT NORMAL VECTORS(MOVING PLATES) TO TOD FRAME
!
      DO 800 I=NFACE-NMOVE+1,NFACE
         TDNRM1(I) =                 TOTROT(1,1)*BFNRM1(I)              &
     &                             + TOTROT(1,2)*BFNRM2(I)              &
     &                             + TOTROT(1,3)*BFNRM3(I)
         TDNRM2(I) =                 TOTROT(2,1)*BFNRM1(I)              &
     &                             + TOTROT(2,2)*BFNRM2(I)              &
     &                             + TOTROT(2,3)*BFNRM3(I)
         TDNRM3(I) =                 TOTROT(3,1)*BFNRM1(I)              &
     &                             + TOTROT(3,2)*BFNRM2(I)              &
     &                             + TOTROT(3,3)*BFNRM3(I)
!       dtemp=TDNRM1(I)*UNTSUN(1)+TDNRM2(I)*UNTSUN(2)+
!    1            TDNRM3(I)*UNTSUN(3)
!     write(6,2222) i,dtemp,tdnrm1(i),tdnrm2(i),tdnrm3(i)
!2222 format(1x,'TRMMAT: i, dtemp, tdnrmn(i)',i2,4f10.6)
  800 END DO
!
  850 CONTINUE
!
! COMPUTE UNIT NORMAL VECTORS IN TOD FRAME
!
      DO 900 I=1,NFACE
         RSUM = SQRT(TDNRM1(I)**2+TDNRM2(I)**2+TDNRM3(I)**2)
         TDNRM1(I) = TDNRM1(I)/RSUM
         TDNRM2(I) = TDNRM2(I)/RSUM
         TDNRM3(I) = TDNRM3(I)/RSUM
  900 END DO
!
! COMPUTE COSINE OF ANGLE BETWEEN TDNRM# AND UNTSUN
!
      IF(LFORCE)THEN
      DO 950 I=1,NFACE
        CTHETA(I)=TDNRM1(I)*UNTSUN(1)+TDNRM2(I)*UNTSUN(2)+              &
     &            TDNRM3(I)*UNTSUN(3)
  950 END DO
!
! ROTATE BODY-FIXED LOUVER ACCELERATION VECTOR IF NECESSARY
      CAll FNDNUM(IDSATS,ISLVID,NISLV ,IRET)
      IF(IRET.LE.0)GOTO 1000
       KST=NSTLOV(IRET)
       KNST0=0
       DO K=1,IRET
       KNST0=KNST0+NSTLOV(K)
       ENDDO
      IF(KST .GT. ZERO) THEN
       DO 955 INLV=1,KST
       KNST=KNST0+INLV-KST
         TSLOUV(INLV,1) =        TEMP(1,1)*VLOUVS(1,KNST)               &
     &                         + TEMP(1,2)*VLOUVS(2,KNST)               &
     &                         + TEMP(1,3)*VLOUVS(3,KNST)
         TSLOUV(INLV,2) =        TEMP(2,1)*VLOUVS(1,KNST)               &
     &                         + TEMP(2,2)*VLOUVS(2,KNST)               &
     &                         + TEMP(2,3)*VLOUVS(3,KNST)
         TSLOUV(INLV,3) =        TEMP(3,1)*VLOUVS(1,KNST)               &
     &                         + TEMP(3,2)*VLOUVS(2,KNST)               &
     &                         + TEMP(3,3)*VLOUVS(3,KNST)
         RSUM = SQRT(TSLOUV(INLV,1)**2+TSLOUV(INLV,2)**2+               &
     &               TSLOUV(INLV,3)**2)
         TSLOUV(INLV,1) = TSLOUV(INLV,1)/RSUM
         TSLOUV(INLV,2) = TSLOUV(INLV,2)/RSUM
         TSLOUV(INLV,3) = TSLOUV(INLV,3)/RSUM
  955  CONTINUE
         ENDIF
 1000  CONTINUE
         ENDIF
!
! OUTPUT TDRS TELEM FILE INFORMATION
      IF(LTXPRT .AND. .NOT. LSTART.AND.LFORCE) THEN
!....INTEGRATION STEP TIME
         CALL UTCET(.FALSE.,1,MJDS,FSEC,FSECU,AA(KA1UT))
         CALL YMDHMS(MJDS,FSECU,IYMD,IHM,SEC,1)
         DIYMD=DBLE(IYMD)
         DIHM= DBLE(IHM)
          RSATID=DBLE(ISATID)
!....PERTINENT ANGLES
         CALL ROTQAT(TEMP,QAT)
         WRITE(97) DIYMD,DIHM,SEC,CM999,BETAP/DEGRAD,SOMEGA/DEGRAD, &
     &             CM999,SGAMMA/DEGRAD,CM999,QAT(1),QAT(2),QAT(3), &
     &             QAT(4),XSAT(1),XSAT(2),XSAT(3),VSAT(1),VSAT(2), &
     &             VSAT(3),RSATID
      ENDIF
      RETURN
      END
