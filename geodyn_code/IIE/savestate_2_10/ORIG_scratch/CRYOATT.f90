!$CRYOATT
      SUBROUTINE CRYOATT(MJDS,FSEC,XSAT,VSAT,BFNRM1,BFNRM2,BFNRM3,      &
     &                  TDNRM1,TDNRM2,TDNRM3,CTHETA,                    &
     &                  NFACE,NMOVE,                                    &
     &                  LFORCE,SUNXYZ,AA,ISATID,ISATN,TOTROT,YAWBST,    &
     &                  ALTPT)
!*******************************************************************
!  ROUTINE NAME:  CRYOATT   DATE: 08/04/2010      PGMR: Oleg Bordyugov
!
!   FUNCTION - TO COMPUTE ROTATION FROM CRYOSAT 2 SAT BODY FIXED FRAME
!              TO THE GEODYN TRUE OF REFERENCE FRAME, BASED ON THE
!              CRYOSAT 2 ATTITUDE CONTROL LAWS.  ALSO, COMPUTE PARTIAL
!              OF SPF ROTATION MATRIX (HCL TO XYZ) WRT SAT STATE AS
!              NEDDED FOR VMATR PARTIALS
!
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
!   NFACE    S      I    NUMBER OF FLAT PLATES USED TO MODEL SATELLITE S
!   NMOVE    S      I    NUMBER OF MOVEABLE FLAT PLATES USED
!   LFORCE          I    .TRUE. IF TOPATT CALLED FROM F
!                        .FALSE. IF TOPATT CALLED FROM TRKTOP
!   SUNXYZ   A      I    TOD SUN POS
!   ISATID   S      I    SAT. ID
!
!************ TOPATT LOCAL VARIABLE DEFFINITIONS************************
!   XTOD     A      W    TOD VECTOR TANGENT TO ORBIT PLANE
!   YTOD     A      W    TOD VECTOR NORMAL TO ORBIT PLANE
!   ZTOD     A      W    TOD VECTOR FROM SPACECRAFT TO EARTH CENTER
!   VORB0    A      W    VECTOR DEFINING LOCATION OF 0 DEG ORBIT ANGLE I
!***********************************************************************
!
!************ TOPEXA,TOPEXI,TOPEXL COMMON BLOCK DEFINITIONS*************
!   SOMEGG   S           SPACECRAFT ORBIT ANGLE
!   BETAG    S           BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE
!   YAWAGG   S           SPACECRAFT ROTATION ANGLE ABOUT YAW AXIS
!   LTOPX1   L           FIRST CALL TO THIS ROUTINE AFTER STARTER(.TRUE.
!
!***********************************************************************
! NOTES:
!          AB =   Antenna Bench reference frame
!                   Z_AB axis is directed normal to the ellipsoid.
!                        (nadir pointing)
!                   Y_AB= Z_AB X VSAT
!                   X_AB along the ground track
!         SBF =   S/C Body-fixed frame (SAT)
!                  +X "Nose Down", 6 degrees off the X_AB along track
!                     axis
!                  +Z 6 degrees off the ellipsoid normal upwards positive
!                  +Y same as Y_AB. (Rotate Y by 6 degrees to go to the AB
!                     reference frame.
!                THERE ARE  NO MOVABLE PARTS
!         NADIR = Geodetic Nadir Frame
!                  +X completes Y x Z
!                  +Y SUN cross Geodetic Nadir
!                  +Z axis pointing geodetic nadir
!         SPF =   Satellite alongtrack, crosstrack, radial frame
!         TOD =   GEODYN TRUE OF DATE INERTIAL FRAME
!
! REFERENCES: esa/ Cryosat-2 Precise Orbit Context Doc.No CS-TN-ESA-SY-0239
!             Issue: 2.00  16 Feb, 2007
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CREFMT/REFMT(9)
      COMMON/CRMI/RMI(9)
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/BWREAL/SHADOW(2),SUNPCT,SCAREA,SCMASS,BWMEAN
      COMMON/CINTL/LORBIT,LORBVE,LNDRAG,LNSLRD,LDRADJ,LSRADJ,           &
     &             LBACKW,LSETS
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
      COMMON/CORA04/KABIAS,KABSTR,KABSTP,KCBIAS,KCBSTR,KCBSTP,          &
     &       KRNM  ,KRKM  ,KRKJ  ,KRIJ  ,KRKPM ,                        &
     &       KRRNM ,KRRKM ,KRRKJ ,KRRIJ ,KRRKPM,                        &
     &       KURNM ,KURKM ,KURKJ ,KURIJ ,KURKPM,                        &
     &       KPRRNM,KPRRKM,KPRRKJ,KPRRIJ,KPRRKP,                        &
     &       KPMPXI,KPMPXE,KRA   ,KRELV ,KUHAT ,KWORK ,KTMPCR,KU    ,   &
     &       KS0   ,KS1   ,KXTP  ,KPXSXP,KPXDXP,KXUTDT,KRPOLE,KPXPOL,   &
     &       KXDPOL,KOBBUF,KOBSC ,KRESID,KSIGMA,KRATIO,KELEVS,          &
     &       KPMPA ,KPXSLV,KTPRTL,                                      &
     &       KFRQOF,KXYZOF,KFRQST,KFRQSA,KSTDLY,KSADLY,KXYZCG,KSCMRA,   &
     &       KEBVAL,KEBSIG,KEBNAM,KBTWB ,KTBTWA,KBTWD ,KPMPE ,          &
     &       KFSEB1,KFSEB2,KSTAMT,KPXSPA,KDIAGN,KOBOUT,KGRLT1,KGRLT2,   &
     &       KGRLT3,KGRLT4,KGRLT5,KPSTAT,KRSUNS,KCHEMR,KCHMOR,KEPOSR,   &
     &       KCHEMT,KCHMOT,KEPOST,KCHCBS,KCPOSS,KSCRTM,KXSTT ,KXSTR ,   &
     &       KXSS  ,KRANGT,KRANGR,KCHB1 ,KCHB2 ,KCHBV1,KCHBV2,KCHEB ,   &
     &       KSSTFQ,KSSTCC,KSSTSS,KSSTWT,KRLCOR,KWVLBI,KQUINF,KBRTS ,   &
     &       KBRTSV,KLRARC,KXEPBF,KXEPBR,KPRCBD,KPRTBD,KXPMPA,KXL,      &
     &       KXSIG ,KXEDTS,KXALO1,KXALO2,KXYOF2,KDLATF,KDLATS,KDLONF,   &
     &       KDLONS,KPF   ,KPS   ,                                      &
     &       KXOBSV,KXOBSW,KXEDSW,                                      &
     &       KXSIGW,KPSF  ,KDNUM ,KCNUM ,KFCOR ,KCOSAR,KSINAR,KSABIA,   &
     &       KSBTM1,KSBTM2,KYAWBS,KVLOUV,KACMAG,KOBSTR,                 &
     &       KPRL1 ,KPRL2, KRL1  ,KT1SE ,KTCP  ,                        &
     &       KRATDR,KFQT1S,KFQT1E,KT1STR,KFTTSE,KFRSTR,KFRRAT,KSV1  ,   &
     &       KSV2  ,KTSLOV,                                             &
     &       KGLGR1,KGLGR2,KGLFR1,KGLFR2,                               &
     &       KARGR1,KARGR2,KARFR1,KARFR2,                               &
     &       KRDS1L,KFT1AV,KDFQP ,KFREQ1,KFREQ3,KSAVD1,KSAVD2,          &
     &       KANTOU,KFM3CF,KF2CF,KTMG,KLTMG,KX2TIM,KX2OBS,KXRNDX,KX2SCR,&
     &       KALTWV,KXXBM ,KX2PAR,KATIME,KPMPAT,KPMATT,KX2PAT,          &
     &       KPXEXI,KPXEPA,KPV,KPXEP2,KX2COF,KACOF2,KACOF3,KBCOF,KBCOF2,&
     &       KDDDA ,KX2AUX,KX2VPR,KX2VPA,KEXTRA,KVARAY,KATROT,KATPER,   &
     &       KLTAR ,KXHOLD,KANTBL,KPHC  ,KOFDRV,KGNAME,KGRSIZ,KGRCNT,   &
     &       KGRDAT,KACCDT,KCTBTI,KCTBWE,KCTCTM,KANTUV,KANTOR,KW1PU ,   &
     &       KPYSQH,KSIGSP,KXYOF3,KXVTM1,KXDIST,KXDST0,KTMSE ,KDSDP ,   &
     &       KEXCST,KEXCDT,KEXCGX,KEXCGY,KEXCGZ,KIMNDX,KIMOBS,KIMTIM,   &
     &       KIMSAT,KM2VPA,KDCDD ,KDWNWT,KDPOR ,KC2PAR,KBOUNC,KBPART,   &
     &       NXCA04
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CEARTH/AEG,AEGSQ,FGINV,FG,EGSQ,EGSQP1,C1,C2,FOURC1,TWOC2,  &
     &              XH2,XL2,WREF,RMEAN,REFC20,REFC40,REFC60,BEG,CBLTC,  &
     &              WAE2,GAMMAA,FSTAR,F4
      COMMON/LSTRT/LSTART
      COMMON/TOPEXA/BETAP,PRVBET,SOMEGA,PRVOMG,YAWANG,SGAMMA,           &
     &              BETAMX,TDLOUV(8,3)
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/STARTT/ESSTRT,FSSTRT
!
      DIMENSION RADIAL(3),CROSS(3),ALONG(3)
      DIMENSION URADIAL(3),UCROSS(3),UALONG(3)
      DIMENSION R1(3,3),R2(3,3),R3(3,3)
      DIMENSION TODSPF(3,3),RNADIR(3,3),RSBF(3,3)
      DIMENSION QL(4),QN(4),QF(4)
      DIMENSION QUAT(4),QAT1(4),QAT2(4),QAT3(4)
      DIMENSION AA(1)
      DIMENSION BFNRM1(NFACE),BFNRM2(NFACE),BFNRM3(NFACE)
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION CTHETA(NFACE)
      DIMENSION XSAT(3),VSAT(3)
      DIMENSION UNTSUN(3)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3),VORB0(3),VEC1(3),VEC2(3)
      DIMENSION YN(3),ZN(3),SCX(3),SCY(3),SCZ(3)
      DIMENSION TOTROT(3,3) ,QRMI(3,3),QREFMT(3,3)
      DIMENSION SUNXYZ(3)
      DIMENSION QAT(4)
      DIMENSION ISATN(MSATYW),YAWBST(MSATYW,MAXYWB,3)
      DIMENSION ALTPT(3)
!
!
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,THREE/3.0D0/,SIX/6.0D0/
      DATA SIXM/-6.0D0/,TEN/10.0D0/,TWENTY/20.0D0/,FF/45.0D0/
      DATA C54P0/54.0D0/,C80P0/80.0D0/,C90P0/90.0D0/,C15P0/15.0D0/
      DATA C104P0/104.0D0/,C260P0/260.0D0/,C284P0/284.0D0/
      DATA C360P0/360.0D0/
      DATA GFOYWB/-6.1928D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
! INITIALIZE
      PID2=PI/TWO
      TIME=MJDS+FSEC
! GEODETIC POINTING
      OPNT=0.D0
!
      CryoSatPitch=-6.0D0                    !!! PITCH ANGLE
      csat=COS(CryoSatPitch*degrad)
      ssat=SIN(CryoSatPitch*degrad)
!
!Determine Cross-track & Along-track vectors (in TOD system)

        x=XSAT(1)
        y=XSAT(2)
        z=XSAT(3)
       dx=VSAT(1)
       dy=VSAT(2)
       dz=VSAT(3)

       crx=y*dz-z*dy
       cry=z*dx-x*dz
       crz=x*dy-y*dx
       alx=cry*z-crz*y
       aly=crz*x-crx*z
       alz=crx*y-cry*x

       radial(1)=xsat(1)
       radial(2)=xsat(2)
       radial(3)=xsat(3)
       cross(1)=crx
       cross(2)=cry
       cross(3)=crz
       along(1)=alx
       along(2)=aly
       along(3)=alz

!  Determine rotation & Rotate about Z-axis



       phi=ACOS(alx/SQRT(alx**2+aly**2))

       if (aly.LT.0) then
        cp=COS(-phi)
        sp=SIN(-phi)
        isign1=-1
       elseif (aly.GT.0) then
        cp=COS(phi)
        sp=SIN(phi)
        isign1=1
       endif

       R1(1,1)=cp
       R1(1,2)=sp
       R1(1,3)=0.D0
       R1(2,1)=-sp
       R1(2,2)=cp
       R1(2,3)=0.D0
       R1(3,1)=0.D0
       R1(3,2)=0.D0
       R1(3,3)=1.D0

       cx=crx
       cy=cry
       cz=crz
       crx=cx*R1(1,1)+cy*R1(1,2)+cz*R1(1,3)
       cry=cx*R1(2,1)+cy*R1(2,2)+cz*R1(2,3)
       crz=cx*R1(3,1)+cy*R1(3,2)+cz*R1(3,3)
       rx=x
       ry=y
       rz=z
       x=rx*R1(1,1)+ry*R1(1,2)+rz*R1(1,3)
       y=rx*R1(2,1)+ry*R1(2,2)+rz*R1(2,3)
       z=rx*R1(3,1)+ry*R1(3,2)+rz*R1(3,3)
       ax=alx
       ay=aly
       az=alz
       alx=ax*R1(1,1)+ay*R1(1,2)+az*R1(1,3)
       aly=ax*R1(2,1)+ay*R1(2,2)+az*R1(2,3)
       alz=ax*R1(3,1)+ay*R1(3,2)+az*R1(3,3)

! Determine rotation & Rotate about Y-axis

       theta=ACOS(alx/SQRT(alx**2+alz**2))

       if (alz.LT.0) then
        ct=COS(theta)
        st=SIN(theta)
        isign2=1
       elseif (alz.GT.0) then
        ct=COS(-theta)
        st=SIN(-theta)
        isign2=-1
       endif

       R2(1,1)=ct
       R2(1,2)=0.D0
       R2(1,3)=-st
       R2(2,1)=0.D0
       R2(2,2)=1.D0
       R2(2,3)=0.D0
       R2(3,1)=st
       R2(3,2)=0.D0
       R2(3,3)=ct

       cx=crx
       cy=cry
       cz=crz
       crx=cx*R2(1,1)+cy*R2(1,2)+cz*R2(1,3)
       cry=cx*R2(2,1)+cy*R2(2,2)+cz*R2(2,3)
       crz=cx*R2(3,1)+cy*R2(3,2)+cz*R2(3,3)
       rx=x
       ry=y
       rz=z
       x=rx*R2(1,1)+ry*R2(1,2)+rz*R2(1,3)
       y=rx*R2(2,1)+ry*R2(2,2)+rz*R2(2,3)
       z=rx*R2(3,1)+ry*R2(3,2)+rz*R2(3,3)
       ax=alx
       ay=aly
       az=alz
       alx=ax*R2(1,1)+ay*R2(1,2)+az*R2(1,3)
       aly=ax*R2(2,1)+ay*R2(2,2)+az*R2(2,3)
       alz=ax*R2(3,1)+ay*R2(3,2)+az*R2(3,3)

! Determine rotation & Rotate about X-axis

       psi=ACOS(z/SQRT(z**2+y**2))

       if (y.LT.0) then
        cs=COS(psi)
        ss=SIN(psi)
        isign3=1
       elseif(y.GT.0) then
        cs=COS(-psi)
        ss=COS(-psi)
        isign3=-1
       endif

       R3(1,1)=1.D0
       R3(1,2)=0.D0
       R3(1,3)=0.D0
       R3(2,1)=0.D0
       R3(2,2)=cs
       R3(2,3)=ss
       R3(3,1)=0.D0
       R3(3,2)=-ss
       R3(3,3)=cs

! Compute the Total Rotation Matrix (TOD -> LOCAL)

       cp=COS(isign1*phi)
       sp=SIN(isign1*phi)
       ct=COS(isign2*theta)
       st=SIN(isign2*theta)
       cs=COS(isign3*psi)
       ss=SIN(isign3*psi)


       TODSPF(1,1)=ct*cp
       TODSPF(1,2)=ct*sp
       TODSPF(1,3)=-st
       TODSPF(2,1)=ss*st*cp-cs*sp
       TODSPF(2,2)=ss*st*sp+cs*cp
       TODSPF(2,3)=ct*ss
       TODSPF(3,1)=cs*st*cp+ss*sp
       TODSPF(3,2)=cs*st*sp-ss*cp
       TODSPF(3,3)=ct*cs

!     write(6,*)' dbg TODSPF '
!     write(6,*)TODSPF(1,1),TODSPF(1,2),TODSPF(1,3)
!     write(6,*)TODSPF(2,1),TODSPF(2,2),TODSPF(2,3)
!     write(6,*)TODSPF(3,1),TODSPF(3,2),TODSPF(3,3)

      CALL ROTQAT(TODSPF,QAT)

!Load the (TOD --> LOCAL) rotation into the quaternion array to simplify
!further operations and minimize the size of information.

       do j=1,4
        QL(j)=QAT(j)
       enddo

       rmag=SQRT(radial(1)**2+radial(2)**2+radial(3)**2)
       cmag=SQRT(cross(1)**2+cross(2)**2+cross(3)**2)
       amag=SQRT(along(1)**2+along(2)**2+along(3)**2)

       do j=1, 3
        uradial(j)=radial(j)/rmag
        ucross(j)=cross(j)/cmag
        ualong(j)=along(j)/amag
       enddo

!      ***** SANITY CHECK *****
!
!      rd1=TODSPF(1,1)*uradial(1)+                              &
!    & TODSPF(1,2)*uradial(2)+                                  &
!    & TODSPF(1,3)*uradial(3)
!      rd2=TODSPF(2,1)*uradial(1)+                              &
!    & TODSPF(2,2)*uradial(2)+                                  &
!    & TODSPF(2,3)*uradial(3)
!      rd3=TODSPF(3,1)*uradial(1)+                              &
!    & TODSPF(3,2)*uradial(2)+                                  &
!    & TODSPF(3,3)*uradial(3)
!
!      c1=TODSPF(1,1)*ucross(1)+                                &
!    & TODSPF(1,2)*ucross(2)+                                   &
!    & TODSPF(1,3)*ucross(3)
!      c2=TODSPF(2,1)*ucross(1)+                                &
!    & TODSPF(2,2)*ucross(2)+                                   &
!    & TODSPF(2,3)*ucross(3)
!      c3=TODSPF(3,1)*ucross(1)+                                &
!    & TODSPF(3,2)*ucross(2)+                                   &
!    & TODSPF(3,3)*ucross(3)
!
!      a1=TODSPF(1,1)*ualong(1)+                                &
!    & TODSPF(1,2)*ualong(2)+                                   &
!    & TODSPF(1,3)*ualong(3)
!      a2=TODSPF(2,1)*ualong(1)+                                &
!    & TODSPF(2,2)*ualong(2)+                                   &
!    & TODSPF(2,3)*ualong(3)
!      a3=TODSPF(3,1)*ualong(1)+                                &
!    & TODSPF(3,2)*ualong(2)+                                   &
!    & TODSPF(3,3)*ualong(3)

!      write(6,*) a1,a2,a3
!      write(6,*) c1,c2,c3
!      write(6,*) rd1,rd2,rd3

!Rotation is correct if Unit_44 displays the Identity Matrices !

!  DETERMINE THE NADIR POINTING VECTOR i.e. GRADIENT

       x=radial(1)
       y=radial(2)
       z=radial(3)

       x2=x*x
       y2=y*y
       z2=z*z
       a2=((x2+y2)*(1-EGSQ)+z2) / (1-EGSQ)
       b2=a2*(1-EGSQ)

       gx=TWO*x/a2
       gy=TWO*y/a2
       gz=TWO*z/b2


!   Compute the angle of rotation

       gradientnorm=SQRT(gx**2+gy**2+gz**2)
       radialnorm=SQRT(x**2+y**2+z**2)
       dnumerator=gx*x+gy*y+gz*z
       denominator=radialnorm*gradientnorm
       offangle=ACOS(dnumerator/denominator)

!   Compute the axis of rotation

       ux=y*gz-z*gy
       uy=z*gx-x*gz
       uz=x*gy-y*gx

       unorm=SQRT(ux*ux+uy*uy+uz*uz)

       ux=ux/unorm
       uy=uy/unorm
       uz=uz/unorm

!   Determine the rotation matrix (LOCAL -> NADIR)

       ux2=ux*ux
       uy2=uy*uy
       uz2=uz*uz
       uxy=ux*uy
       uxz=ux*uz
       uyz=uy*uz

       cf=COS(offangle)
       sf=SIN(offangle)

       RNADIR(1,1)=ux2+(1-ux2)*cf
       RNADIR(1,2)=uxy*(1-cf)-uz*sf
       RNADIR(1,3)=uxz*(1-cf)+uy*sf
       RNADIR(2,1)=uxy*(1-cf)+uz*sf
       RNADIR(2,2)=uy2+(1-uy2)*cf
       RNADIR(2,3)=uyz*(1-cf)-ux*sf
       RNADIR(3,1)=uxz*(1-cf)-uy*sf
       RNADIR(3,2)=uyz*(1-cf)+ux*sf
       RNADIR(3,3)=uz2+(1-uz2)*cf

      CALL ROTQAT(RNADIR,QAT)

!Load the (LOCAL --> NADIR) rotation into the quaternion array to simplify
!further operations and minimize the size of information.

       do j=1,4
        QN(j)=QAT(j)
       enddo


! The Axis of Rotation into the SBF frame must be orthogonal
! to both the direction of motion(along-track),
! and the nadir-pointing vector(radial) i.e. cross-track
! NOTE: CROSS-TRACK MUST BE FIRST ROTATED INTO THE GEODETIC FRAME!


       QW=QL(4)
       QX=QL(1)
       QY=QL(2)
       QZ=QL(3)

       QW2=QW*QW
       QX2=QX*QX
       QY2=QY*QY
       QZ2=QZ*QZ
       QWX=TWO*QW*QX
       QWY=TWO*QW*QY
       QWZ=TWO*QW*QZ
       QXY=TWO*QX*QY
       QXZ=TWO*QX*QZ
       QYZ=TWO*QY*QZ

       v1=cross(1)
       v2=cross(2)
       v3=cross(3)

       u1=qw2*v1+qx2*v1-qy2*v1-qz2*v1+qxy*v2+qwz*v2+qxz*v3-qwy*v3
       u2=qxy*v1-qwz*v1+qw2*v2-qx2*v2+qy2*v2-qz2*v2+qwx*v3+qyz*v3
       u3=qxz*v1+qwy*v1+qyz*v2-qwx*v2+qw2*v3-qx2*v3-qy2*v3+qz2*v3

       QW=QN(4)
       QX=QN(1)
       QY=QN(2)
       QZ=QN(3)

       QW2=QW*QW
       QX2=QX*QX
       QY2=QY*QY
       QZ2=QZ*QZ
       QWX=TWO*QW*QX
       QWY=TWO*QW*QY
       QWZ=TWO*QW*QZ
       QXY=TWO*QX*QY
       QXZ=TWO*QX*QZ
       QYZ=TWO*QY*QZ

       ux=qw2*u1+qx2*u1-qy2*u1-qz2*u1+qxy*u2+qwz*u2+qxz*u3-qwy*u3
       uy=qxy*u1-qwz*u1+qw2*u2-qx2*u2+qy2*u2-qz2*u2+qwx*u3+qyz*u3
       uz=qxz*u1+qwy*u1+qyz*u2-qwx*u2+qw2*u3-qx2*u3-qy2*u3+qz2*u3

       unorm=SQRT(ux*ux+uy*uy+uz*uz)
       ux=ux/unorm
       uy=uy/unorm
       uz=uz/unorm

       ux2=ux*ux
       uy2=uy*uy
       uz2=uz*uz
       uxy=ux*uy
       uxz=ux*uz
       uyz=uy*uz

       RSBF(1,1)=ux2+(1-ux2)*csat
       RSBF(1,2)=uxy*(1-csat)-uz*ssat
       RSBF(1,3)=uxz*(1-csat)+uy*ssat
       RSBF(2,1)=uxy*(1-csat)+uz*ssat
       RSBF(2,2)=uy2+(1-uy2)*csat
       RSBF(2,3)=uyz*(1-csat)-ux*ssat
       RSBF(3,1)=uxz*(1-csat)-uy*ssat
       RSBF(3,2)=uyz*(1-csat)+ux*ssat
       RSBF(3,3)=uz2+(1-uz2)*csat

      CALL ROTQAT(RSBF,QAT)

       do j=1,4
        QF(j)=QAT(j)         !Load NADIR->SBF into a Quaternion array
       enddo

!     [TOD->SBF]=[QF]*[QN]*[QL]
!     [TOD->SBF]=[R1]*[R2]*[R3]
!     [TOD->SBF]=[RI]*[R3]


       do j=1,4
        QAT1(j)=QF(j)
        QAT2(j)=QN(j)
        QAT3(j)=QL(j)
       enddo

      CALL QATROT(QAT1,R1)
      CALL QATROT(QAT2,R2)
      CALL QATROT(QAT3,R3)

      CALL MATM(R1,R2,RI)
      CALL MATM(RI,R3,TOTROT)

!     DO 100 I=1,3
!        write(6,*)'dbg ROT',TOTROT(I,1),TOTROT(I,2),TOTROT(I,3)
! 100 END DO

!      RI=MATMUL(R1,R2)
!      ROT=MATMUL(RI,R3)

      CALL ROTQAT(TOTROT,QAT)

       QAT(4) = -QAT(4)   !!! SBF --> TOD

       do j=1,4
        QUAT(j)=QAT(j)
       enddo

      CALL QATROT(QUAT,TOTROT)


!     DO 150 I=1,3
!        write(6,*)'dbg 2  ROT',TOTROT(I,1),TOTROT(I,2),TOTROT(I,3)
! 150 END DO

!        write(6,*)'dbg REFMT ',REFMT
!        write(6,*)'dbg RMI ',RMI
!        write(6,*)'dbg QAT ',QAT

      GOTO 400

!  GO TO TRUE OF REFERENCE

      DO 200 I=1,3
       QRMI(1,I) = RMI(1)*TOTROT(1,I) +                               &
     &               RMI(4)*TOTROT(2,I) +                             &
     &               RMI(7)*TOTROT(3,I)
       QRMI(2,I) = RMI(2)*TOTROT(1,I) +                               &
     &               RMI(5)*TOTROT(2,I) +                             &
     &               RMI(8)*TOTROT(3,I)
       QRMI(3,I) = RMI(3)*TOTROT(1,I) +                               &
     &               RMI(6)*TOTROT(2,I) +                             &
     &               RMI(9)*TOTROT(3,I)

       QREFMT(1,I) = REFMT(1)*QRMI(1,I) +                             &
     &               REFMT(4)*QRMI(2,I) +                             &
     &               REFMT(7)*QRMI(3,I)
       QREFMT(2,I) = REFMT(2)*QRMI(1,I) +                             &
     &               REFMT(5)*QRMI(2,I) +                             &
     &               REFMT(8)*QRMI(3,I)
       QREFMT(3,I) = REFMT(3)*QRMI(1,I) +                             &
     &               REFMT(6)*QRMI(2,I) +                             &
     &               REFMT(9)*QRMI(3,I)

  200 END DO

         CALL MJDYMD(MJDS,IYMD,IHMS,3)
         CALL UTCET(.FALSE.,1,MJDS,FSEC,FSECU,AA(KA1UT))
         CALL YMDHMS(MJDS,FSECU,IYMD,IHM,SEC,1)
         CALL ROTQAT(QREFMT,QUAT)
         write(20,21212)IYMD,IHM,SEC,QUAT
         write(22,21213)IYMD,IHM,SEC,XSAT,VSAT
      QRMI(1,1)=RMI(1)
      QRMI(1,2)=RMI(2)
      QRMI(1,3)=RMI(3)
      QRMI(2,1)=RMI(4)
      QRMI(2,2)=RMI(5)
      QRMI(2,3)=RMI(6)
      QRMI(3,1)=RMI(7)
      QRMI(3,2)=RMI(8)
      QRMI(3,3)=RMI(9)
         CALL ROTQAT(QRMI,QUAT)
         write(23,21212)IYMD,IHM,SEC,QUAT
      QREFMT(1,1)=REFMT(1)
      QREFMT(1,2)=REFMT(2)
      QREFMT(1,3)=REFMT(3)
      QREFMT(2,1)=REFMT(4)
      QREFMT(2,2)=REFMT(5)
      QREFMT(2,3)=REFMT(6)
      QREFMT(3,1)=REFMT(7)
      QREFMT(3,2)=REFMT(8)
      QREFMT(3,3)=REFMT(9)
         CALL ROTQAT(QREFMT,QUAT)
         write(24,21212)IYMD,IHM,SEC,QUAT
21212 FORMAT(2I7,F10.3,4F10.6)
21213 FORMAT(2I7,F10.3,6F20.8)


400   CONTINUE
!
!
! ROTATE SBF UNIT NORMAL VECTORS(NON-MOVING PLATES) TO TOD FRAME
      DO 500 I=1,NFACE-NMOVE
!       if(.not.lforce) then
!       print *,'bfnrm#: ',i,bfnrm1(i),bfnrm2(i),bfnrm3(i)
!       endif
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
! IF MOVEABLE PLATES EXIST COMPUTE THEIR ROTATIONS
      IF(NMOVE.LE.0) GOTO 850
      WRITE(6,*) 'STOP IN GFOATT.  NO MOVEABLE PLATES ANTCIPATED'
      STOP
  850 CONTINUE
!
! COMPUTE UNIT NORMAL VECTORS IN TOD FRAME
      DO 900 I=1,NFACE
         RSUM = SQRT(TDNRM1(I)**2+TDNRM2(I)**2+TDNRM3(I)**2)
         TDNRM1(I) = TDNRM1(I)/RSUM
         TDNRM2(I) = TDNRM2(I)/RSUM
         TDNRM3(I) = TDNRM3(I)/RSUM
  900 END DO
!
! REDEFINE SUN VECTOR
      SUNMAG = SQRT(SUNXYZ(1)**2+SUNXYZ(2)**2+SUNXYZ(3)**2)
      UNTSUN(1)=SUNXYZ(1)/SUNMAG
      UNTSUN(2)=SUNXYZ(2)/SUNMAG
      UNTSUN(3)=SUNXYZ(3)/SUNMAG
! COMPUTE COSINE OF ANGLE BETWEEN TDNORM AND UNTSUN
      IF(LFORCE) THEN
      DO 950 I=1,NFACE
        CTHETA(I)=TDNRM1(I)*UNTSUN(1)+TDNRM2(I)*UNTSUN(2)+              &
     &            TDNRM3(I)*UNTSUN(3)
  950 END DO
!     moda=mod(int(fsstrt),60000)
!     if(.not.lstart.and.moda.eq.0) then
!     if(.not.lstart) then
!     write(79,78) time,betag,somegg,yawtot,(ctheta(ij),ij=1,nface)
!  78 format(f16.3,3f9.3,8f8.4)
!     endif
      ENDIF
!     write(79,78) time,betag,somegg,yawtot,(ctheta(ij),ij=1,nface)
!  78 format(f16.3,3f9.3,8f8.4)
!


!
! CONVERT BETAG AND SOMEGG BACK TO RADIANS
!     BETAG=BETAG*DEGRAD
!     SOMEGG=SOMEGG*DEGRAD
!
! OUTPUT  TELEM FILE INFORMATION
      IF(LTXPRT.AND..NOT.LSTART.AND.LFORCE) THEN
!....INTEGRATION STEP TIME
         CALL UTCET(.FALSE.,1,MJDS,FSEC,FSECU,AA(KA1UT))
         CALL MJDYMD(MJDS,IYMD,IHMS,4)
         IHM= IHMS/100
         ISEC=IHMS-IHM*100
         RSEC=DBLE(ISEC)
         SEC= FSECU+RSEC
         DIYMD=DBLE(IYMD)
         DIHM= DBLE(IHM)
         RSATID=DBLE(ISATID)
!....YAW ALGORITHM INDICATOR
            YTEMP = 1.0D0
!....PERTINENT ANGLES
         CALL ROTQAT(TOTROT,QAT)
         BETAG = -999.D0
         SOMEGG = -999.D0
         YAWTOT = -999.D0
         GEOPNT = 0.D0
         ! jjm 20120531  BETAG  below is undefined - should be BETAP
         ! jjm 20120531  SOMEGG below is undefined - should be SOMEGA
         ! jjm 20120531  YAWTOT below is undefined - should be YAWANG?
         ! jjm 20120531  GEOPNT below is undefined
         WRITE(97) DIYMD,DIHM,SEC,YTEMP,BETAG/DEGRAD,   &
     &             SOMEGG/DEGRAD,YAWTOT,(SGAMMA)/DEGRAD,  &
     &             GEOPNT,QAT(1),QAT(2),QAT(3),QAT(4),XSAT(1), &
     &             XSAT(2),XSAT(3),VSAT(1),VSAT(2),VSAT(3),RSATID
      ENDIF
      RETURN
  999 WRITE(6,*)'SAT ID DOES NOT MATCH ANY ID in ISATID ARRAY'
      STOP 16
      END
