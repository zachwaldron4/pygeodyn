!$SOLIDT
      SUBROUTINE SOLIDT(TRUE, MJDSEC, FSEC, COSTG, SINTG, NM, XYP,      &
     &                  XYZLUV, PXSPHL, COSLAT, SINLAT, COSLON,         &
     &                  SINLON, NDSTA, LNEG, LETAOF, LDIFF, RADIFF,     &
     &                  DXTIDE, ANGFAC, SPEED, PRMFSD, SCRFR, COSANG,   &
     &                  SINANG, AA, II)
!********1*********2*********3*********4*********5*********6*********7**
! SOLIDT     00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  COMPUTE DISPLACEMENT OF TRACKING STATIONS DUE
!            TO SOLID EARTH TIDES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   TRUE     I    A        EARTH-FIXED TRUE POLE COORDINATES WHERE
!                          EARTH TIDE IS REQUESTED
!   MJDSEC   I    S        INTEGER SECOND TIME SINCE GEODYN REF.TIME
!                          WHICH EVER MEASUREMENT IN BLOCK IS REFERENCED
!   FSEC     I    A        TIME SINCE MJDSEC
!   COSTG    I    A        COSINE OF GREENWICH HR ANGLE AT STATION TIMES
!   SINTG    I    A        SINE OF GREENWICH HR ANGLE AT STATION TIMES
!   NM       I    S        NUMBER OF MEASUREMENT TIMES
!   XYP      I    A        XYP(I,1) IS X OF THE POLE AND XYP(I,2) IS Y
!   XYZLUV   O    A        TRUE POLE COORDINATES CORRECTED FOR
!                          SOLID EARTH TIDES.
!   PXSPHL   O    A        PARTIALS OF STATION POSITION WRT H2&L2
!
! COMMENTS:  TRUE & XYZLUV MAY BE ONE AND THE SAME ARRAY
!
! RESTRICTIONS:  STATIONS IN ARRAY TRUE MUST BE PACKED TIGHTLY
!                AND XYZLUV WILL BE PACKED TIGHTLY
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE

      COMMON/ADJGEO/LADATT,LADEBS,LADABS,LADGBS,LADLOV,LADPOL
      COMMON/BUFOPT/LADRAG,LXTIDE,LPOLTI,NXBUFO
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CEARTH/AEG,AEGSQ,FGINV,FG,EGSQ,EGSQP1,C1,C2,FOURC1,TWOC2,  &
     &              XH2,XL2,WREF,RMEAN,REFC20,REFC40,REFC60,BEG,CBLTC,  &
     &              WAE2,GAMMAA,FSTAR,F4
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CLGVTM/LGVTM,LDPM,LGVTPM,LOPT,LCSDPM,L_MP_IERS2003,        &
     &              L_CS_IERS2003,L_MP_IERS2010,L_CS_IERS2010
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/DYNPOL/DPMEP ,DPMXP ,DPMYP ,DPMXDT,DPMYDT,DPMOPT,DPMXPC,   &
     &              DPMYPC,DPMKF ,DPMC21,DPMS21,DPMPD ,DPMUDK,DPMVDK,   &
     &              DPMOCP,DPMNLD,DPMERS,XDYNPL
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/LCBODY/LEARTH,LMOON,LMARS
      COMMON/NPCOM /NPNAME,NPVAL(92),NPVAL0(92),IPVAL(92),IPVAL0(92),   &
     &              MPVAL(28),MPVAL0(28),NXNPCM
      COMMON/NPCOMX/IXARC ,IXSATP,IXDRAG,IXSLRD,IXACCL,IXGPCA,IXGPSA,   &
     &              IXAREA,IXSPRF,IXDFRF,IXEMIS,IXTMPA,IXTMPC,IXTIMD,   &
     &              IXTIMF,IXTHTX,IXTHDR,IXOFFS,IXBISA,IXFAGM,IXFAFM,   &
     &              IXATUD,IXRSEP,IXACCB,IXDXYZ,IXGPSBW,IXCAME,IXBURN,  &
     &              IXGLBL,IXGPC ,IXGPS, IXTGPC,IXTGPS,IXGPCT,IXGPST,   &
     &              IXTIDE,IXETDE,IXOTDE,IXOTPC,IXOTPS,IXLOCG,IXKF  ,   &
     &              IXGM  ,IXSMA ,IXFLTP,IXFLTE,IXPLTP,IXPLTV,IXPMGM,   &
     &              IXPMJ2,IXVLIT,IXEPHC,IXEPHT,IXH2LV,IXL2LV,IXOLOD,   &
     &              IXPOLX,IXPOLY,IXUT1 ,IXPXDT,IXPYDT,IXUTDT,IXVLBI,   &
     &              IXVLBV,IXXTRO,IXBISG,IXSSTF,IXFGGM,IXFGFM,IXLNTM,   &
     &              IXLNTA,IX2CCO,IX2SCO,IX2GM ,IX2BDA,IXRELP,IXJ2SN,   &
     &              IXGMSN,IXPLNF,IXPSRF,IXANTD,IXTARG,                 &
     &              IXSTAP,IXSSTC,IXSSTS,IXSTAV,IXSTL2,                 &
     &              IXSTH2,IXDPSI,IXEPST,IXCOFF,IXTOTL,NXNPCX
      COMMON/PLNETI/IPLNET(999),IPLNIN(999),MPLNGD(999),MPLNGO(999),   &
     &              IPLNZ(999),NXPLNI
      COMMON/SETDI /NSETDP,NFRSED,NFRSDI,NFRSLN,NXSETI
      COMMON/SETDR /H20,XL20,DH2DP,DL2DP,H3,XL3,DHID,DLID,DHIS,DLIS,    &
     &              XLID,XLISD

      DOUBLE PRECISION, PARAMETER :: GAV = 9.81D0
      DOUBLE PRECISION, PARAMETER :: SSECRRAD = 0.484813681109536D-5

      DOUBLE PRECISION :: XYZLUV(NDSTA,3), TRUE(NDSTA,3), FSEC(NM)
      DOUBLE PRECISION :: COSTG(NM), SINTG(NM), XYP(NM,2)
      DOUBLE PRECISION :: PXSPHL(NM,3,NSETDP)

      ! THESE NEED TO BE TIME DEPENDENT FOR ALTIMETRY
      DOUBLE PRECISION :: COSLAT(NM), SINLAT(NM), COSLON(NM), SINLON(NM)
      DOUBLE PRECISION :: ANGFAC(5,NFRSED), SPEED(NFRSED)
      DOUBLE PRECISION :: PRMFSD(4,NFRSED), SCRFR(4,NFRSED)
      DOUBLE PRECISION :: COSANG(NM,NFRSED), SINANG(NM,NFRSED)
      DOUBLE PRECISION :: POS(4,2,2), FSP(2), SCRP(2), EF(2), ZF(2)
      DOUBLE PRECISION :: TF(2), RPMAT(3,3)
      DOUBLE PRECISION :: AA(*)
      INTEGER :: II(*)
      DOUBLE PRECISION :: XSUN(3), XMON(3), DXTIDE(3,MINTIM), XCRSTA(3)

      ! pd %%%%%% if LDIFF is FALSE nothing happens to RADIFF
      DOUBLE PRECISION :: RADIFF(NDSTA)

      INTEGER :: NDSTA, MJDSEC, NM, I, IK, IPPT, IT, IXPTK, J, NM3, NM6
      INTEGER :: NZ
      LOGICAL :: LNEG, LETAOF, LDIFF, LPART, LPART2
      DOUBLE PRECISION :: CONSPT, COS2PHI, COSE, COSLA, COSP2, COSPHI
      DOUBLE PRECISION :: COST, COSTWOLA, COSZ, DE, DEMDLD, DEMON
      DOUBLE PRECISION :: DESDLD, DESUN, Djxp, Djyp, DN, DNMDLD, DNMON
      DOUBLE PRECISION :: DNSDLD, DNSUN, DP2MDH, DP2MDL, DP2SDH, DP2SDL
      DOUBLE PRECISION :: DP3MDH, DP3MDL, DP3SDH, DP3SDL, DR, DRMDHD
      DOUBLE PRECISION :: DRMON, DRSDHD, DRSUN, DX2MDL, DX2SDL, DX3MDL
      DOUBLE PRECISION :: DX3SDL, FAC2MON, FAC2SUN, FAC3MON, FAC3SUN
      DOUBLE PRECISION :: FRAC, H2, P2MON, P2SUN, P3MON, P3SUN, RMI
      DOUBLE PRECISION :: RMON, RMON2, RSI, RSTA2, RSTA4, RSTAIN, RSUN
      DOUBLE PRECISION :: RSUN2, S10, S4, S5, S6, S7, S8, S9, SCM, SCMON
      DOUBLE PRECISION :: SCS, SCSUN, SINE, SINLA, SINP2, SINPHI, SINT
      DOUBLE PRECISION :: SINTWOLA, SINZ, STIME, TCORR, TDIF, TEST
      DOUBLE PRECISION :: TTIME, X2MON, X2SUN, X3MON, X3SUN, XL1, XM
      DOUBLE PRECISION :: XMI, XS, XSI, XXL2, YM, YMI, YS, YSI, ZMI, ZSI

!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************

      LPART2 = (NPVAL0(IXSTL2) > 0) .OR. (NPVAL0(IXSTH2) > 0)
      IF (NPVAL0(IXH2LV) > 0) THEN
          LPART2 = .TRUE.
      END IF
      LPART = LETAOF .AND. LSTINR .AND. LPART2
      CONSPT = -WREF * WREF * RMEAN * RMEAN / GAV
      NM6 = 6 * NM
      NM3 = 3 * NM

      ! %%%%%%%%%%%%%%%% NEED TO DO SOMETHING ABOUT CASE LNEG=.TRUE.

      ! ZERO OUT THE ARRAY WHERE THE PARTIALS WRT SOLIDT EARTH TIDE
      ! DISPLACEMENT PARAMETERS WILL BE SUMMED.

      NZ = NSETDP * NM * 3
      DO I = 1, NZ
          PXSPHL(I,1,1) = 0.0D0
      END DO

      ! GET ELEMENTS OF PRECESSION SINCE 1950 FOR FIRST & LAST TIMES
      FSP(1) = FSEC(1)
      FSP(2) = FSEC(NM)
      CALL PRECSS(MJDSEC, FSP, EF, ZF, TF, SCRP, 2)

      ! GET MEAN UNIT VECTOR & RADIUS OF MOON&SUN AT FIRST AND LAST TIMES
      DO IT = 1, 2
          ! FORM PRECESSION MATRIX
          SINE = SIN(EF(IT))
          COSE = SQRT(1.0D0-SINE*SINE)
          SINZ = SIN(ZF(IT))
          COSZ = SQRT(1.0D0-SINZ*SINZ)
          SINT = SIN(TF(IT))
          COST = SQRT(1.0D0-SINT*SINT)
          RPMAT(1,1) = COSZ*COST*COSE - SINZ*SINE
          RPMAT(2,1) = SINZ*COST*COSE + COSZ*SINE
          RPMAT(3,1) = SINT * COSE
          RPMAT(1,2) = -COSZ*COST*SINE - SINZ*COSE
          RPMAT(2,2) = -SINZ*COST*SINE + COSZ*COSE
          RPMAT(3,2) = -SINT * SINE
          RPMAT(1,3) = -COSZ * SINT
          RPMAT(2,3) = -SINZ * SINT
          RPMAT(3,3) = COST

          ! GET UNIT VECTOR AND RADIUS OF MOON AND SUN
          CALL PLANPO(MJDSEC, FSP(IT), .FALSE., .FALSE., AA, II)

          ! IN INTERPLANTARY CASE TRANSLATE CIG COORDINATES OF MOON & SUN
          ! TO TRACKING BODY COORDINATES
          IK = 11
          IF (ICBDGM /= ITBDGM) THEN
              IXPTK = IPLNIN(ITBDGM)
              DO J = 1, 3
                  BDSTAT(J,IK) = BDSTAT(J,IK) - BDSTAT(J,IXPTK)
                  BDSTAT(J,8) = BDSTAT(J,8) - BDSTAT(J,IXPTK)
              END DO
          END IF

          ! MOON MEAN
          POS(1,IT,1) = RPMAT(1,1)*BDSTAT(1,IK)                         &
     &                + RPMAT(1,2)*BDSTAT(2,IK)                         &
     &                + RPMAT(1,3)*BDSTAT(3,IK)
          POS(2,IT,1) = RPMAT(2,1)*BDSTAT(1,IK)                         &
     &                + RPMAT(2,2)*BDSTAT(2,IK)                         &
     &                + RPMAT(2,3)*BDSTAT(3,IK)
          POS(3,IT,1) = RPMAT(3,1)*BDSTAT(1,IK)                         &
     &                + RPMAT(3,2)*BDSTAT(2,IK)                         &
     &                + RPMAT(3,3)*BDSTAT(3,IK)

          ! SUN MEAN
          POS(1,IT,2) = RPMAT(1,1)*BDSTAT(1,8) + RPMAT(1,2)*BDSTAT(2,8) &
     &                + RPMAT(1,3)*BDSTAT(3,8)
          POS(2,IT,2) = RPMAT(2,1)*BDSTAT(1,8) + RPMAT(2,2)*BDSTAT(2,8) &
     &                + RPMAT(2,3)*BDSTAT(3,8)
          POS(3,IT,2) = RPMAT(3,1)*BDSTAT(1,8) + RPMAT(3,2)*BDSTAT(2,8) &
     &                + RPMAT(3,3)*BDSTAT(3,8)

          ! STORE UNIT VECTOR AND NORMALIZE
          POS(4,IT,1) = POS(1,IT,1)*POS(1,IT,1)                         &
     &                + POS(2,IT,1)*POS(2,IT,1)                         &
     &                + POS(3,IT,1)*POS(3,IT,1)
          POS(4,IT,2) = POS(1,IT,2)*POS(1,IT,2)                         &
     &                + POS(2,IT,2)*POS(2,IT,2)                         &
     &                + POS(3,IT,2)*POS(3,IT,2)
          POS(4,IT,1) = SQRT(POS(4,IT,1))
          POS(4,IT,2) = SQRT(POS(4,IT,2))
          POS(1,IT,1) = POS(1,IT,1) / POS(4,IT,1)
          POS(2,IT,1) = POS(2,IT,1) / POS(4,IT,1)
          POS(3,IT,1) = POS(3,IT,1) / POS(4,IT,1)
          POS(1,IT,2) = POS(1,IT,2) / POS(4,IT,2)
          POS(2,IT,2) = POS(2,IT,2) / POS(4,IT,2)
          POS(3,IT,2) = POS(3,IT,2) / POS(4,IT,2)
      END DO

      XSI = POS(1,2,2) - POS(1,1,2)
      YSI = POS(2,2,2) - POS(2,1,2)
      ZSI = POS(3,2,2) - POS(3,1,2)
      RSI = POS(4,2,2) - POS(4,1,2)
      XMI = POS(1,2,1) - POS(1,1,1)
      YMI = POS(2,2,1) - POS(2,1,1)
      ZMI = POS(3,2,1) - POS(3,1,1)
      RMI = POS(4,2,1) - POS(4,1,1)

      ! GET INTERPOLATION COEFFICIENTS FOR PLANET POSITIONS
      TDIF = FSEC(NM) - FSEC(1)
      TTIME = 0.0D0
      IF (NM > 1) THEN
          TTIME = 1.0D0 / TDIF
      END IF

      DO I = 1, NM
          ! STATION RADIUS SQUARED AND TO THE 4TH AND OVER ONE
          RSTA2 = TRUE(I,1)*TRUE(I,1) + TRUE(I,2)*TRUE(I,2)             &
     &          + TRUE(I,3)*TRUE(I,3)
          RSTA4 = RSTA2 * RSTA2
          RSTAIN = 1.0D0 / SQRT(RSTA2)
          FRAC = (FSEC(I)-FSEC(1)) * TTIME
          XS = POS(1,1,2) + FRAC*XSI
          YS = POS(2,1,2) + FRAC*YSI
          RSUN = POS(4,1,2) + FRAC*RSI
          XSUN(3) = POS(3,1,2) + FRAC*ZSI
          XM = POS(1,1,1) + FRAC*XMI
          YM = POS(2,1,1) + FRAC*YMI
          RMON = POS(4,1,1) + FRAC*RMI
          XMON(3) = POS(3,1,1) + FRAC*ZMI
          XSUN(1) =  COSTG(I)*XS + SINTG(I)*YS
          XSUN(2) = -SINTG(I)*XS + COSTG(I)*YS
          XMON(1) =  COSTG(I)*XM + SINTG(I)*YM
          XMON(2) = -SINTG(I)*XM + COSTG(I)*YM

          RSUN2 = RSUN * RSUN
          RMON2 = RMON * RMON

          SCS = TRUE(I,1)*XSUN(1) + TRUE(I,2)*XSUN(2)                   &
     &        + TRUE(I,3)*XSUN(3)
          SCSUN = SCS * RSTAIN
          SCM = TRUE(I,1)*XMON(1) + TRUE(I,2)*XMON(2)                   &
     &        + TRUE(I,3)*XMON(3)
          SCMON = SCM * RSTAIN

          ! COMPUTATION OF NEW H2 AND L2
          ! ----------------------------
          COSP2 = COSLAT(I)**2
          H2 = H20 - DH2DP * (1.D0-1.5D0*COSP2)
          XXL2 = XL20 + DL2DP * (1.D0-1.5D0*COSP2)

          ! P2-TERM
          ! -------
          P2SUN = (1.5D0*H2-3.D0*XXL2)*SCSUN**2 - 0.5D0*H2
          P2MON = (1.5D0*H2-3.D0*XXL2)*SCMON**2 - 0.5D0*H2

          ! P3-TERM
          ! -------
          P3SUN = 2.5D0*(H3-3.D0*XL3)*SCSUN**3 + 1.5D0*(XL3-H3)*SCSUN
          P3MON = 2.5D0*(H3-3.D0*XL3)*SCMON**3 + 1.5D0*(XL3-H3)*SCMON

          ! TERM IN DIRECTION OF SUN/MOON VECTOR
          ! ------------------------------------
          X2SUN = 3.D0 * XXL2 * SCSUN
          X2MON = 3.D0 * XXL2 * SCMON
          X3SUN = XL3 * (7.5D0*SCSUN**2-1.5D0)
          X3MON = XL3 * (7.5D0*SCMON**2-1.5D0)

          ! FACTORS FOR SUN/MOON
          ! --------------------
          FAC2SUN = RATIOM(2) * AEG * (AEG/RSUN)**3
          FAC2MON = RATIOM(1) * AEG * (AEG/RMON)**3
          FAC3SUN = FAC2SUN * (AEG/RSUN)
          FAC3MON = FAC2MON * (AEG/RMON)

          ! TOTAL DISPLACEMENT
          ! ------------------
          DO J = 1, 3
              DXTIDE(J,I) = FAC2SUN*( X2SUN*XSUN(J)                     &
     &                           + P2SUN*TRUE(I,J)*RSTAIN )             &
     &                    + FAC2MON*( X2MON*XMON(J)                     &
     &                           + P2MON*TRUE(I,J)*RSTAIN)              &
     &                    + FAC3SUN*( X3SUN*XSUN(J)                     &
     &                           + P3SUN*TRUE(I,J)*RSTAIN)              &
     &                    + FAC3MON*( X3MON*XMON(J)                     &
     &                           + P3MON*TRUE(I,J)*RSTAIN)
          END DO


          ! THIS SECTION OF CODE GIVES THE OUT-OF-PHASE CORRECTIONS
          ! INDUCED BY MANTLE INELASTICITY IN THE DIURNAL BAND
          !       INPUT : XSTA,XSUN,XMON,FAC2SUN,FAC2MON
          !      OUTPUT : XCRSTA
          SINPHI = SINLAT(I)
          COSPHI = COSLAT(I)
          COS2PHI = COSLAT(I)**2 - SINLAT(I)**2
          SINLA = SINLON(I)
          COSLA = COSLON(I)
          DRSUN = -3.D0 * DHID * SINPHI * COSPHI * FAC2SUN * XSUN(3)    &
     &                * (XSUN(1)*SINLA-XSUN(2)*COSLA)
          DRMON = -3.D0 * DHID * SINPHI * COSPHI * FAC2MON * XMON(3)    &
     &                * (XMON(1)*SINLA-XMON(2)*COSLA)
          DNSUN = -3.D0 * DLID * COS2PHI * FAC2SUN * XSUN(3)            &
     &                * (XSUN(1)*SINLA-XSUN(2)*COSLA)
          DNMON = -3.D0 * DLID * COS2PHI * FAC2MON * XMON(3)            &
     &                * (XMON(1)*SINLA-XMON(2)*COSLA)
          DESUN = -3.D0 * DLID * SINPHI * FAC2SUN * XSUN(3)             &
     &                * (XSUN(1)*COSLA+XSUN(2)*SINLA)
          DEMON = -3.D0 * DLID * SINPHI * FAC2MON * XMON(3)             &
     &                * (XMON(1)*COSLA+XMON(2)*SINLA)
          DR = DRSUN + DRMON
          DN = DNSUN + DNMON
          DE = DESUN + DEMON
          XCRSTA(1) = DR*COSLA*COSPHI - DE*SINLA - DN*SINPHI*COSLA
          XCRSTA(2) = DR*SINLA*COSPHI + DE*COSLA - DN*SINPHI*SINLA
          XCRSTA(3) = DR*SINPHI + DN*COSPHI
          DO J = 1, 3
              DXTIDE(J,I) = DXTIDE(J,I) + XCRSTA(J)
          END DO

          ! THIS SECTION OF CODE GIVES THE OUT-OF-PHASE CORRECTIONS
          ! INDUCED BY MANTLE INELASTICITY IN THE DIURNAL BAND
          !       INPUT : XSTA,XSUN,XMON,FAC2SUN,FAC2MON
          !      OUTPUT : XCORSTA
          COSTWOLA = COSLA**2 - SINLA**2
          SINTWOLA = 2.D0 * COSLA * SINLA
          DRSUN = -.75D0 * DHIS * COSPHI**2 * FAC2SUN                   &
     &            * ((XSUN(1)**2-XSUN(2)**2)*SINTWOLA                   &
     &               -2.D0*XSUN(1)*XSUN(2)*COSTWOLA)
          DRMON = -.75D0 * DHIS * COSPHI**2 * FAC2MON                   &
     &            * ((XMON(1)**2-XMON(2)**2)*SINTWOLA                   &
     &               -2.D0*XMON(1)*XMON(2)*COSTWOLA)
          DNSUN = 1.5D0 * DLIS * SINPHI * COSPHI * FAC2SUN              &
     &            * ((XSUN(1)**2-XSUN(2)**2)                            &
     &               *SINTWOLA-2.D0*XSUN(1)*XSUN(2)*COSTWOLA)
          DNMON = 1.5D0 * DLIS * SINPHI * COSPHI * FAC2MON              &
     &            * ((XMON(1)**2-XMON(2)**2)                            &
     &               *SINTWOLA-2.D0*XMON(1)*XMON(2)*COSTWOLA)
          DESUN = -1.5D0 * DLIS * COSPHI * FAC2SUN                      &
     &            * ((XSUN(1)**2-XSUN(2)**2)                            &
     &               *COSTWOLA+2.D0*XSUN(1)*XSUN(2)*SINTWOLA)
          DEMON = -1.5D0 * DLIS * COSPHI * FAC2MON                      &
     &            * ((XMON(1)**2-XMON(2)**2)                            &
     &               *COSTWOLA+2.D0*XMON(1)*XMON(2)*SINTWOLA)
          DR = DRSUN + DRMON
          DN = DNSUN + DNMON
          DE = DESUN + DEMON
          XCRSTA(1) = DR*COSLA*COSPHI - DE*SINLA - DN*SINPHI*COSLA
          XCRSTA(2) = DR*SINLA*COSPHI + DE*COSLA - DN*SINPHI*SINLA
          XCRSTA(3) = DR*SINPHI + DN*COSPHI
          DO J = 1, 3
              DXTIDE(J,I) = DXTIDE(J,I) + XCRSTA(J)
          END DO

          !**************************************************************

          ! THIS SECTION OF CODE GIVES THE CORRECTIONS INDUCED BY THE
          ! LATITUDE DEPENDENCE GIVEN BY L^(1) IN MAHTEWS ET AL (1991)
          !       INPUT : XSTA,XSUN,XMON,FAC3SUN,FAC3MON
          !      OUTPUT : XCORSTA
          SINP2 = SINLAT(I)**2

          ! FOR THE DIURNAL BAND
          XL1 = XLID
          DNSUN = -XL1 * SINP2 * FAC2SUN * XSUN(3)                      &
     &            * (XSUN(1)*COSLA+XSUN(2)*SINLA)
          DNMON = -XL1 * SINP2 * FAC2MON * XMON(3)                      &
     &            * (XMON(1)*COSLA+XMON(2)*SINLA)
          DESUN = XL1 * SINPHI * (COSP2-SINP2) * FAC2SUN * XSUN(3)      &
     &            * (XSUN(1)*SINLA-XSUN(2)*COSLA)
          DEMON = XL1 * SINPHI * (COSP2-SINP2) * FAC2MON * XMON(3)      &
     &            * (XMON(1)*SINLA-XMON(2)*COSLA)
          DE = 3.D0 * (DESUN+DEMON)
          DN = 3.D0 * (DNSUN+DNMON)
          XCRSTA(1) = -DE*SINLA - DN*SINPHI*COSLA
          XCRSTA(2) = DE*COSLA - DN*SINPHI*SINLA
          XCRSTA(3) = DN * COSPHI

          ! FOR THE SEMI-DIURNAL BAND
          XL1 = XLISD
          COSTWOLA = COSLA**2 - SINLA**2
          SINTWOLA = 2.D0 * COSLA * SINLA
          DNSUN = -XL1 * 0.5D0 * SINPHI * COSPHI * FAC2SUN              &
     &            * ((XSUN(1)**2-XSUN(2)**2)                            &
     &               *COSTWOLA+2.D0*XSUN(1)*XSUN(2)*SINTWOLA)
          DNMON = -XL1 * 0.5D0 * SINPHI * COSPHI * FAC2MON              &
     &            * ((XMON(1)**2-XMON(2)**2)                            &
     &               *COSTWOLA+2.D0*XMON(1)*XMON(2)*SINTWOLA)
          DESUN = -XL1 * 0.5D0 * SINP2 * COSPHI * FAC2SUN               &
     &            * ((XSUN(1)**2-XSUN(2)**2)                            &
     &               *SINTWOLA-2.D0*XSUN(1)*XSUN(2)*COSTWOLA)
          DEMON = -XL1 * 0.5D0 * SINP2 * COSPHI * FAC2MON               &
     &            * ((XMON(1)**2-XMON(2)**2)                            &
     &               *SINTWOLA-2.D0*XMON(1)*XMON(2)*COSTWOLA)
          DE = 3.D0 * (DESUN+DEMON)
          DN = 3.D0 * (DNSUN+DNMON)
          XCRSTA(1) = XCRSTA(1) - DE*SINLA - DN*SINPHI*COSLA
          XCRSTA(2) = XCRSTA(2) + DE*COSLA - DN*SINPHI*SINLA
          XCRSTA(3) = XCRSTA(3) + DN*COSPHI
          DO J = 1, 3
              DXTIDE(J,I) = DXTIDE(J,I) + XCRSTA(J)
          END DO

          ! COMPUTE THE POLE TIDE DISPLACEMENT COMPONENTS
          ! AND STORE IN S4-S6.

          IF (LPOLTI) THEN
              ! COMPUTE THE MEAN POLE.
              CALL IERSMP(MJDSEC, FSEC(I), Djxp, Djyp)
              ! DPMUDK CORRESPONDS TO M_1, DPMVDK CORRESPONDS TO -M_2 (NO
              ! THE NEGATIVE)
              DPMUDK = XYP(I,1) - Djxp
              DPMVDK = XYP(I,2) - Djyp

              ! DELTA PHI : CHANGE IN LAT. DUE TO XP,YP
              S7 = -DPMVDK*SINLON(I) + DPMUDK*COSLON(I)
              ! CORRECTION ALONG LATITUDE
              S8 = CONSPT * S7 * (1.0D0-2.0D0*SINLAT(I)**2)
              S4 = S8 * XXL2
              ! CORRECTION ALONG LONGITUDE
              S9 = -CONSPT * SINLAT(I)                                  &
     &             * (DPMVDK*COSLON(I)+DPMUDK*SINLON(I))
              S5 = S9 * XXL2
              ! CORRECTION ALONG HEIGHT
              S10 = CONSPT * S7 * COSLAT(I) * SINLAT(I)
              S6 = S10 * H2

              ! ROTATE THE CORRECTIONS INTO THE XYZ CARTESIAN AND
              ! UPDATE THE INPUT/OUTPUT ARRAY TRUE.
              DXTIDE(1,I) = -S4*SINLAT(I)*COSLON(I) - S5*SINLON(I)      &
     &                      + S6*COSLAT(I)*COSLON(I) + DXTIDE(1,I)
              DXTIDE(2,I) = -S4*SINLAT(I)*SINLON(I) + S5*COSLON(I)      &
     &                      + S6*COSLAT(I)*SINLON(I) + DXTIDE(2,I)
              DXTIDE(3,I) = S4*COSLAT(I) + S6*SINLAT(I) + DXTIDE(3,I)
          END IF

          IF (.NOT. LPART) THEN
              CYCLE
          END IF

          !**************************************************************

          ! PARTIALS

          DP2SDH = 1.5D0*SCSUN**2 - 0.5D0
          DP2MDH = 1.5D0*SCMON**2 - 0.5D0
          DP2SDL = -3.D0**SCSUN**2
          DP2MDL = -3.D0**SCMON**2
          DX2SDL = 3.D0 * SCSUN
          DX2MDL = 3.D0 * SCMON
          DP3SDH = 2.5D0*SCSUN**3 - 1.5D0*SCSUN
          DP3MDH = 2.5D0*SCMON**3 - 1.5D0*SCMON
          DP3SDL = -7.5D0*SCSUN**3 + 1.5D0*SCSUN
          DP3MDL = -7.5D0*SCMON**3 + 1.5D0*SCMON
          DX3SDL = 7.5D0*SCSUN**2 - 1.5D0
          DX3MDL = 7.5D0*SCMON**2 - 1.5D0

          DO J = 1, 3
              ! H20 PARTIALS
              PXSPHL(I,J,1) = (TRUE(I,J)*RSTAIN)                        &
     &                * (DP2SDH*FAC2SUN+DP2MDH*FAC2MON)
              ! L20 PARTIALS
              PXSPHL(I,J,2) = (TRUE(I,J)*RSTAIN)                        &
     &                        *(DP2SDL*FAC2SUN+DP2MDL*FAC2MON)          &
     &                  + FAC2SUN*(XSUN(J))*DX2SDL                      &
     &                  + FAC2MON*(XMON(J))*DX2MDL
              ! H2 LAT DEP PARTIALS
              PXSPHL(I,J,3) = -PXSPHL(I,J,1) * (1.D0-1.5D0*COSP2)
              ! L2 LAT DEP PARTIALS
              PXSPHL(I,J,4) =  PXSPHL(I,J,2) * (1.D0-1.5D0*COSP2)
              ! H3 PARTIALS
              PXSPHL(I,J,5) = (TRUE(I,J)*RSTAIN) &
     &                * (DP3SDH*FAC3SUN+DP3MDH*FAC3MON)
              ! L3 PARTIALS
              PXSPHL(I,J,6) = (TRUE(I,J)*RSTAIN)                        &
     &                        *(DP3SDL*FAC3SUN+DP3MDL*FAC3MON)          &
     &                  + FAC3SUN*(XSUN(J))*DX3SDL                      &
     &                  + FAC3MON*(XMON(J))*DX3MDL
          END DO


          ! THIS SECTION OF CODE GIVES THE PARTIALS FOR OUT-OF-PHASE
          ! CORRECTIONS INDUCED MANTLE INELASTICITY IN THE DIURNAL BAND
          DRSDHD = -3.D0 * SINPHI * COSPHI * FAC2SUN * XSUN(3)          &
     &             * (XSUN(1)*SINLA-XSUN(2)*COSLA)
          DRMDHD = -3.D0 * SINPHI * COSPHI * FAC2MON * XMON(3)          &
     &             * (XMON(1)*SINLA-XMON(2)*COSLA)
          DNSDLD = -3.D0 * COS2PHI * FAC2SUN * XSUN(3)                  &
     &             * (XSUN(1)*SINLA-XSUN(2)*COSLA)
          DNMDLD = -3.D0 * COS2PHI * FAC2MON * XMON(3)                  &
     &             * (XMON(1)*SINLA-XMON(2)*COSLA)
          DESDLD = -3.D0 * SINPHI * FAC2SUN * XSUN(3)                   &
     &             * (XSUN(1)*COSLA+XSUN(2)*SINLA)
          DEMDLD = -3.D0 * SINPHI * FAC2MON * XMON(3)                   &
     &             * (XMON(1)*COSLA+XMON(2)*SINLA)
          DR = DRSDHD + DRMDHD
          DN = DNSDLD + DNMDLD
          DE = DESDLD + DEMDLD
          PXSPHL(I,1,7) = DR * COSLA * COSPHI
          PXSPHL(I,2,7) = DR * SINLA * COSPHI
          PXSPHL(I,3,7) = DR * SINPHI
          PXSPHL(I,1,8) = -DE*SINLA - DN*SINPHI*COSLA
          PXSPHL(I,2,8) =  DE*COSLA - DN*SINPHI*SINLA
          PXSPHL(I,3,8) =  DN * COSPHI

          ! THIS SECTION OF CODE GIVES THE PARTIALS FOR OUT-OF-PHASE
          ! CORRECTIONS INDUCED MANTLE INELASTICITY IN THE DIURNAL BAND
          DRSUN = -.75D0 * COSPHI**2 * FAC2SUN                          &
     &            * ((XSUN(1)**2-XSUN(2)**2)*SINTWOLA                   &
     &               -2.D0*XSUN(1)*XSUN(2)*COSTWOLA)
          DRMON = -.75D0 * COSPHI**2 * FAC2MON                          &
     &            * ((XMON(1)**2-XMON(2)**2)*SINTWOLA                   &
     &               -2.D0*XMON(1)*XMON(2)*COSTWOLA)
          DNSUN = 1.5D0 * SINPHI * COSPHI * FAC2SUN                     &
     &            * ((XSUN(1)**2-XSUN(2)**2)*SINTWOLA                   &
     &               -2.D0*XSUN(1)*XSUN(2)*COSTWOLA)
          DNMON = 1.5D0 * SINPHI * COSPHI * FAC2MON                     &
     &            * ((XMON(1)**2-XMON(2)**2)*SINTWOLA                   &
     &               -2.D0*XMON(1)*XMON(2)*COSTWOLA)
          DESUN = -1.5D0 * COSPHI * FAC2SUN                             &
     &            * ((XSUN(1)**2-XSUN(2)**2)*COSTWOLA                   &
     &               +2.D0*XSUN(1)*XSUN(2)*SINTWOLA)
          DEMON = -1.5D0 * COSPHI * FAC2MON                             &
     &            * ((XMON(1)**2-XMON(2)**2)*COSTWOLA                   &
     &               +2.D0*XMON(1)*XMON(2)*SINTWOLA)
          DR = DRSUN + DRMON
          DN = DNSUN + DNMON
          DE = DESUN + DEMON
          PXSPHL(I,1,9) = DR * COSLA * COSPHI
          PXSPHL(I,2,9) = DR * SINLA * COSPHI
          PXSPHL(I,3,9) = DR * SINPHI
          PXSPHL(I,1,10) = -DE*SINLA - DN*SINPHI*COSLA
          PXSPHL(I,2,10) =  DE*COSLA - DN*SINPHI*SINLA
          PXSPHL(I,3,10) =  DN * COSPHI

          !--------------------------------------------------------------

          ! THIS SECTION OF CODE GIVES THE PARTILA FOR THE CORRECTIONS
          ! INDUCED BY THE LATITUDE DEPENDENCE GIVEN BY L^(1) IN MAHTEWS
          ! ET AL (1991)

          ! FOR THE DIURNAL BAND
          DNSUN = -SINP2 * FAC2SUN * XSUN(3)                            &
     &            * (XSUN(1)*COSLA+XSUN(2)*SINLA)
          DNMON = -SINP2 * FAC2MON * XMON(3)                            &
     &            * (XMON(1)*COSLA+XMON(2)*SINLA)
          DESUN = SINPHI * (COSP2-SINP2) * FAC2SUN * XSUN(3)            &
     &            * (XSUN(1)*SINLA-XSUN(2)*COSLA)
          DEMON = SINPHI * (COSP2-SINP2) * FAC2MON * XMON(3)            &
     &            * (XMON(1)*SINLA-XMON(2)*COSLA)
          DE = 3.D0 * (DESUN+DEMON)
          DN = 3.D0 * (DNSUN+DNMON)
          PXSPHL(I,1,11) = -DE*SINLA - DN*SINPHI*COSLA
          PXSPHL(I,2,11) =  DE*COSLA - DN*SINPHI*SINLA
          PXSPHL(I,3,11) =  DN * COSPHI

          ! FOR THE SEMI-DIURNAL BAND
          DNSUN = -0.5D0 * SINPHI * COSPHI * FAC2SUN                    &
     &            * ((XSUN(1)**2-XSUN(2)**2)*COSTWOLA                   &
     &               +2.D0*XSUN(1)*XSUN(2)*SINTWOLA)
          DNMON = -0.5D0 * SINPHI * COSPHI * FAC2MON                    &
     &            * ((XMON(1)**2-XMON(2)**2)*COSTWOLA                   &
     &               +2.D0*XMON(1)*XMON(2)*SINTWOLA)
          DESUN = -0.5D0 * SINP2 * COSPHI * FAC2SUN                     &
     &            * ((XSUN(1)**2-XSUN(2)**2)*SINTWOLA                   &
     &               -2.D0*XSUN(1)*XSUN(2)*COSTWOLA)
          DEMON = -0.5D0 * SINP2 * COSPHI * FAC2MON                     &
     &            * ((XMON(1)**2-XMON(2)**2)*SINTWOLA                   &
     &               -2.D0*XMON(1)*XMON(2)*COSTWOLA)
          DE = 3.D0 * (DESUN+DEMON)
          DN = 3.D0 * (DNSUN+DNMON)
          PXSPHL(I,1,12) = -DE*SINLA - DN*SINPHI*COSLA
          PXSPHL(I,2,12) =  DE*COSLA - DN*SINPHI*SINLA
          PXSPHL(I,3,12) =  DN * COSPHI
      END DO

      ! NOW MAKE FREQUENCY DEPENDENT PARAMETER CORRECTIONS

      CALL GCSAST(MJDSEC, FSEC, NM, ANGFAC, SPEED, SCRFR, NFRSED,       &
     &            COSANG, SINANG)
      CALL ST2DIU(NM, NFRSDI, COSLAT, SINLAT, COSLON, SINLON,           &
     &            COSANG, SINANG, PRMFSD, DXTIDE)
      CALL ST2LON(NM, NFRSLN, COSLAT, SINLAT, COSLON, SINLON,           &
     &            COSANG(1,NFRSDI+1), SINANG(1,NFRSDI+1),               &
     &            PRMFSD(1,NFRSDI+1), DXTIDE)
      IF (LPART) THEN
          IPPT = NSETDP - (NFRSDI+NFRSLN)*4 + 1
          CALL PT2DIU(NM, NFRSDI, COSLAT, SINLAT, COSLON, SINLON,       &
     &                COSANG, SINANG, PXSPHL(1,1,IPPT))
          IPPT = NSETDP - NFRSLN*4 + 1
          CALL PT2LON(NM, NFRSLN, COSLAT, SINLAT, COSLON, SINLON,       &
     &                COSANG(1,NFRSDI+1), SINANG(1,NFRSDI+1),           &
     &                PXSPHL(1,1,IPPT))
      END IF

      IF (LDIFF) THEN
          DO I = 1, NM
              TCORR = SQRT(DXTIDE(1,I)*DXTIDE(1,I)                      &
     &                + DXTIDE(2,I)*DXTIDE(2,I)                         &
     &                + DXTIDE(3,I)*DXTIDE(3,I))
              TEST = TRUE(I,1)*DXTIDE(1,I) + TRUE(I,2)*DXTIDE(2,I)      &
     &                + TRUE(I,3)*DXTIDE(3,I)
              IF (TEST < 0.D0) THEN
                  TCORR = -TCORR
              END IF
              IF (LNEG) THEN
                  TCORR = -TCORR
              END IF
              RADIFF(I) = TCORR
          END DO
      END IF

      IF (.NOT. LNEG) THEN
          DO I = 1, NM
              XYZLUV(I,1) = TRUE(I,1) + DXTIDE(1,I)
              XYZLUV(I,2) = TRUE(I,2) + DXTIDE(2,I)
              XYZLUV(I,3) = TRUE(I,3) + DXTIDE(3,I)
          END DO
      ELSE
          DO I = 1, NM
              XYZLUV(I,1) = TRUE(I,1) - DXTIDE(1,I)
              XYZLUV(I,2) = TRUE(I,2) - DXTIDE(2,I)
              XYZLUV(I,3) = TRUE(I,3) - DXTIDE(3,I)
          END DO
      END IF

      END SUBROUTINE
