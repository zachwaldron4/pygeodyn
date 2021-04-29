!$ROTXTR
      SUBROUTINE ROTXTR(MJDSEC,FSEC,LEQC,EQN,SCRTCH,THETAG,COSTHG,      &
     &                  SINTHG,NM,AA,ACOSW,BSINW,ANGWT,WT,ACOEFW,       &
     &                  BCOEFW,II,INDX)
!********1*********2*********3*********4*********5*********6*********7**
! ROTXTR           86/12/24            8701.0    PGMR - D. ROWLANDS
!
! FUNCTION:  COMPUTE THE ANGLE BETWEEN THE IAU VECTOR AND
!            THE PRIME MERIDIAN FOR A PLANET AT
!            A VECTOR OF TIMES OF LENGTH "NM".
!            ALSO COMPUTE THE COSINES AND SINES OF THIS
!            ANGLE. THIS SUBROUTINE PARALLELS GRHRAN. IT
!            THEREFORE HAS DUMMY ARGUMENTS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    MODIFIED JULIAN DAY SECONDS OF THE VECTOR
!                      OF TIMES OF INTEREST (EPHEMERIS TIME)
!   FSEC     I    A    THE FRACTION OF SECONDS OF THE VECTOR OF
!                      TIMES OF INTEREST (EPHEMERIS TIME)
!   LEQC     I    S    .TRUE. IF IT IS NECCESSARY TO CALCULATE EQN
!                      OF THE EQUINOX IN ROUTINE
!   EQN     I/O   A    EQUATION OF THE EQUINOX (INPUT IF LEQC=.FALSE.)
!                      EQN OF THE EQUINOX (OUTPUT IF LEQC=.TRUE.)
!   SCRTCH  I/O    A   5*NM
!   THETAG   O     A   VECTOR OF VALUES OF RIGHT ASCENSION OF
!                      GREENWICH
!   COSTHG   O     A   COSINES OF THETAG
!   SINTHG   O     A   SINES OF THETAG
!   NM       I     S   NUMBER OF TIMES OF INTEREST
!   AA      I/O    A   DYNAMIC REAL SPACE
!   ACOSW   I/O    A   ARRAY FOR SINE TIME DEP TERMS FOR PLANET ROTATION
!   BSINW   I/O    A   ARRAY FOR COSINE TIME DEP TERMS FOR PLANET ROTATI
!   ANGWT   I/O    A   ARRAY FOR FREQUENCIES FOR TIME DEP PLANET ROTATIO
!   WT      I/O    A   ANGLE WT
!   II       I     A   DYNANIC INTEGER ARRAY
!   INDX     I     S   INDEX TO PLANETARY BODY
!                      =1 PRIMARY BODY
!                      =2 SECOND BODY (IN A BIBARY ASTEROID CASE)
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
      DIMENSION FSEC(NM),THETAG(NM),COSTHG(NM),SINTHG(NM),EQN(NM),      &
     &          SCRTCH(NM,11),AA(*)
      DIMENSION ACOSW(*),BSINW(*),ANGWT(*),WT(*)
      DIMENSION ACOEFW(*),BCOEFW(*)
      DIMENSION XLLIB(3),XLLIBV(3)
      DIMENSION II(*)
      COMMON/AXIS/LINTAX
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
      COMMON/CORA06/KPRMV ,KPNAME,KPRMV0,KPRMVC,KPRMVP,KPRMSG,KPARVR,   &
     &              KPRML0,KPDLTA,KSATCV,KSTACV,KPOLCV,KTIDCV,KSUM1 ,   &
     &              KSUM2 ,KGPNRA,KGPNRM,KPRSG0,KCONDN,KVELCV,          &
     &              KSL2CV,KSH2CV,KTIEOU,KPRMDF,NXCA06
      COMMON/CORA07/KELEVT,KSRFEL,KTINT,KFSCSD,KXTIM,KFSDM,             &
     & KSGM1,KSGM2,KNOISE,KEPHMS,NXCA07
      COMMON/CORI07/KSMSA1,KSMSA2,KSMSA3,KSMST1,KSMST2,KSMST3,KSMTYP,   &
     &              KSMT1,KSMT2,KMSDM,KIOUT,KISUPE,NXCI07
      COMMON/CRDDIM/NDCRD2,NDCRD3,NDCRD4,NDCRD5,NDCRD6,NDCRD7,          &
     &              NDCRD8,NDCRD9,NXCRDM
      COMMON/CTHDXT/THDXT
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/EPHSET/EMFACT,DTENPD,DTINPD,FSC1EN,FSCENP(2),FSCINP(4),    &
     &   FSDINP(4),XEPHST
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/IEPHM2/ISUPL,ICBODY,ISEQB(2),MAXDEG,ITOTSE,IREPL(988), &
     &              NXEPH2
      COMMON/LCBODY/LEARTH,LMOON,LMARS
      COMMON/LDUAL/LASTSN,LSTSNX,LSTSNY
      COMMON/LEPHM2/LXEPHM
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
      COMMON/THXTGC/THXTGC(3,2)
      COMMON/XTRAOR/T0XTRO,T0XTRO2,CVENUS,CLIBMN,CMARS,CMERC,XXTRAO
      COMMON/XPCNT /NXTRAC,NXFLAG,NXALLC,NCPPER,NMINRT,NDPOR,NXXTRA

!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************

      IF (CMARS > 1.9D0 .AND. CMARS < 2.1D0) THEN
          CALL ROTYTR(MJDSEC, FSEC, THETAG, NM, THTDX)
          DO I = 1, NM
              COSTHG(I) = COS(THETAG(I))
              SINTHG(I) = SIN(THETAG(I))
          END DO
          RETURN
      END IF

      ! GET EQN OF EQUINOX IF NECESSARY
      IF (LEQC) THEN
          CALL BUFXTR(MJDSEC, FSEC(1), MJDSEC, FSEC(NM), &
                      1, MJDDUM, FSCDM, LDUM, AA)

          IF (LXEPHM) THEN
              DO IJ = 1, ICBODY
                CALL BUFSUP(AA, MJDSEC, FSEC(1), MJDSEC, FSEC(NM), &
                            II(KISUPE), AA(KEPHMS), IJ, 1)
              END DO
          END IF
      END IF

      ! GET UT TIMES FROM ET TIMES; PUT IT INTO SCRTCH( ,3)
      IF (.NOT. LMOON) THEN
          XJDSC0 = DBLE(MJDSEC) - FSDINP(1)
          THDXT = THXTGC(2,INDX)
          X0 = THXTGC(1,INDX) + TWOPI
          IF (T0XTRO > 0.D0) THEN
              TIMEC = DBLE(MJDSEC) - T0XTRO
              DO I = 1, NM
                  SCRTCH(I,1) = XJDSC0 + FSEC(I)
                  TIMEAB = TIMEC + FSEC(I)

                  ! RE-COMPUTE ACOSW BSINW
                  SUMAC = 0.D0
                  SUMBS = 0.D0
                  IF (.NOT. LINTAX) THEN
                      ! ADD PERIODIC TERMS
                      ! THIS RELIES ON THE ORDERING OF THE PARAMETERS BEI
                      ! COS_RA, SIN_RA, COS_DEC, SIN_DEC, COS_W, SIN_W
                      DO IPSET = 1, NCPPER-1, 3 ! LOOP OVER SETS OF PERIO
                          DO IP = 5, 6
                              COEF = AA(KPRMV-1 + IPVAL(IXXTRO)-1 &
                                        + 7 + 2*(IPSET-1) + IP)
                              PERIOD = AA(KPPER-1 + IPSET-1 + (IP+1)/2)
                              WT(IPSET) = MOD(TWOPI/PERIOD, TWOPI)

                              IF (MOD(IP,2) == 1) THEN
                                  ACOSW(IPSET) = COEF &
                                          * COS(WT(IPSET) * TIMEAB)
                                  SUMAC = SUMAC + ACOSW(IPSET)
                              ELSE
                                  BSINW(IPSET) = COEF &
                                          * SIN(WT(IPSET) * TIMEAB)
                                  SUMBS = SUMBS + BSINW(IPSET)
                              END IF
                          END DO
                      END DO
                  END IF

                  THETAG(I) = X0 + SCRTCH(I,1)*THXTGC(2,INDX) &
     &                        + THXTGC(3,INDX)*SCRTCH(I,1)**2 &
     &                        + SUMAC + SUMBS

              END DO
          ELSE
              DO I = 1, NM
                  SCRTCH(I,1) = XJDSC0 + FSEC(I)
                  THETAG(I) = X0 + SCRTCH(I,1)*THXTGC(2,INDX)
              END DO
          END IF
      ELSE
          IF (CLIBMN == 1.0D0)THEN
              DO K = 1, NM
                  CALL CMNLIB(MJDSEC, FSEC(K), XLLIB, XLLIBV)
                  THETAG(K) = XLLIB(3)
              END DO
              THXTGC(2,INDX) = XLLIBV(3)
              THDXT = THXTGC(2,INDX)
          ELSE
              XJDSC0 = DBLE(MJDSEC)
              DO I = 1, NM
                  SCRTCH(I,1) = XJDSC0 + FSEC(I)
                  DW = SCRTCH(I,1) / SECDAY - (51544.5D0-TMGDN2)
                  D = DW
                  CALL NLUNCN(D, DW, ALF, ALFD, &
                              DEC, DECD, THETAG(I), THXTGC(2,INDX) )
                  THXTGC(2,INDX) = MOD(THXTGC(2,INDX)+TWOPI, TWOPI)
                  THDXT = THXTGC(2,INDX)
                  THETAG(I) = MOD(THETAG(I), TWOPI)
                  IF (I == 1) THEN
                      TAG2 = THETAG(I) / DEGRAD
                  END IF
              END DO
          END IF
      END IF

      DO I = 1, NM
          THETAG(I) = MOD(THETAG(I), TWOPI)
      END DO

      DO I = 1, NM
          COSTHG(I) = COS(THETAG(I))
      END DO

      DO I = 1, NM
          SINTHG(I) = SIN(THETAG(I))
      END DO

      END
