!$PRXCSS
      SUBROUTINE PRXCSS(MJDSC,FSEC,RAX,DCX,DUM,SCRTCH,NM,DPSI,EPST,&
     & EPSM,AA,INDX)
!********1*********2*********3*********4*********5*********6*********7**
! PRXCSS           86/12/24            8701.0    PGMR - D. ROWLANDS
!
! FUNCTION:  OBTAIN ELEMENTS OF PRECESION FOR A PLANET
!            OTHER THAN EARTH. THIS ROUTINE PARALLELS
!            PRECSS AND THEREFORE HAS A DUMMY ARGUMENT.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSC    I         INTEGER ET SECONDS SINCE GEODYN REFERENCE TIME
!   FSEC     I         REMAINING SECONDS SINCE MJDSC
!   RAX      O
!   DCX      O         ELEMENTS OF PRECESION SINCE 1950.0 OR 2000
!                      IN RADIANS
!   DUM
!   SCRTCH        A    SCRATCH ARRAY. MUST BE AN ARRAY OF LENGTH NM
!   NM       I         # TIMES AT WHICH ELEMENTS ARE REQUESTED
!   INDX     I     S   INDEX TO PLANETARY BODY
!                      =1 PRIMARY BODY
!                      =2 SECOND BODY (IN A BIBARY ASTEROID CASE)
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!      DIMENSION FSEC(NM),SCRTCH(NM),RAX(NM),DCX(NM),SCRCH5(NM,5) ! orig
!  TEMPORARY FIX >>>>
                                                                  ! jjm
!
      DIMENSION AA(*)
      DIMENSION FSEC(NM),SCRTCH(NM),RAX(NM),DCX(NM),SCRCH5(1000,5)
!  TEMPORARY FIX <<<<
      DIMENSION DPSI(NM),EPST(NM),EPSM(NM),VPMEJ2(3),VPMOBE(3),RM(3)
      DIMENSION VPMEEC(3),AUTMOB(3,3),OBMTAU(3,3)
      DIMENSION XLLIB(3),XLLIBV(3)
      INCLUDE 'COMMON_DECL.inc'
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
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
      COMMON/CORA06/KPRMV ,KPNAME,KPRMV0,KPRMVC,KPRMVP,KPRMSG,KPARVR,   &
     &              KPRML0,KPDLTA,KSATCV,KSTACV,KPOLCV,KTIDCV,KSUM1 ,   &
     &              KSUM2 ,KGPNRA,KGPNRM,KPRSG0,KCONDN,KVELCV,          &
     &              KSL2CV,KSH2CV,KTIEOU,KPRMDF,NXCA06
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/IEPHM2/ISUPL,ICBODY,ISEQB(2),MAXDEG,ITOTSE,IREPL(988), &
     &              NXEPH2
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
      COMMON/PRXCON/PRXCON(2,2,2)
      COMMON/EPHSET/EMFACT,DTENPD,DTINPD,FSC1EN,FSCENP(2),FSCINP(4),    &
     &   FSDINP(4),XEPHST
      COMMON/XPCNT /NXTRAC,NXFLAG,NXALLC,NCPPER,NMINRT,NDPOR,NXXTRA
      COMMON/XTRAOR/T0XTRO,T0XTRO2,CVENUS,CLIBMN,CMARS,CMERC,XXTRAO
      COMMON/EPHM/FSECXY(1),XP(261),YP(261),A1UT(261),EP(261),EC(261),  &
     &            FSCFLX(1),FLUXS(36),AVGFLX(36),FLUXM(288),FSECEP(1),  &
     &            EPHP(816),EPHN(96),ELIB(120),BUFT(2700),FSECNP(4)

!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************

      DD0 = 2451545.0D0 - TMGDN1

      ! APPLY PRECESSION ONLY

      ! Due to MJDSEC is related to Geodyn reference time and the
      ! constants PRXCON(2,2,1)for planets are refered to 2430000.5
      DSEC = (TMGDN2-30000.D0) * 86400.0
      OFFSET = DBLE(MJDSC) + DSEC

      DO I = 1, NM
          SCRTCH(I) = FSEC(I) + OFFSET
          IF (LMOON) THEN
              IF (CLIBMN == 1.D0)THEN
                  CALL CMNLIB(MJDSC, FSEC(I), XLLIB, XLLIBV)
                  RAX(I) = XLLIB(1)
                  DCX(I) = XLLIB(2)
                  RTX = XLLIB(3)
              ELSE
                  DW = SCRTCH(I) / 86400.D0 - DD0
                  D = DW
                  CALL NLUNCN(D, DW, RAX(I), RAXD, &
                              DCX(I), DCXD, THT, THTD)
              END IF
          ELSE
              RAX(I) = PRXCON(1,1,INDX) + PRXCON(2,1,INDX)*SCRTCH(I)
              DCX(I) = PRXCON(1,2,INDX) + PRXCON(2,2,INDX)*SCRTCH(I)
              IF(NPVAL0(IXXTRO).GT.0) THEN
              ! ADD PERIODIC TERMS
              ! THIS RELIES ON THE ORDERING OF THE PARAMETERS BEING:
              ! COS_RA, SIN_RA, COS_DEC, SIN_DEC, COS_W, SIN_W
              ! ... LOOP OVER SETS OF PERIODIC TERMS
              TIME4PERIODIC = DBLE(MJDSC) - T0XTRO + FSEC(I)
              DO IPSET = 1, NCPPER-1, 3
                  DO IP = 1, 4
                      COEF = AA(KPRMV-1 + IPVAL(IXXTRO)-1 &
                             + 7 + 2*(IPSET-1) + IP)
                      PERIOD = AA(KPPER-1 + IPSET-1 + (IP+1)/2)

                          IF (MOD(IP,2) == 1) THEN
                              TMP = COEF * COS(2*PI/PERIOD &
                                               * TIME4PERIODIC)
                          ELSE
                              TMP = COEF * SIN(2*PI/PERIOD &
                                               * TIME4PERIODIC)
                          END IF

                      IF (IP == 1 .OR. IP == 2) THEN
                          RAX(I) = RAX(I) + TMP
                      ELSE IF (IP == 3 .OR. IP == 4) THEN
                          DCX(I) = DCX(I) + TMP
                      END IF
                  END DO
              END DO
              ENDIF
          END IF
      END DO

      IF (LMARS .AND. IMRNUT /= 0) THEN
          ! OPTION: OLD MARS PRECESSION + NUTATION

          ! GET MARS NUTATION DPSI AND EPSM
          CALL NUVMCT(MJDSC, FSEC, DPSI, EPST, EPSM, NM, SCRTCH)

          DO I=1,NM
              T = (DBLE(MJDSC)+FSEC(I))/86400.D0 - DD0

              ! GOING TO MARS MEAN ORBIT + MARS VERNAL EQUIMOX SYSTEM
              ! FOR ADDING MARS NUTATION
              CALL EPSMRR(T, ARMN, EPSMM, RM, VPMOBE)
              CALL MOBIAU(RM, VPMOBE, OBMTAU, AUTMOB)

              ! OBTAIN MARS POLE VECTOR IN EARTH J2000
              ! (IT APPLIES MARS PRECESSION)
              ! THEN GOING TO MARS MEAN ORBIT + MARS VERNAL EQUIMOX
              ! SYSTEM FOR ADDING MARS NUTATION
              CALL PMEQJ2(T, VPMEJ2, VPMEEC)

              ! OBTAIN UPDATED RIGHT ASCENSION AND DECLINATION OF
              ! MARS POLE (APPLY MARS PRECESSION AND NUTATION)
              CALL PMUPJ2(VPMEJ2, DPSI(I), EPSM(I), AUTMOB, OBMTAU, &
                          RR, DD)

              RAX(I) = RR
              DCX(I) = DD
              RR = RR / DEGRAD
              DD = DD / DEGRAD
          END DO
      END IF

      END
