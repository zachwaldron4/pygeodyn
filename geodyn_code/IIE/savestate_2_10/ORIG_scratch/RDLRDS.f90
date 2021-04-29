!$RDLDDS
      SUBROUTINE RDLRDS(PARMVC,TXSTAT,NXSTAT,NX,INDEX,NSETA,WT,AA)
!********1*********2*********3*********4*********5*********6*********7**
! RDLDDS           00/00/00            0000.0    PGMR - DEP
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A
!   II      I/O   A
!   LL      I/O   A

! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CDSTAT/CDX(1000,2,3,6)
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
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
      COMMON/ICDSTA/ICDX(1000,3,6)
      COMMON/IDELTX/MXSTAT,NEVENTS,NDSTAT,IDSYST,NDELTX
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
!

      DIMENSION AA(1)
      DIMENSION PARMVC(MAPARM),TXSTAT(MXSTAT,30)
      DIMENSION NX(6),UTDT(1),WT(1)
      DATA C1P6/1.D6/

      INTEGER, parameter :: iunt62 = 62  ! jjm added 20120531
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************

      IF(.NOT.LITER1) GOTO 1500

! The times in TXSTAT are ET times
      DO I=1,NSETA
      IF(TXSTAT(1,I).GT.0.D0) IN=I
      ENDDO

      DO I=1,6
      NX(I)=0
      ENDDO
      OPEN(IUNT62,FILE='fort.62',STATUS='OLD',FORM='FORMATTED')
      READ(IUNT62,20000,END=2000)STDV
      WT(1)= 1.D0/STDV**2.D0
      INDEX=IPVAL0(IXDXYZ)
 1000 CONTINUE
      READ(IUNT62,10000,END=2000)IDIR,ISAT,IYMD,HMS,DT

       DO KK=1,6

      IF(IDIR.EQ.KK) THEN
! WE HAVE THE AXIS IDENTIFICATION

      III=HMS
      IHMS=INT(HMS)
      FSEC=HMS-DBLE(IHMS)
      CALL YMDTIS(IYMD,IHMS,MJDSEC)
      CALL UTCET(.TRUE.,1,MJDSEC,FSEC,FSECOT,AA(KA1UT))
      UTTIME=DBLE(MJDSEC)+FSECOT
! CONVERT YYMMDDHHMMSS.SS TO MJDSEC.FSEC UTC
!        I1YMD=EPOCH/C1P6
!        I1HMS=EPOCH-I1YMD*C1P6
!        F1SEC=EPOCH-I1YMD*C1P6-I1HMS
!        CALL YMDTIS(I1YMD,I1HMS,I1SEC)
!  CONVERT TO ET TIME SYSTEM
!        DT1=TDIF(7,3,I1YMD,I1HMS,AA(KA1UT))
!        UTTIME=I1SEC+F1SEC+DT1

! VERIFY THAT THE TIME READ FROM 62 AGREES WITH A TIME IN TXSTAT
      DO I=1,MXSTAT
!      write(6,*)' dbg times ',UTTIME, TXSTAT(I,IN),I
       IF(UTTIME.EQ.TXSTAT(I,IN)) THEN
!      write(6,*)' dbg WE FOUND A TIME !! ',UTTIME
! WE FOUND A TIME
       NX(KK)=NX(KK)+1
         DO IJ=1,3
         CDX(NX(KK),2,IJ,KK)=DT
!      write(6,*)' dbg time in CDX ',CDX(NX(KK),2,IJ,KK),NX(KK),IJ,KK
         ENDDO
! IDNTIFY THE PARAMETER VALUE
       DO IJ=1,3
       CDX(NX(KK),1,IJ,KK)=PARMVC(INDEX+IJ+2+6*(I-1))
       ICDX(NX(KK),IJ,KK)=INDEX+IJ+2+6*(I-1)
!      write(6,*)' dbg cdx icdx ',cdx(nx(kk),1,ij,kk),
!    . icdx(nx(kk),ij,kk),nx(kk),kk
       ENDDO
       ENDIF
      ENDDO
      ENDIF

       ENDDO

       GOTO 1000

 1500 CONTINUE
       IF(LITER1) GOTO 2000
       DO KKK=1,6
        DO KK=1,NX(KKK)
         DO IJ=1,3
!        CDX(KK,1,IJ,KKK)=PARMVC(INDEX+IJ+2+6*(KK-1))
         CDX(KK,1,IJ,KKK)=PARMVC(ICDX(KK,IJ,KKK))
!        write(6,*)' dbg next iteration ',cdx(kk,1,IJ,KKK),
!    . kk,1,ij,kkk
         ENDDO
        ENDDO
       ENDDO

 2000 CONTINUE
10000 FORMAT(I2,15X,I7,20X,I6,F9.2,F13.3)
20000 FORMAT(D12.5)
      RETURN
      END
