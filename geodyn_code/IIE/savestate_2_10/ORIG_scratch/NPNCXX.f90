!$NPNCXX
      SUBROUTINE NPNCXX(MINEP,LFLAG,LINIT,IDIM1,ICB)
!********1*********2*********3*********4*********5*********6*********7**
! NPNCXX           86/12/24            8701.0    PGMR - D. ROWLANDS
!                                                       D. PAVLIS
!
!  FUNCTION:  DETERMINE ELEMENTS OF PRECESION SINCE 2000.0
!             FOR A PLANET OTHER THAN THE EARTH PROVIDED BY THE
!             SUPPLEMENTARY PLANETARY EPHEMERIS.
!             THIS IS DONE ONLY AT 12 HR INTERPOLATION ENDPOINT
!             TIMES. AT PRESENT THIS 12HR INFORMATION IS NOT USED -
!             BUT IT MAY BE USED LATER. IN ADDITION, DETERMINE THE
!             COEFFICIENTS OF THE LINEAR POLYNOMIALS NEEDED TO
!             CALCULATE THE ABOVE QUANTITIES AT OTHER TIMES. ALSO
!             FIGURES THE COEFFCIENTS OF THE LINEAR POLYNOMIAL USED
!             TO DESCRIBE THE ROTATION OF THE PLANET.THE TIME IN THIS
!             FORMULATION WILL BE REFERENCED TO THE FIRST 12HR INTERP
!             TIME.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MINEP    I    S    THE NUMBER OF 12HR TIMES NEEDED TO BE FILLED IN
!                      COMMON BLOCK DNUTAT.
!   LFLAG    I    S    .FALSE. THE SUBROUTINE WILL OPERATE THE ORIGINAL
!                      .TRUE.  PLANOR PARAMETERS NEED TO BE SUBSTITUTED
!   LINIT    I    S    .TRUE. INITIALIZE PLANETARY CONSTANTS
!                      IN ESTIMG
!
! COMMENTS: THE OUTPUT IS THROUGH THE COMMON BLOCKS.
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/DNUTAT/RAPXTR(1),DCPXTR(1),DNUTXR(8),SMDNUT(30)
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/EPHSET/EMFACT,DTENPD,DTINPD,FSC1EN,FSCENP(2),FSCINP(4),    &
     &   FSDINP(4),XEPHST
!     COMMON/IBODPT/
      COMMON/LASTER/LASTR(2),LBINAST,NXASTR
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
      COMMON/ROTELM/ALF020,ALFR20,DEC020,DECR20,W020,WR20,X100,WRR20,  &
     &               XR100,XA100,THXTGC2S(3),XROTLM
      COMMON/ROTEL2/ALF0202,ALFR202,DEC0202,DECR202,W0202,WR202,X1002, &
     &              WRR202,  &
     &               XR1002,XA1002,THXTGC2S2(3),XROTLM2
      COMMON/THXTGC/THXTGC(3,2)
      COMMON/XTRAOR/T0XTRO,T0XTRO2,CVENUS,CLIBMN,CMARS,CMERC,XXTRAO
      COMMON/XPCNT /NXTRAC,NXFLAG,NXALLC,NCPPER,NMINRT,NDPOR,NXXTRA
!
      DIMENSION XLLIB(3),XLLIBV(3)
!
      DATA ONE/1.D0/,TWO/2.D0/,THREE/3.D0/,D100/8640000.D0/
      DATA CENJUL /36525.D0/
      DATA XJD2K /2451545.D0/
! NEED TO ADD DATA FOR OTHER PLANETS TOO
!
!-----------------------------------------------------------------------
!
      DATA NSDNUT/10/
      DATA ZERO/0.0D0/
      DATA LFIRST/.TRUE./
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**

!
         IF(.NOT.LFLAG) THEN
         IF(LINIT) THEN
! FIRST TIME
         THXTGC(1,1)=THXTGC2S(1)
         THXTGC(1,1)=MOD(THXTGC(1,1)+TWOPI,TWOPI)
         THXTGC(2,1)=THXTGC2S(2)
         THXTGC(3,1)=THXTGC2S(3)
         GO TO 50
       ENDIF
       ENDIF

!...note: FSDINP(1) depends on Geodyn internal reference time
!
         DDR0=(TMGDN1-2430000.5D0)*86400.D0
         FSDIN0=FSDINP(1)+DDR0
!
         XTIME=MOD(FSDIN0,D100)
         XTM100=FSDIN0-XTIME+ONE
         ITM100=XTM100/D100
         XTM100=DBLE(ITM100)
         XA100=XTM100*X100
         XA100=MOD(XA100,TWOPI)
         XAREST=XTIME*WR20
         XAREST=MOD(XAREST,TWOPI)
!!!      XRESTR=(XTIME**2.D0)*WRR20
         YTIME=FSDIN0-T0XTRO
         XRESTR=(YTIME**2.D0)*WRR20
         XRESTR=MOD(XRESTR,TWOPI)
         THXTGC(1,1)=XA100+XAREST+XRESTR+W020
         THXTGC(1,1)=MOD(THXTGC(1,1)+TWOPI,TWOPI)
         THXTGC(2,1)=WR20
         THXTGC(2,1)=THXTGC(2,1)+2.D0*YTIME*WRR20
         THXTGC(3,1)=WRR20
!
   50 CONTINUE
! IF UPDATED VALUES ARE PRINTED IN ESTIMG THEN
!
      IF( LFIRST ) THEN
         WRITE(6,*) ' '
         WRITE(6,*) 'NPNCXX: ICB ', ICB
         WRITE(6,*) 'NPNCXX: XA100, XAREST, W020 ',XA100, XAREST,       &
     &               W020
         WRITE(6,*) 'NPNCXX: THXTGC(1,1), THXTGC(2,1) ',  &
     &               THXTGC(1,1), THXTGC(2,1)
!
         WRITE(6,*) 'NPNCXX: MINEP, NSDNUT ', MINEP, NSDNUT
         WRITE(6,*) 'NPNCXX: ALF020        ', ALF020
         WRITE(6,*) 'NPNCXX: ALFR20        ', ALFR20
         WRITE(6,*) 'NPNCXX: DEC020        ', DEC020
         WRITE(6,*) 'NPNCXX: DECR20        ', DECR20
         WRITE(6,*) ' '
!
      ENDIF
!
      DO 100 I=1,MINEP
      FSDIN0=FSDINP(I)+DDR0
      IPT=1+(I-1)*NSDNUT
      RAPXTR(IPT)=ALF020+FSDIN0*ALFR20
      DCPXTR(IPT)=DEC020+FSDIN0*DECR20
  100 END DO
!
      PRXCON(1,1,1)=ALF020
      PRXCON(2,1,1)=ALFR20
      PRXCON(1,2,1)=DEC020
      PRXCON(2,2,1)=DECR20
!
      IF( LFIRST) THEN
         WRITE(6,*) ' '
         WRITE(6,*) 'NPNCXX: ICB ', ICB
         OUTXX = PRXCON(1,1,1) / DEGRAD
         WRITE(6,*) 'NPNCXX: PRXCON(1,1,1) A2000  ',                    &
     &                       PRXCON(1,1,1),' RAD   ', OUTXX, ' DEGREES '
         OUTXX = PRXCON(2,1,1) / DEGRAD
         WRITE(6,*) 'NPNCXX: PRXCON(2,1,1) A2000 DOT ',                 &
     &                       PRXCON(2,1,1),' RAD   ', OUTXX, ' DEGREES '
         OUTXX = PRXCON(1,2,1) / DEGRAD
         WRITE(6,*) 'NPNCXX: PRXCON(1,2,1) D2000    ',                  &
     &                       PRXCON(1,2,1),' RAD   ', OUTXX, ' DEGREES '
         OUTXX = PRXCON(2,2,1) / DEGRAD
         WRITE(6,*) 'NPNCXX: PRXCON(2,2,1) D2000 DOT ',                 &
     &                       PRXCON(2,2,1),' RAD   ', OUTXX, ' DEGREES '
         OUTXX = THXTGC(1,1) / DEGRAD
         WRITE(6,*) 'NPNCXX: THXTGC(1,1) ',                             &
     &                       THXTGC(1,1),' RAD   ', OUTXX, ' DEGREES '
         OUTXX = THXTGC(2,1) / DEGRAD * SECDAY
         WRITE(6,*) 'NPNCXX: THXTGC(2,1) ',                             &
     &                       THXTGC(2,1),' RAD/SEC   ',                 &
     &                       OUTXX, ' DEGREES/DAY '
         WRITE(6,*) ' '
!        LFIRST = .FALSE.
         IF(.NOT.LBINAST) LFIRST = .FALSE.
      ENDIF

         IF(.NOT.LFLAG) THEN
         IF(LINIT) THEN
! FIRST TIME
         THXTGC(1,2)=THXTGC2S2(1)
         THXTGC(1,2)=MOD(THXTGC(1,2)+TWOPI,TWOPI)
         THXTGC(2,2)=THXTGC2S2(2)
         THXTGC(3,2)=THXTGC2S2(3)
         GO TO 150
         ENDIF
         ENDIF

!
!...note: FSDINP(1) depends on Geodyn internal reference time
!
         DDR0=(TMGDN1-2430000.5D0)*86400.D0
         FSDIN0=FSDINP(1)+DDR0
!
         XTIME2=MOD(FSDIN0,D100)
         XTM1002=FSDIN0-XTIME2+ONE
         ITM1002=XTM1002/D100
         XTM1002=DBLE(ITM1002)
         XA1002=XTM1002*X1002
         XA1002=MOD(XA1002,TWOPI)
         XAREST2=XTIME2*WR202
         XAREST2=MOD(XAREST2,TWOPI)
!!!      XRESTR2=(XTIME2**2.D0)*WRR202
         YTIME=FSDIN0-T0XTRO
         XRESTR2=(YTIME**2.D0)*WRR202
         XRESTR2=MOD(XRESTR2,TWOPI)
         THXTGC(1,2)=XA1002+XAREST2+XRESTR2+W0202
         THXTGC(1,2)=MOD(THXTGC(1,2)+TWOPI,TWOPI)
         THXTGC(2,2)=WR202
         THXTGC(2,2)=THXTGC(2,2)+2.D0*YTIME*WRR202
         THXTGC(3,2)=WRR202
!
  150 CONTINUE
! IF UPDATED VALUES ARE PRINTED IN ESTIMG THEN

      IF( LFIRST ) THEN
         WRITE(6,*) ' '
         WRITE(6,*) 'NPNCXN: ICBDGM ', ICBDGM
         WRITE(6,*) 'NPNCXN: XA1002, XAREST2, W0202 ',XA1002, XAREST2,  &
     &               W0202
         WRITE(6,*) 'NPNCXN:THXTGC(1,2),THXTGC(2,2)',THXTGC(1,2), &
     &               THXTGC(2,2)
!
         WRITE(6,*) 'NPNCXN: MINEP, NSDNUT ', MINEP, NSDNUT
         WRITE(6,*) 'NPNCXN: ALF0202        ', ALF0202
         WRITE(6,*) 'NPNCXN: ALFR202        ', ALFR202
         WRITE(6,*) 'NPNCXN: DEC0202        ', DEC0202
         WRITE(6,*) 'NPNCXN: DECR202        ', DECR202
         WRITE(6,*) ' '
!
      ENDIF
!
!     DO 200 I=1,MINEP
!     FSDIN0=FSDINP(I)+DDR0
!     IPT=1+(I-1)*NSDNUT
!     RAPXTR(IPT)=ALF0202+FSDIN0*ALFR202
!     DCPXTR(IPT)=DEC0202+FSDIN0*DECR202
! 200 END DO


      PRXCON(1,1,2)=ALF0202
      PRXCON(2,1,2)=ALFR202
      PRXCON(1,2,2)=DEC0202
      PRXCON(2,2,2)=DECR202
!
      IF( LFIRST) THEN
         WRITE(6,*) ' '
         WRITE(6,*) 'NPNCXN: ICBDGM ', ICBDGM
         OUTXX = PRXCON(1,1,2) / DEGRAD
         WRITE(6,*) 'NPNCXN: PRXCON(1,1,2) A2000  ',                   &
     &                       PRXCON(1,1,2),' RAD   ', OUTXX, ' DEGREES '
         OUTXX = PRXCON(2,1,2) / DEGRAD
         WRITE(6,*) 'NPNCXN: PRXCON(2,1,2) A2000 DOT ',                &
     &                       PRXCON(2,1,2),' RAD   ', OUTXX, ' DEGREES '
         OUTXX = PRXCON(1,2,1) / DEGRAD
         WRITE(6,*) 'NPNCXN: PRXCON(1,2,2) D2000    ',                 &
     &                       PRXCON(1,2,2),' RAD   ', OUTXX, ' DEGREES '
         OUTXX = PRXCON(2,2,1) / DEGRAD
         WRITE(6,*) 'NPNCXN: PRXCON(2,2,2) D2000 DOT ',                &
     &                       PRXCON(2,2,2),' RAD   ', OUTXX, ' DEGREES '
         OUTXX = THXTGC(1,2) / DEGRAD
         WRITE(6,*) 'NPNCXN: THXTGC(1,2) ',                            &
     &                       THXTGC(1,2),' RAD   ', OUTXX, ' DEGREES '
         OUTXX = THXTGC(2,2) / DEGRAD * SECDAY
         WRITE(6,*) 'NPNCXN: THXTGC(2,2) ',                            &
     &                       THXTGC(2,2),' RAD/SEC   ',                &
     &                       OUTXX, ' DEGREES/DAY '
         WRITE(6,*) ' '
         LFIRST = .FALSE.
      ENDIF ! LFIRST


!


      RETURN
      END
