!$TRUEPV
      SUBROUTINE TRUEPV(S,S1,NMP,INDP,RPOLE,PXPOLE,LPOLAD,XM,XT,PXSPXP, &
     &   PXSPLV,NM,NINTPL,NINTP2,MJDSEC,FSEC,COSTG,SINTG,LCET,SCRTCH,   &
     &   STAINF,PLTDRV,PLTVEL,ISTA,STAV,STAVT,PMPXE,PMPXI,              &
     &   ICNL2,ICNH2,LVLBI,AA, LYARA, ISNUMB,II,XDPOLE,PXSPXD)
!********1*********2*********3*********4*********5*********6*********7**
! TRUEPV           83/03/29            8303.0    PGMR - TOM MARTIN
!
! FUNCTION:  ROTATE THE MEAN POLE COORDINATES OF A TRACKING
!            SPATION TO TRUE POLE COORDINATES FOR A SET OF
!            MEASUREMENTS OF VECTOR LENGTH "NM".
!            TO ACCOMPLISH THIS, LINEARLY INTERPOLATE THE TRUE
!            COORDINATES OVER A TIME SPAN
!            AS LONG AS ONE PASS OF DATA. IF DISCRETE POLAR
!            MOTION VALUES SWITCH OVER DURING THIS INTERVAL OF
!            TIME THEN SUBDIVIDE THE INTERPOLATION INTERVAL AS
!            NECESSARY TO AVOID INTERPOLATING ACROSS DISCRETE
!            BOUNDARIES.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   S        I         INTERPOLATION FRACTIONS
!   S1            A    1.0-S
!   NMP      I         NUMBER OF MEASUREMENTS IN EACH INTERPOLATION
!                      INTERVAL
!   INDP     I         POLAR MOTION INTERVAL->ADJUSTED PARAMETER
!                      POINTER
!   RPOLE    I         POLAR MOTION ROTATION MATRICES AT EACH END
!                      OF EACH INTERPOLATION INTERVAL
!   PXPOLE   I         PARTIALS OF THE TRUE POLE STATION LOCATION
!                      W.R.T. THE POLAR MOTION X & Y PARAMETERS AT
!                      EACH END OF EACH INTERPOLATION INTERVAL
!   XDPOLE   I         PARTIALS OF THE TRUE POLE STATION RATE
!                      W.R.T. THE POLAR MOTION X & Y PARAMETERS AT
!                      EACH END OF EACH INTERPOLATION INTERVAL
!   LPOLAD   I         SWITCH INDICATING THAT POLAR MOTION IS
!                      ADJUSTED IN THIS RUN
!   XM       I         MEAN POLE STATION COORDINATES
!   XT       O         TRUE POLE STATION COORDINATES FOR EACH OF "NM"
!                      MEASUREMENT TIMES
!   PXSPXP   O         PARTIALS OF TRUE POLE STATION COORDINATES
!                      W.R.T. POLAR MOTION FOR EACH OF "NM"
!                      MEASUREMENT TIMES
!   PXSPLV
!   NM       I         NUMBER OF MEASUREMENT TIMES
!   NINTPL   I         NUMBER OF INTERPOLATION INTERVALS
!   NINTP2   I         NINTPL*2
!   MJDSEC   I         INTEGER SECONDS SINCE GEODYN REF. TIME
!   FSEC     I         FRACTIONAL REMAINING SECONDS
!   COSTG    I         COSINE OF GREENWICH HR ANGLE
!   SINTG    I         SINE OF GREENWICH HR ANGLE
!   LCET     I         RETURN TRUE STATION COORDINATES CORRECTED
!                      FOR SOLID EARTH TIDES IF TRUE
!   SCRTCH        A    WORK ARRAY 17*NM
!   STAINF   I         ARRAY OF STATION INFORMATION
!   PLTDRV   I         ARRAY CONTAING DERIVATIVES OF STATION
!                      VELOCITIES WRT PLATE MOTION PARAMETERS
!   PLTVEL   I         STATION VELOCITIES DUE TO PLATE MOTION
!   PMPXE   I/O   A    STATION PARTIALS
!   ISTA
!   STAV
!   STAVT
!
! COMMENTS: MODIFICATIONS                LUCIA TSAOUSSI
!
! 02.20.91: THE POLAR MOTION VALUES XP & YP INTERPOLATED ARE STORED
!           IN SCRTCH(NM,1) & SCRTCH(NM,2) RESPECTIVELY
!           THESE ARE USED IN SOLIDT FOR COMPUTING THE POLE TIDE
! 06.28.91:COMPUTATION OF A  PART OF THE STATION PARTIALS FOR THE
!          VLBI DELAY : POL.MOT.MATRIX * EHAT = PMPXE
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CBLOKV/QUANO(3),EHAT(3),DEDA(3),DEDD(3),EFLG
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/GCMMOD/GCMNPS,GCMT0,GCMPER(10),GCMAMP(30),GCMPHA(30),      &
     &              XXGCM
      COMMON/PLATE /NPLATE,MJDPLR,NPLTMS,NXPLAT
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/CEARTH/AEG,AEGSQ,FGINV,FG,EGSQ,EGSQP1,C1,C2,FOURC1,TWOC2,  &
     &              XH2,XL2,WREF,RMEAN,REFC20,REFC40,REFC60,BEG,CBLTC,  &
     &              WAE2,GAMMAA,FSTAR,F4
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
      COMMON/POLINF/NPOLE,NPOLEA,NUTA,MXUTC,NMODEL,NXPOLI
      COMMON/SETDI /NSETDP,NFRSED,NFRSDI,NFRSLN,NXSETI
      COMMON/SETDR /H20,XL20,DH2DP,DL2DP,H3,XL3,DHID,DLID,DHIS,DLIS,    &
     &              XLID,XLISD
      DIMENSION S(NM),S1(NM),RPOLE(3,3,NINTP2),NMP(NINTPL),             &
     &  PXPOLE(3,2,NINTP2),INDP(NINTPL),XM(3),XT(NM,3),                 &
     &  PXSPXP(NM,3,2),PXSPLV(NM,3,NSETDP),FSEC(NM),COSTG(NM),SINTG(NM),&
     &  STAINF(NSTAIN,*),PLTDRV(3,3,*),PLTVEL(3,*),SCRTCH(NM,21),       &
     &  XMPLTC(3),STAV(3,1),STAVT(*),PMPXE(NM,3),PMPXI(NM,3),           &
     &  ICNL2(NSTA),ICNH2(NSTA),AA(*),II(*),XDPOLE(3,2,NINTP2),         &
     &  PXSPXD(NM,3,2)
!
!>>>>>>>>>>>
!
      PARAMETER ( NMETID = 1000 )
      dimension xtnoet(3,NMETID), xtet(3,NMETID), dummy(3,3)
      DATA kentry /0/
      DATA iprint /2/
!
      DATA ZERO/0.0D0/,ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!        write(6,*) 'truepv: at entry : lyara, isnumb ',lyara, isnumb
      H20H=H20
      XL20H=XL20

! ZERO OUT PLATE CORRECTION
      DO 10 I=1,3
      XMPLTC(I)=ZERO
   10 END DO
! COMPUTE COMPLEMENT OF INTERPOLATION FRACTIONS
      DO 100 N=1,NM
      S1(N)=ONE-S(N)
  100 END DO
! CLEAR TRUE POLE STATION COORDINATE ARRAY
      DO 300 I=1,3
      DO 200 N=1,NM
      XT(N,I)=ZERO
  200 END DO
  300 END DO
! FIRST INTERPOLATION INTERVAL ALWAYS BEGINS WITH 1
      NM1=1
      INDEX=1
      INDXA1=0
! LOOP THRU ALL INTERVALS
      DO 2000 INTVL=1,NINTPL
! LAST MEASUREMENT IN THE INTERVAL
      NM2=NMP(INTVL)+NM1-1
      INDEX1=INDEX+1
      NMQ=NM1
      INDXQ=INDEX
!   GET TRUE POLE COORDINATES AT ENDPOINT TIMES
      DO 1100 I=1,NM
!   FIRST CORRECT MEAN COORDINATES FOR PLATE MOTION AT ENDPOINT TIMES
      IF(NPLATE.LE.0) GO TO 1040
      TE=MJDSEC-MJDPLR+FSEC(NMQ)
      XMPLTC(1)=TE*PLTVEL(1,ISTA)
      XMPLTC(2)=TE*PLTVEL(2,ISTA)
      XMPLTC(3)=TE*PLTVEL(3,ISTA)
 1040 CONTINUE
      IF(STAVT(ISTA).EQ.0.D0) GOTO 1049
      TE=MJDSEC-STAVT(ISTA)+FSEC(NMQ)
      XMPLTC(1)=TE*STAV(1,ISTA)
      XMPLTC(2)=TE*STAV(2,ISTA)
      XMPLTC(3)=TE*STAV(3,ISTA)
 1049 CONTINUE
!
      DO 1050 J=1,3
      XT(NMQ,J)=RPOLE(J,1,INDXQ)*(XM(1)+XMPLTC(1))                      &
     &         +RPOLE(J,2,INDXQ)*(XM(2)+XMPLTC(2))                      &
     &         +RPOLE(J,3,INDXQ)*(XM(3)+XMPLTC(3))
 1050 END DO
!
!   GET POLAR MOTION COORDINATES AT ENDPOINT TIMES
      SCRTCH(NMQ,1)=RPOLE(3,1,INDXQ)
      SCRTCH(NMQ,2)=RPOLE(2,3,INDXQ)
!
      NMQ=NM2
      INDXQ=INDEX1
 1100 END DO
      NM2M1=NM2-1
      NM1P1=NM1+1
      NM1122=NM2-NM1-1
      IF(NM1P1.GT.NM2M1) GO TO 1550
!   INTERPOLATE FOR COORDINATES AT OTHER TIMES
      DO 1510 I=1,3
      DO 1500 N=NM1P1,NM2M1
      XT(N,I)=S(N)*XT(NM1,I)+S1(N)*XT(NM2,I)
 1500 END DO
 1510 END DO
      DO 1520 N=NM1P1,NM2M1
      SCRTCH(N,1)=S(N)*SCRTCH(NM1,1)+S1(N)*SCRTCH(NM2,1)
      SCRTCH(N,2)=S(N)*SCRTCH(NM1,2)+S1(N)*SCRTCH(NM2,2)
 1520 END DO
 1550 CONTINUE
! BYPASS PARTIAL DERIVATIVE COMPUTATIONS UNLESS ADJUSTMENT REQUESTED
      IF(.NOT.LPOLAD) GO TO 1950
      INDXA=INDXA1+1
      INDXA1=INDXA+1
      IF(INDP(INTVL).LE.0) GO TO 1950
      NM12=NM1122+2
      DO 1900 I=1,3
! LOOP THRU X & Y OF THE POLE
      DO 1800 J=1,2
! LOAD THE FRACTIONAL PART OF THE DERIVATIVE ELEMENT COMING FROM THE
!      FIRST PARTIAL DERIVATIVE MATRIX OF EACH INTERVAL
      DO 1600 N=NM1,NM2
      PXSPXP(N,I,J)=              PXPOLE(I,J,INDXA )*S(N)
 1600 END DO
      IF(NMODEL.EQ.1) THEN
      DO 1601 N=NM1,NM2
      PXSPXD(N,I,J)=              XDPOLE(I,J,INDXA )*S(N)
 1601 END DO
      ENDIF
! SUM THE FRACTIONAL PART OF THE DERIVATIVE ELEMENT COMING FROM THE
!     LAST PARTIAL DERIVATIVE MATRIX OF EACH INTERVAL
      DO 1700 N=NM1,NM2
      PXSPXP(N,I,J)=PXSPXP(N,I,J)+PXPOLE(I,J,INDXA1)*S1(N)
 1700 END DO
      IF(NMODEL.EQ.1) THEN
      DO 1701 N=NM1,NM2
      PXSPXD(N,I,J)=PXSPXD(N,I,J)+XDPOLE(I,J,INDXA1)*S1(N)
 1701 END DO
      ENDIF
 1800 END DO
 1900 END DO
 1950 CONTINUE
! INCREMENT THE MEASUREMENT POINTER AND THE MATRIX INDEX
      NM1=NM2+1
      INDEX=INDEX1+1
 2000 END DO

! *** ADD CORRECTION FOR GEOCENTER MOTION (GCMMOD) IF REQUESTED
!
      IF (INT(GCMNPS+0.001D0).GE.1) THEN
       DO JJ=1,NM
         CALL GCMCOR(MJDSEC,FSEC(JJ),XT(JJ,1),XT(JJ,2),XT(JJ,3))
       END DO
      END IF
!
! *** COMPUTE THE FINAL ROTATION FOR THE EARTH FIXED GEOCENTRIC
!     QUASAR UNIT VECTOR (STORED IN PMPXE AND COPIED IN PMPXI).
      IF (.NOT.LVLBI) GOTO 3000
      DO 2100 I=1,NM
      PMPXE(I,1)=PMPXI(I,1)+PMPXI(I,2)*SCRTCH(I,1)*SCRTCH(I,2)          &
     &          +PMPXI(I,3)*SCRTCH(I,1)
      PMPXE(I,2)=PMPXI(I,2)-PMPXI(I,3)*SCRTCH(I,2)
      PMPXE(I,3)=-SCRTCH(I,1)*PMPXI(I,1)+SCRTCH(I,2)*PMPXI(I,2)         &
     &          +PMPXI(I,3)
 2100 END DO
      DO 2200 I=1,NM
      DO 2200 J=1,3
      PMPXI(I,J)=PMPXE(I,J)
 2200 CONTINUE
!
 3000 IF(.NOT.LCET) RETURN
      IF(NSTLV.NE.0)THEN
      KL0=ICNL2(ISTA)
      KH0=ICNH2(ISTA)
      KKL=KSTEL2+KL0-1
      KKH=KSTEH2+KH0-1
      XL20 = AA(KKL)
      H20 = AA(KKH)
      ENDIF
      DO 3010 I=1,NM
      SCRTCH(I,18)=STAINF(2,ISTA)
      SCRTCH(I,19)=STAINF(3,ISTA)
      SCRTCH(I,20)=STAINF(5,ISTA)
      SCRTCH(I,21)=STAINF(6,ISTA)
 3010 END DO
!     WRITE(6,*)'*TRUEPV*ISTA*XL2,XH2*',ISTA,XL2,XH2,KL0,KH0,KKL,KKH
!>>>>>>>>>>>>>>
!
!     ....this section writes out the earth tide vs time
!
      IF(  lyara  ) then
         kentry = kentry + 1
         if(kentry .eq. 1) then
!            write(6,*) 'truepv: open file 98 '
            OPEN(98,FILE='fort.98',STATUS='NEW',FORM='FORMATTED',       &
     &         BLANK='ZERO')
            write(98,5000)
 5000       format(1x,'EARTH TIDE FILE'/                                &
     &             1x,'station ',4x, 'MJD', 5x,'NM',1x, 'YYMMDD ',      &
     &             'HHMMSS ',3x, 'del phi', 6x, 'del lam',              &
     &             6x, 'del height')
            mjdse0 = mjdsec
          endif
!         write(6,*) 'truepv: call solidt '
         do 3001 iii=1,3
            do 3011 jjj=1,nm
               xtnoet(iii,jjj) = xt(jjj,iii)
 3011       continue
 3001    continue
      endif

! pd %%%%%% since the last logical is FALSE nothing happens to
!           SCRTCH(1,15) in the SOLIDT subroutine
      CALL SOLIDT(XT,MJDSEC,FSEC,COSTG,SINTG,NM,SCRTCH,XT,              &
     &     PXSPLV,SCRTCH(1,18),SCRTCH(1,19),SCRTCH(1,20),               &
     &     SCRTCH(1,21),NM,.FALSE.,.TRUE.,.FALSE.,SCRTCH(1,15),         &
     &     AA(KDXTID),AA(KANFSD),AA(KSPDSD),AA(KPRMFS),AA(KSCRFR),      &
     &     AA(KCOSAS),AA(KSINAS),AA,II)
!>>>>>>>>>>>>>>
      H20=H20H
      XL20=XL20H
      if( lyara ) then
         do 3002 iii=1,3
            do 3012 jjj=1,nm
               xtet(iii,jjj) = xt(jjj,iii)
 3012       continue
 3002    continue
!
         do 3050 jjj=1,NM
!
!         if( kentry .le. iprint .and. jjj .le. 2 ) then
!         write(6,*) 'truepv: mjdsec ', mjdsec
!         write(6,*) 'truepv: jjj ', jjj
!         write(6,*) 'truepv:  fsec(jjj) ',fsec(jjj)
!         write(6,*) 'truepv: xtnoet ', (xtnoet(iii,jjj),iii=1,3)
!         write(6,*) 'truepv: xtet ', (xtet(iii,jjj),iii=1,3)
!         endif
!
            call plhout( xtnoet(1,jjj), phino, cosphi, sinphi, xlamno,  &
     &                coslam, sinlam, hnoet, dummy, .true., .false. )
            call plhout( xtet(1,jjj), phiet, cosphi, sinphi, xlamet,    &
     &                coslam, sinlam, het,   dummy, .true., .false. )
!
            delphi = phiet - phino
            dellam = xlamet - xlamno
            del_h  = het   -  hnoet
            mjddif = mjdsec - mjdse0
            mjdsex = mjdsec+fsec(jjj)
            xmjddy = (DBLE(mjdsec)+fsec(jjj))/86400.D0 + TMGDN2
            call MJDYMD(mjdsex,IYMD,IHMS, 4)
            if( MOD(kentry,2) .eq. 1) then
               write(98,'(1x,i8,1x,f10.3,i3,i7,i7,3d13.3 )')            &
     &         isnumb, xmjddy, jjj, IYMD, IHMS, delphi, dellam, del_h
         endif
 3050    continue
      endif
      RETURN
      END
