!$RDOFFL
      SUBROUTINE RDOFFL( nsta0, mjdin , fsecin, nm, antout,  aa, ii )
!********1*********2*********3*********4*********5*********6*********7**
! RDOFFL           95/04/04            9505.0    PGMR - J. McCarthy
!
! FUNCTION: READ ANTENNA AXIS DISPLACEMENT CORRECTIONS FROM
!           STATION FILE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NM       I         NUMBER OF MEASUREMENTS
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
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
      COMMON/CORI03/KICRD ,KICON ,KISTNO,KINDPI,KMJDPL,KIPOLC,          &
     &              KNDOLA,KIOLPA,KKIOLA,KIP0OL,KNDOLM,KIOLPM,          &
     &              KKIOLM,KIPVOL,KICNL2,KICNH2,                        &
     &              KSTMJD, KNUMST, KITAST, KSTNRD, KICNV,              &
     &              KTIDES,KFODEG,KFOORD,KRDEGR,KRORDR,                 &
     &              KPHINC,KDOODS,KOTFLG,KJDN  ,KPTDN ,                 &
     &              KATIND,KPRESP,KMP2RS,KSITE,KPTOLS,NXCI03
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
!
!     DIMENSION antout(500), fsecin(*), aa(*), ii(*)
      DIMENSION antout(1), fsecin(*), aa(*), ii(*)
!     DIMENSION fstemp(1)
!
      DATA ZERO/0.0D0/,ONE/1.0D0/
      DATA ifirst/0/
      DATA kentry/0/
      DATA iprint/100/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      kentry = kentry + 1
!
!
!      write(6,*) 'rdoffl: ifirst ', ifirst
!      write(6,*) 'rdoffl: NSTNUM ', NSTNUM
!      write(6,*) 'rdoffl: NSTIME ', NSTIME
!      write(6,*) 'rdoffl: station array ',
!     &      ( II(KNUMST-1+jjj),jjj=1, NSTNUM )
!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
      IF(ifirst .le. 0 ) then
          ifirst=1
          OPEN(99,FILE='fort.99',STATUS='NEW',FORM='FORMATTED',         &
     &         BLANK='ZERO')
            write(99,5000)
 5000     format(1x,'STATION HEIGHT CHANGES READ FROM INPUT FILE'       &
     &           /2x,'station  ',4x, 'MJD', 8x,'NM',1x, 'YYMMDD',       &
     &              1x,  'HHMMSS ',5x, 'height change')
!
      endif
!
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!
  200 continue
!
      do 220 N=1,NM
         antout(N) = 0.0D0
  220 continue
!
!     ....input station number is nsta0
!
            ista = 0
            call FNDNUM( nsta0, II(KNUMST), NSTNUM, ista )
!
      if( ista .le. 0 .and. kentry.le.iprint ) then
         write(6,*) 'station number ', nsta0, ' not in station list'
         write(6,*) 'zero station antenna offset applied '
         return
      endif
!
      if( ista .le. 0 ) then
         if( MOD(kentry,2) .eq. 1 ) then
            xmjddy = ( DBLE(mjdx) )/86400.D0 + TMGDN2
            call MJDYMD(mjdx,IYMD,IHMS, 4)
            write(99,'(1x, i9,1x,f12.3,1x,i4,i7,i7,1x,1d15.5 )')        &
     &         nsta0, xmjddy, n, IYMD, IHMS,  antout(1)
         endif
         return
      endif
!
!
!     ....i1 and i2 are the bounding indices in the array for the
!     ....station "nsta0"
!
      i1 = II(KITAST - 1 + ista )   + 1
      i2 = II(KITAST - 1 + ista+1 )
!
       if(kentry.le.iprint )                                            &
     & write(6,*) 'rdoffl:  nsta0,ista, i1,i2  ',nsta0, ista, i1,i2

!     ....find the correct offset for each measurement
!
      do 300 N=1,NM
!
!        ....input measurement time
         mjdx = mjdin + fsecin(N)
!
         if(kentry.le.iprint .and. n.eq.1)                              &
     &   write(6,*) 'rdoffl:  n, mjdx ', n, mjdx
!
!        ....search station time array for times bounding
!        ....measurement time
!
         do 350 i=i1, i2
!
!            call mjdymd( II(KSTMJD -1 + i), iymd0, ihm0, 4 )
!            write(6,*) 'rdoffl:  i, II(KSTMJD -1 + i), iymd0, ihm0 ',
!     &                           i, II(KSTMJD -1 + i), iymd0, ihm0

            if ( mjdx .ge. II(KSTMJD -1 + i) .and.                      &
     &           mjdx .lt. II(KSTMJD -1 + i+1) ) then
               J=i
!c               go to 400
            endif
  350    continue
!
!        ....if meas time is before table
         if ( mjdx .lt. II(KSTMJD -1 + i1) ) then
            J=i1
            go to 400
         endif
!
!        ....if meas time is after table
         if ( mjdx .ge. II(KSTMJD -1 + i2) ) then
            J=i2
            go to 400
         endif
!
  400    continue
!
         antout(N) = AA(KASOFF - 1 + J)
!
         if( MOD(kentry,2) .eq. 1 ) then
            xmjddy = ( DBLE(mjdx) )/86400.D0 + TMGDN2
            call MJDYMD(mjdx,IYMD,IHMS, 4)
            write(99,'(1x, i9,1x,f12.3,1x,i4,i7,i7,1x,1d15.5 )')        &
     &         nsta0, xmjddy, n, IYMD, IHMS,  antout(n)
         endif
!
  300 continue
!
      RETURN
      END
