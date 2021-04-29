!$RDOFFP
      SUBROUTINE RDOFFP( nsta0, mjdin , fsecin, nm, antout ,AA, II )
!********1*********2*********3*********4*********5*********6*********7**
! RDOFFP           94/09/15            9404.x    PGMR - J. McCarthy
!
! FUNCTION: READ ANTENNA AXIS DISPLACEMENT CORRECTIONS DUE TO PRESSURE
!            FROM STATION FILE
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
!      parameter ( N_STA = 50 )
!      parameter ( N_TIME = 20 )
!      parameter ( N_MAX = N_STA * N_TIME )
      SAVE
!
!      DIMENSION  nrd( N_STA )
!     DIMENSION antout(500), fsecin(*)
      DIMENSION antout(1), fsecin(*)
      DIMENSION  AA(*)
      DIMENSION  II(*)
!     DIMENSION fstemp(1)
!
      COMMON/CMET/P0,T0,RELHUM,E0,WAVLEN
      COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
     &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
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
      DATA ZERO/0.0D0/,ONE/1.0D0/
      DATA ifirst/0/
      DATA kentry/0/
      DATA iprint/5/
!
!     ....Pstd is standard pressure
!     ....dpdh is the lapse rate (change of pressure with height change)
      DATA Pstd/1013.5D0/, dpdh/-1.1138D-4/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      kentry = kentry + 1
!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
!     ....open unit for station height offset printout
      IF(ifirst .le. 0 ) then
          ifirst=1
          OPEN(99,FILE='fort.99',STATUS='NEW',FORM='FORMATTED',         &
     &         BLANK='ZERO')
            write(99,5000)
 5000 format(1x,'PRESSURE LOADING ON STATION HEIGHTS'                   &
     &   /1x, 'YYMMDD', 'HHMMSS ', 'station ',                          &
     &     '   ht chg', '   press ', '   std pr', '     scale',         &
     &      '    height'/                                               &
     &        1x, '      ', '       ', 1x,'        ',                   &
     &     '   x1E6 ', '   x1000 ', '   x1000 ', '      x1E6  ',        &
     &     '   x1000 ')
!
      endif
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!
!     ....clear output array before starting
      do 220 N=1,NM
         antout(N) = 0.0D0
  220 continue
!
  200 continue
!
!     ....input station number is nsta0
!
      call FNDNUM( nsta0, ii(KNUMST), NSTNUM, ista )
!
      if( ista .le. 0 ) then
         if(kentry.le.iprint ) then
            if( NALLST .le. 0 ) then
               write(6,*) 'rdoffP: all station option NALLST ', NALLST
               write(6,*) 'Use only stations in station list in file'
               write(6,*) 'station number ', nsta0,                     &
     &                      ' not in station list'
               write(6,*) 'zero station antenna offset applied '
            endif
         endif
!
        if( NALLST .gt. 0 ) then
!
!           ....NALLST > 0  means all unspecified stations
!           ....get Pstd and scale = scale0
            scale0 = AA(KASCAL)
!
!           ....compute the correct offset for each measurement
!           ....using standard pressure and scale=scale0
!
            if(kentry.le.iprint ) then
               write(6,*) 'rdoffP: all station option NALLST ', NALLST
               write(6,*) 'station number ', nsta0,                     &
     &                      ' not in station list'
               write(6,*) 'apply standard pressure offset      '
            endif
!
            do 310 N=1,NM
!
!              ....input measurement time
               mjdx = mjdin + fsecin(N)
!
!               if(kentry.le.iprint .and. n.eq.1) then
!               if(kentry.le.iprint ) then
!                 write(6,*) 'rdoffP:  n, mjdx ', n, mjdx
!               endif
!
!              ....P0x       = P0
!              ....Pmean  = Pstd * ( one + dpdh * height )
!              ....antout(N) = ( P0x - Pmean ) * scale0
!
!              ....P0 is the pressure from the data
!              ....if P0 is zero, then use standard pressure
!
               if( P0 .gt. 0.0D0 ) then
                   P0x       = P0
               else
                   P0x       = Pstd * ( one + dpdh * height )
               endif
!
                   PstdH       = Pstd * ( one + dpdh * height )
!
!              ....antenna offset is -0.5 mm for 1 mbar of pressure
!              ....NOTE: antenna offset is zero if no pressure on data
!
               antout(N) = +(P0x - PstdH) * scale0
!
!
!
         if( MOD(kentry,2) .eq. 1 ) then
            xmjddy = ( DBLE(mjdx) )/86400.D0 + TMGDN2
            call MJDYMD(mjdx,IYMD,IHMS, 4)

        iant6 = antout(n) * 1.D6
        iP0x6  = P0x  * 1000.D0
        iPst6 =  PstdH * 1000.D0
!               if(kentry.le.iprint ) then
!                 write(6,*) 'rdoffP:  PstdH, iPst6 ',PstdH, iPst6
!               endif
        iscl6 =  scale0 * 1.D6
        ihts6 =  height * 1000.D0
!
            write(99,'(1x, i6,i6,i9,3I9,2I10 )')                        &
     &         IYMD, IHMS,  nsta0, iant6, iP0x6, iPst6, iscl6, ihts6

!            write(6,* )IYMD, IHMS,  nsta0, iant6, iP0x6, iPst6, iscl6
         endif
!
!
!               if(kentry.le.iprint .and. n.eq.1) then
!               if(kentry.le.iprint ) then
!                  write(6,*) 'rdoffP:  n,  Pstd , dpdh, height ',
!     &                                 n,  Pstd , dpdh, height
!                  write(6,*) 'rdoffP: n, P0,PstdH,P0x ',n,P0,PstdH,P0x
!                  write(6,*) 'rdoffP:  n, antout(n) ',
!     &                                 n, antout(n)
!               endif
  310       continue
         endif
!
         return
      endif
!     ....end of case where station number is zero
!
!     ....i1 and i2 are the bounding indices in the array for the
!     ....station "nsta0"
!
      i1 = II(KITAST -1 +  ista )   + 1
      i2 = II(KITAST -1 +  ista+1 )
!
       if(kentry.le.iprint ) then
       write(6,*) 'rdoffP: station number found in table ! '
       write(6,*) 'rdoffP:  nsta0,ista, i1,i2  ',nsta0, ista, i1,i2
       endif

!     ....find the correct offset for each measurement
!
      do 300 N=1,NM
!
!        ....input measurement time
         mjdx = mjdin + fsecin(N)
!
!         if(kentry.le.iprint .and. n.eq.1)
!         if(kentry.le.iprint )
!     &   write(6,*) 'rdoffP:  n, mjdx ', n, mjdx
!
!        ....search station time array for times bounding
!        ....measurement time
!
         do 350 i=i1, i2
!
!            call mjdymd( II(KSTMJD -1 + i), iymd0, ihm0, 4 )
!            write(6,*) 'rdoffP:  i, II(KSTMJD -1 + i), iymd0, ihm0 ',
!     &                           i, II(KSTMJD -1 + i), iymd0, ihm0
!            if(kentry.le.iprint .and. n.eq.1)
!     &      write(6,*) 'rdoffP:  i, II(KSTMJD -1 + i) ',
!     &                           i, II(KSTMJD -1 + i)
!            if(kentry.le.iprint .and. n.eq.1)
!     &      write(6,*) 'rdoffP:  i, II(KSTMJD -1 + i+1) ',
!     &                           i, II(KSTMJD -1 + i+1)
!
            if ( mjdx .ge. II(KSTMJD -1 + i) .and.                      &
     &           mjdx .lt. II(KSTMJD -1 + i+1) ) then
               J=i
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
!        ....P0 is the pressure from the data
!        ....if P0 is zero, then use standard pressure
!
         if( P0 .gt. 0.0D0 ) then
             P0x = P0
          else
             P0x       = Pstd * ( one + dpdh * height )
         endif
!
!        ....antenna offset is scale* (pressure - mean pressure)
!        ....note that values on card are used without scaling!
!
         antout(N) = ( P0x - AA(KAPRES -1 + J) ) * AA(KASCAL -1 + J)
!
!>>>>>>>
        if ( AA(KAPRES -1 + J) .le. 0.0D0 ) then
           PstdH       = Pstd * ( one + dpdh * height )
           antout(N) = ( P0x - PstdH  ) * AA(KASCAL -1 + J)
!           if(kentry.le.iprint ) then
!              write(6,*) 'rdoffP:  PstdH, antout(N) ',PstdH, antout(N)
!              write(6,*) 'rdoffP:  P0x, ascale(J) ',P0x, ascale(J)
!           endif
        endif

!
         if( MOD(kentry,2) .eq. 1 ) then
            xmjddy = ( DBLE(mjdx) )/86400.D0 + TMGDN2
            call MJDYMD(mjdx,IYMD,IHMS, 4)

        iant6 = antout(n) * 1.D6
        iP0x6  = P0x  * 1000.D0
        iPst6 =  AA(KAPRES -1 + J) * 1000.D0

        if ( iPst6 .le. 0 ) then
           PstdH       = Pstd * ( one + dpdh * height )
           iPst6 =  PstdH * 1000.D0
!           if(kentry.le.iprint ) then
!              write(6,*) 'rdoffP:  PstdH, iPst6 ',PstdH, iPst6
!           endif
        endif

        iscl6 = AA(KASCAL-1+J) * 1.D6
        ihts6 =  height    * 1000.D0
            write(99,'(1x, i6,i6,i9,3I9,2I10 )')                        &
     &         IYMD, IHMS,  nsta0, iant6, iP0x6, iPst6, iscl6, ihts6
!            write(6,* )IYMD, IHMS,  nsta0, iant6, iP0x6, iPst6, iscl6
         endif
!
!         if(kentry.le.iprint .and. n.eq.1) then
!         if(kentry.le.iprint ) then
!         write(6,*) 'rdoffP:  n, j, P0x ', P0x
!         write(6,*) 'rdoffP:  P0, Pstd, P0x ', P0, Pstd, P0x
!         write(6,*)
!     &'rdoffP:  n, j, AA(KAPRES-1+J), AA(KASCAL-1+J), antout(n) ',
!     &        n, j, AA(KAPRES -1 + J), AA( KASCAL -1 + J), antout(n)
!         endif
  300 continue
!
      RETURN
      END
