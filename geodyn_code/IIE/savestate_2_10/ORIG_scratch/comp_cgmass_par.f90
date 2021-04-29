      subroutine comp_cgmass_par( AA, II, &
     &                       kprmvl, kprmv0l, kprmvcl, &
     &                      kprmsgl, kprsg0l, &
     &                      PARMSG, PARMSG0, &
     &                      mjds, fsec, fsec_ut, &
     &                      ecgcrx, ecgcry, ecgcrz, &
     &                      sbftor, &
     &                      meas_uvx, meas_uvy, meas_uvz, &
     &                      NM_L, imeas, cg_par_array, KARGR1L  )

      use cgmass_module

      implicit none


      INCLUDE 'COMMON_DECL.inc'
      COMMON/CASPER/NFGGLB,NFFGLB,NFGARC,NFFARC,NGGA,NGFA,NAGA,NAFA,    &
     &              NFANEP,NFDIR(4),NSATFA(2,200),NFANTM,NXCASP



!           *CASPER*  NUMBER OF PARAMETERS IN THE FANTOM GROUP
!
!            NFGGLB - GEOMETRIC MODEL GLOBAL PARAMETERS
!            NFFGLB - FORCE     MODEL GLOBAL PARAMETERS
!            NFGARC - GEOMETRIC MODEL ARC    PARAMETERS
!            NFFARC - FORCE     MODEL ARC    PARAMETERS
!            NGGA   - ADJUSTED GEOM GLOBAL PARAMETERS
!            NGFA   - ADJUSTED FORCE  GLOBAL PARAMETERS
!            NAGA   - ADJUSTED GEOM ARC PARAMETERS
!            NAFA   - ADJUSTED FORCE  ARC PARAMETERS
!            NFANEP - NUMBER OF FANTOM GROUPS THAT REQUEST AN EPOCH TIME
!            NFDIR  - NUMBER OF FANTOM PARAMETER DIRECTIONS
!            NSATFA - ARRAY FOR UNADJ/ADJ NUMBER OF ARC FANTOM PARAM.
!            NFANTM - NUMBER OF FANTOM GROUPS THAT REQUEST A  START TIME
!            NXCASP - NUMBER OF WORDS IN THIS COMMON BLOCK
!

      COMMON/FANTOM/LFGLB,LFARC,LFTARC,LFTGLB,LENDGL,LPHNTM(4),LPHTIM(4)&
     &             ,LXTRAF,LSATFA(4,3),NXFANT
!
!           *FANTOM* LOGICAL FLAGS FOR IDENTIFYING THE FANTOM
!                    OPTION AS GLOBAL OR ARC
!            LFGLB  -.FALSE. IF NO FANTOM CARD EXISTS IN THE ARC  SECTIO
!            LFARC  -.FALSE. IF NO FANTOM CARD EXISTS IN THE GLOBAL SECT
!            LFTGLB  -.FALSE. IF NO FANTIM CARD EXISTS IN THE ARC  SECTI
!            LFTARC  -.FALSE. IF NO FANTIM CARD EXISTS IN THE GLOBAL SEC
!            LENDGL -.TRUE.  IF ENDGLB CARD HAS BEEN READ ALREADY
!            LPHNTM - SWITCHES FOR FANTOM PARAMETERS
!                     (1)- GLOBAL GEOMETRIC
!                     (2)- GLOBAL FORCE
!                     (3)- ARC GEOMETIRIC
!                     (4)- ARC FORCE
!            LPHTIM - SWITCHES FOR FANTOM EPOCH OR APPLICATION START TIM
!                     (1)- GLOBAL GEOMETRIC
!                     (2)- GLOBAL FORCE
!                     (3)- ARC GEOMETIRIC
!                     (4)- ARC FORCE
!            LXTRAF - TRUE IF ONE NEEDS TO ALLOCATE FOR EXTRA REAL INFOR
!                     USING COLS 60-72 and 73-80 ON THE FANTOM CARD
!            LSATFA - LOGICAL FOR  UNADJUSTED/ADJUSTED FANTOM ARC FORCE
!                     PARAMETERS
!            NXFANT - NUMBER OF WORDS IN THIS COMMON BLOCK
!
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

!            IXFAGM - ARC FANTOM GEOMETRIC MODEL PARAMETERS
!            IXFAFM - ARC FANTOM FORCE MODEL PARAMETERS

!            IXFGGM - GLOBAL FANTOM GEOMETRIC MODEL PARAMETERS
!            IXFGFM - GLOBAL FANTOM FORCE MODEL PARAMETERS

      COMMON/SPOOKY/EPTIME(4),STFTIM(4),SPOOK1(4),SPOOK2(4),XSPOOK

!           *SPOOKY* TIME AND OTHER REAL INFORMATION THAT WOULD
!                    APPLY TO FANTOM PARAMETERS
!                    THE FOUR SLOTS ARE:
!                    1=GLOBAL GEOMETRIC PARAMETERS
!                    2=GLOBAL FORCE PARAMETERS
!                    3=ARC GEOMETRIC PARAMETERS
!                    4=ARC FORCE PARAMETERS
!
!           EPTIME - EPOCH TIME FOR A TIME DEPENDENT MODEL
!           STFTIM - START TIME FOR A TIME DEPENDENT OPTION
!           SPOOK1 - REAL INFORMATION TO BE DETERMINED BY THE USER
!           SPOOK2 - REAL INFORMATION TO BE DETERMINED BY THE USER
!                    SPOOK1 AND SPOOK2 APPLY TO ALL THE PARAMETERS
!                    IN A GROUP (GROUPS 1-4 ABOVE)
!           XSPOOK - NUMBER OF REAL WORDS IN THIS COMMON BLOCK
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY

!           *CONSTR*  CONVERSION CONSTANTS USED BE IIS AND IIE
!
!            PI     - PI IN RADIANS     (D=3.1415926535897932D0)
!            TWOPI  - TWO PI IN RADIANS (D=6.2831853071795864D0)
!            DEGRAD - DEGREES TO RADIAN CONVERSION.
!                      DEGREES*DEGRAD=RADIANS  (D=.017453292519943296D0)
!            SECRAD - SECONDS OF ARC TO RADIANS (D=.484813681109536D-5)
!            SECDAY - SECONDS OF TIME IN A DAY(D=8.64D4)
!            XCONST - NUMBER OF WORDS IN THIS COMMON BLOCK

      COMMON/NPCOM /NPNAME,NPVAL(92),NPVAL0(92),IPVAL(92),IPVAL0(92),   &
     &              MPVAL(28),MPVAL0(28),NXNPCM

!
!           *NPCOM *  NUMBER OF ADJUSTED AND UNADJUSTED PARAMETERS AND
!                     POINTERS TO EACH MAJOR TYPE OF PARAMETER
!            NPNAME - DIMENSION OF ALL NP ARRAYS
!            NPVAL  - ARRAY CONTAINING THE NO. OF EACH TYPE OF PARAMETER
!            NPVAL0 - ARRAY CONTAINING THE NUMBER OF EACH TYPE
!                     OF ADJUSTED PARAMETER
!            IPVAL  - POINTERS FOR PARMV ARRAYS BASED ON THE
!                     NUMBER OF PARAMETERS INDICATED IN NPVAL.
!                     IPVAL(IXARC)-IPVAL(IXGLBL-1) MUST BE
!                     RECALCULATED FOR EACH ARC.
!            IPVAL0 - POINTERS FOR PARMV0 ARRAYS BASED ON THE
!                     NUMBER OF PARAMETERS INDICATED IN NPVAL0.
!                     IPVAL0(IXARC)-IPVAL0(IXGLBL) MUST BE
!                     RECALCULATED FOR EACH ARC.
!            MPVAL  - MAXIMUM VALUES FOR NPVAL  OVER ALL ARCS
!            MPVAL0 - MAXIMUM VALUES FOR NPVAL0 OVER ALL ARCS.
!            NXNPCM - NUMBER OF WORDS IN THIS COMMON BLOCK



       double precision, dimension(3,3) ::  SBFTOR
       double precision, dimension(3)   ::  meas_uv
       double precision, dimension(3)   ::  meas_uv_sbf
       double precision, dimension(3)   ::  vec_sbf
       double precision, dimension(3)   ::  vec_tor

       double precision ::  vec_tor_mag
       double precision ::  meas_uvx, meas_uvy, meas_uvz


       INTEGER :: mjds
       INTEGER :: imeas
       INTEGER ::  kprmvl, kprmvcl, kprmsgl
       INTEGER ::  kprsg0l
       INTEGER ::  kprmv0l
       INTEGER ::  NM_L
       INTEGER ::  i
       INTEGER ::  j
       INTEGER ::  k
       INTEGER ::  KARGR1L
       double precision ::  time
       double precision ::  fsec
       double precision ::  fsec_ut
       double precision ::  period
       double precision ::  ecgcrx, ecgcry, ecgcrz
       double precision ::  F_ecgcrx, F_ecgcry, F_ecgcrz


       double precision, dimension( ndim_cgmass, NM_L )  :: cg_par_array



       double precision, dimension(ndim_cgmass)   ::  pmpcg
       double precision, dimension(ndim_cgmass)   ::  pcgpa

       double precision, dimension(ndim_cgmass)   ::  cg_value


       double precision, dimension(ndim_cgmass)   ::  fan_period

       double precision, dimension(*)   ::  AA
       double precision, dimension(*)   ::  PARMSG
       double precision, dimension(*)   ::  PARMSG0

       INTEGER,          dimension(*)   ::  II


       INTEGER,save ::  kentry = 0


       double precision, parameter  :: model_scale  =  +1.0D0

       INTEGER, parameter :: ncomp  = 5  ! number of parameters per compo

! icesat
      double precision, parameter :: sat_period = &
     &                               0.9657308216D+02*60.D0


       double precision :: freq1    ! = twopi / sat_period
       double precision :: freq2    ! = freq1 * 2.0d0

!------------------------------------------------------------------------------

       if( NPVAL(IXFAGM)  <= 0 ) return

       if( NPVAL(IXFAGM) < ncomp * 3 ) then

       write(6,'(A)') 'cgpar: not enough fantom parameters !!! '
       write(6,'(A,1x,I5)') 'cgpar: NPVAL(IXFAGM) = ', NPVAL(IXFAGM)
       write(6,'(A,1x,I5)') 'cgpar: ncomp*3       = ', ncomp*3
       stop 'cgpar:bad no. fantom'

       endif

       kentry = kentry + 1




       if( kentry == 1 ) then
    !&rite(6,'(/A/)') 'cgpar:  at entry      '
       write(6,*)'cgpar:  kentry ', kentry
    !&!!!!11write(6,*)'cgpar:  model_scale negative ', model_scale
       write(6,*)'cgpar:  model_scale ', model_scale
       write(6,*)'cgpar:  bias + 1perrev + 2perrev  '
       endif




! time is utc from start of the cgmass file

!time = mjds + fsec - eptime(3)
!orig time = dble( mjds )  + fsec_ut - eptime(3)

! eptime(3) is in UTC

! numbers in external file were computed using time in UTC
! so use fsec_ut  to compute the time instead of fsec
! using fsec_ut is correct - jjm 20120416



      time = ( DBLE( mjds ) - eptime(3) ) + fsec_ut  !orig
!time = ( dble( mjds ) - eptime(3) ) + fsec ! debug only


!if( imeas == 1 )then
!    write(6,'(A,1x,I10,1x,F17.4, 2(1x,F10.3),1x,F17.4)')&
!          'cgpar: mjds, eptime(3), fsec, fsec_ut, time ',&
!                  mjds, eptime(3), fsec, fsec_ut, time
!endif ! imeas == 1

!------------------------------------------------------

! unit vector of the measurement

      meas_uv(1) = meas_uvx
      meas_uv(2) = meas_uvy
      meas_uv(3) = meas_uvz

!if( imeas == 1 )then
!    write(6,'(A,3(1x,E20.10))')   &
!          'cgpar: unit vector meas_uv ', meas_uv(1:3)
!endif ! imeas == 1
!write(6,'(A,3(1x,E20.10))')   &
!      'cgpar: meas_uvx, meas_uvy, meas_uvz ', &
!              meas_uvx, meas_uvy, meas_uvz
!------------------------------------------------------


!write(6,*)'cgpar:  eptime(3)     ', eptime(3)
!write(6,*)'cgpar:  period (sec)  ', period

!write(6,*)'cgpar:  MJDS, FSEC    ', MJDS, FSEC
!write(6,*)'cgpar:  TIME          ', TIME




!------------------------------------------------------------------


      if( kentry == 1 )then

    ! fill period array with input periods

      fan_period = 0.0D0

      do  i = 1, NFGARC

        !write(6,'(A,1x,I4,1x,E15.7)') 'cgpar: i, aa( KARGR1L-1+i ) ', &
        !                                      i, aa( KARGR1L-1+i )

        if( aa( KARGR1L-1+i ) > 0.0D0 )then
            fan_period(i) = aa( KARGR1L-1+i )
        endif

      enddo  ! i


      write(6,'(/A)') 'cgpar: fan_period array '
      do  i = 1, nfgarc
        write(6,'(I3,1x,E15.7)')  i, fan_period(i)
      enddo

      endif

!------------------------------------------------------------------

! default freqencies


      freq1 = twopi / sat_period
      freq2 = freq1 * 2.0D0


! partials


! X
      pcgpa(         1)  =  1.0D0
      pcgpa(         2)  =  SIN( time * freq1 )
      pcgpa(         3)  =  COS( time * freq1 )
      pcgpa(         4)  =  SIN( time * freq2 )
      pcgpa(         5)  =  COS( time * freq2 )

! Y
      do  i = 1, 5
      pcgpa( ncomp + i)  =  pcgpa( i )
      enddo

! Z
      do  i = 1, 5
      pcgpa( 2*ncomp + i)  =  pcgpa( i )
      enddo



!-------------------------------------------------------------------
      i = 2
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = SIN( time * twopi / fan_period(i) )
      endif
      i = 3
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = COS( time * twopi / fan_period(i) )
      endif

      i = 4
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = SIN( time * twopi  / fan_period(i) )
      endif
      i = 5
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = COS( time * twopi  / fan_period(i) )
      endif

!-------------------------------------------------------------------
      i = 7
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = SIN( time * twopi / fan_period(i) )
      endif
      i = 8
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = COS( time * twopi / fan_period(i) )
      endif

      i = 9
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = SIN( time * twopi / fan_period(i) )
      endif
      i = 10
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = COS( time * twopi / fan_period(i) )
      endif

!-------------------------------------------------------------------
      i = 12
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = SIN( time * twopi / fan_period(i) )
      endif
      i = 13
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = COS( time * twopi / fan_period(i) )
      endif

      i = 14
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = SIN( time * twopi / fan_period(i) )
      endif
      i = 15
      if( fan_period(i) > 0.0D0 )then
       pcgpa(i) = COS( time * twopi / fan_period(i) )
      endif

!-------------------------------------------------------------------


!------------------------------------------------------

! apply an overall scale factor for testing
! model_scale is either 1 or -1

       do  j = 1, ndim_cgmass
         pcgpa(j)  =  pcgpa(j) * model_scale
       enddo

!------------------------------------------------------

!if( imeas == 1 )then
!    write(6,'(A)')'cgpar:0  pcgpa  -- after scaling  '
!    !do  j = 1, ndim_cgmass
!    !    write(6,'(I3,1x,E20.10)') j, pcgpa(j)
!    !enddo
!
!    write(6,'(6(1x,E15.7 ))') pcgpa(1:ndim_cgmass)
!endif ! imeas == 1
!
!------------------------------------------------------

! compute the current values for the CGMASS offsets

!write(6,'(A)') ' '

!write(6,*)'cgpar:  IXFAGM, IPVAL(IXFAGM) ',  IXFAGM, IPVAL(IXFAGM)

!write(6,*)'cgpar:  (j, NPVAL0( IPVAL(IXFAGM) -1 + j ),j=1,20 ) ', &
!                   (j, NPVAL0( IPVAL(IXFAGM) -1 + j ),j=1,20 )

!write(6,*)'cgpar: NPVAL(IXFAGM)  ', NPVAL(IXFAGM)
!write(6,*)'cgpar: NPVAL0(IXFAGM) ', NPVAL0(IXFAGM)

!write(6,*)'cgpar:  (j, aa( kprmvl  + IPVAL(IXFAGM) -1 + j ),j=0,NFGARC-1 ) '
!do  j=0, NFGARC-1
!    write(6,'(I3,1x,E20.10)') j, aa( kprmvl  + IPVAL(IXFAGM) -1 + j )
!enddo

!write(6,*)'cgpar:  (j, aa( kprmvcl  + IPVAL0(IXFAGM) -1 + j ),j=0,NAGA-1 ) '
!do  j=0, NFGARC -1
!    write(6,'(I3,1x,E20.10)') j, aa( kprmvcl  + IPVAL0(IXFAGM) -1 + j )
!enddo

!write(6,*)'cgpar:  (j, aa( kprmv0l  + IPVAL0(IXFAGM) -1 + j ),j=0,NAGA-1 ) '
!do  j=0, NFGARC -1
!    write(6,'(I3,1x,E20.10)') j, aa( kprmv0l  + IPVAL0(IXFAGM) -1 + j )
!enddo

!write(6,*)'cgpar:  (j, aa( kprmsgl  + IPVAL0(IXFAGM) -1 + j ),j=0,NAGA-1 ) '
!do  j=0, NAGA -1
!    write(6,'(I3,1x,E20.10)') j, aa( kprmsgl  + IPVAL0(IXFAGM) -1 + j )
!enddo
!
!write(6,*)'cgpar:  (j, aa( KPRSG0L  + IPVAL0(IXFAGM) -1 + j ),j=0,NAGA-1 ) '
!do  j=0, NAGA -1
!    write(6,'(I3,1x,E20.10)') j, aa( KPRSG0L  + IPVAL0(IXFAGM) -1 + j )
!enddo

!write(6,*)'cgpar:  (j, PARMSG(  IPVAL0(IXFAGM)  + j ),j=0,NFGARC-1 ) '
!do  j=1, NFGARC
!    write(6,'(I3,1x,E20.10)') j, PARMSG(  IPVAL(IXFAGM) -1 + j )
!enddo

!write(6,*)'cgpar:  (j, PARMSG0(  IPVAL0(IXFAGM) + j ),j=0,NAGA-1 ) '
!!do  j=1, NFGARC
!    write(6,'(I3,1x,E20.10)') j, PARMSG0(  IPVAL0(IXFAGM) -1 + j )
!enddo


!write(6,'(A)') ' '

      cg_value = 0.0D0

      do  j = 0, NFGARC - 1

    !&rite(6,'(I3,1x,E20.10)') j, PARMSG0( IPVAL0(IXFAGM) + j )

    ! is it adjusted?

        if( PARMSG0( IPVAL0(IXFAGM) + j ) >  0.0D0 )then

        !write(6,'(A, 1x,I3,1x,E20.10)') &
        !      'cgpar: j, aa( kprmvcl + IPVAL0(IXFAGM) -1 + j ) ', &
        !              j, aa( kprmvcl + IPVAL0(IXFAGM) -1 + j )

        ! current value of adj. par.

        cg_value(j+1)  = aa( kprmvcl + IPVAL0(IXFAGM) -1 + j )

       else

        ! not adjusted
        ! test if non-adj value exists

        ! is there a non-adj value?

        !write(6,'(A, 1x,I3,1x,E20.10)') &
        !      'cgpar: j, aa( kprmvl  + IPVAL(IXFAGM) -1 + j )  ', &
        !              j, aa( kprmvl  + IPVAL(IXFAGM) -1 + j )

        ! unadj value

        cg_value(j+1) = aa( kprmvl + IPVAL(IXFAGM) -1 + j )

        !endif

       endif ! PARMSG0( IPVAL0(IXFAGM) + j ) > 0.0d0

    !&rite(6,'(A,1x,I3,1x,E20.10)') 'cgpar:1 j+1, cg_value(j+1) ',  &
    !                                        j+1, cg_value(j+1)

       enddo ! j

!-------------------------------------------------------------------------

! ecgcrx, etc. are the cgmass offsets from the cgmass file
! compare these to the model cgmass offsets F_ecgcrx, etc.

!write(6,'(A,3(1x,E20.10))') &
!      'cgpar: before ecgcrx, ecgcry, ecgcrz  ', &
!                     ecgcrx, ecgcry, ecgcrz


      do  i = 1, ncomp

       F_ecgcrx = F_ecgcrx + cg_value(i)         * pcgpa(i)
       F_ecgcry = F_ecgcry + cg_value(i+ncomp)   * pcgpa(i+ncomp)
       F_ecgcrz = F_ecgcrz + cg_value(i+ncomp*2) * pcgpa(i+ncomp*2)

      enddo

!write(6,'(A/(5(1x,E15.7 )))') 'cgpar: cg_value(1:ndim_cgmass) ', &
!                                      cg_value(1:ndim_cgmass)
!write(6,'(A,6(1x,E15.7 ))')   'cgpar: pcgpa(1:ndim_cgmass )   ', &
!                                      pcgpa(1:ndim_cgmass)
!write(6,'(A,3(1x,E24.16))') &
!      'cgpar: F_ecgcrx, F_ecgcry, F_ecgcrz  ', &
!              F_ecgcrx, F_ecgcry, F_ecgcrz



!write(6,'(A,1x,I3)') 'cgpar: NFGARC ', NFGARC

!------------------------------------------------------------------

! compute the partials of the FANTOM parameters

      do  j = 1, ndim_cgmass

    !&rite(6,'(A,1x,I3,1x,E20.10)') &
    !      'cgpar:2 j, PARMSG0( IPVAL0(IXFAGM) -1 + j ) ', &
    !               j, PARMSG0( IPVAL0(IXFAGM) -1 + j )

          if( PARMSG0( IPVAL0(IXFAGM) -1 + j ) > 0.0D0 ) then

        ! partial of meas wrt cg 1 parm

            if( j <= ncomp ) then

            vec_sbf(1) = pcgpa(j)
            vec_sbf(2) = 0.0D0
            vec_sbf(3) = 0.0D0

            !write(6,'(/A,1x,I3, 3(1x,E20.10))')   &
            !       'cgpar: j, vector vec_sbf ', j, vec_sbf(1:3)

          elseif( j <= 2*ncomp )then

            vec_sbf(1) = 0.0D0
            vec_sbf(2) = pcgpa(j)
            vec_sbf(3) = 0.0D0

            !write(6,'(/A,1x,I3, 3(1x,E20.10))')   &
            !       'cgpar: j, vector vec_sbf ', j, vec_sbf(1:3)

          elseif( j <= ndim_cgmass )then

            vec_sbf(1) = 0.0D0
            vec_sbf(2) = 0.0D0
            vec_sbf(3) = pcgpa(j)

            !write(6,'(/A,1x,I3, 3(1x,E20.10))')   &
            !       'cgpar: j, vector vec_sbf ', j, vec_sbf(1:3)

        endif


        !write(6,*)' '

        !----------------------------------------------------------------
        vec_tor = 0.0D0

        do  i = 1, 3
            do  k = 1, 3
                vec_tor(i) = vec_tor(i) + sbftor(i,k) * vec_sbf(k)
            enddo
        enddo

        !----------------------------------------------------------------

        ! meas_uv_sbf is the measurement unit vector in the SBF

        meas_uv_sbf = 0.0D0
        do  i = 1, 3
            do  k = 1, 3
            meas_uv_sbf(i) = meas_uv_sbf(i) + sbftor(k,i) * meas_uv(k)
            enddo
        enddo
        !----------------------------------------------------------------

        !write(6,'(A,3(1x,E20.10))')   &
        !           'cgpar:      vector vec_sbf ', vec_sbf(1:3)
        !if( imeas == 1 )then
        !write(6,'(A,3(1x,E20.10))')   &
        !           'cgpar:      vector vec_tor ', vec_tor(1:3)
        !endif
        !write(6,'(A,3(1x,E20.10))')   &
        !           'cgpar: unit vector meas_uv ', meas_uv(1:3)

        !write(6,'(A,3(1x,E20.10))')   &
        !           'cgpar: unit vector meas_uv_sbf ', meas_uv_sbf(1:3)

        !pmpcg(j) = dot_product( meas_uv, vec_tor )


        pmpcg(j) = dot_product( meas_uv_sbf, vec_sbf )


        !if( imeas == 1 )then
        !    write(6,'(A,1x,E20.10)') 'cgpar:4  pmpcg(j) ',  pmpcg(j)
        !    write(6,'(A,1x,E20.10)') &
        !          'cgpar:4  dot_product( meas_uv_sbf, vec_sbf ) ', &
        !                    dot_product( meas_uv_sbf, vec_sbf )
        !    write(6,'(A,1x,E20.10)') &
        !          'cgpar:4  dot_product( meas_uv    , vec_tor ) ', &
        !                    dot_product( meas_uv    , vec_tor )
        !endif ! imeas == 1


       endif  ! NFGARC  and PARMSG0

       enddo ! j

!------------------------------------------------------


! now the array pmpcg is filled

!------------------------------------------------------

! put the array pmpcg into the array of partials

!write(6,'(/A/)')&
!      'cgpar:  put the array pmpcg into the array of partials '


      do  j = 0, NFGARC-1

    !&rite(6,'(A,1x,I3,1x,E20.10)') &
    !      'j, PARMSG0( IPVAL0(IXFAGM) + j ) ',  &
    !       j, PARMSG0( IPVAL0(IXFAGM) + j )


         if( PARMSG0( IPVAL0(IXFAGM) + j ) > 0.0D0 ) then

         cg_par_array( j+1, imeas ) = pmpcg(j+1)

        !write(6,'(A,1x,I3,1x,E20.10)') 'cgpar: j+1, pmpcg(j+1)  ', &
        !                                       j+1, pmpcg(j+1)

          endif

       enddo

!do  j = 1, NFGARC
!    write(6,'(A,2(1x,I3),1x,E20.10)') &
!          'cgpar: imeas, j, cg_par_array(j,imeas)  ', &
!                  imeas, j, cg_par_array(j,imeas)
!enddo

!if( imeas == 1 )then

    !&rite(6,'(/A)')  'cgpar: cg_par_array '
    !&rite(6,'(6(1x,E15.7))') cg_par_array( 1:ndim_cgmass, imeas)


    !&ec_sbf(1) = pcgpa(2)
    !&ec_sbf(2) = pcgpa(4)
    !&ec_sbf(3) = pcgpa(6)

    !&ec_tor = 0.0d0

    !&o  i = 1, 3
    !    do  k = 1, 3
    !        vec_tor(i) = vec_tor(i) + sbftor(i,k) * vec_sbf(k)
    !    enddo
    !&nddo

    !&rite(6,'(A,1x,F15.4, 3(1x,E20.10))')   &
    !       'cgpar: mjds+fsec   vector vec_sbf ', &
    !          dble(mjds) + fsec, vec_sbf(1:3)
    !&rite(6,'(A,3(1x,E20.10))')   &
    !       'cgpar:      vector vec_tor ', vec_tor(1:3)
    !&rite(6,'(A,3(1x,E20.10))')   &
    !       'cgpar:      vector meas_uv ', meas_uv(1:3)
    !&rite(6,'(A,1x,E20.10)') &
    !      'cgpar:  dot_product( meas_uv    , vec_tor ) ', &
    !               dot_product( meas_uv    , vec_tor )
!endif ! imeas == 1

!write(6,'(/A/)') 'cgpar: at return  '

      return



      end  subroutine comp_cgmass_par
