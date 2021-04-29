      subroutine store_cg_fan_pars(  LNPNM_L, NM_L, PMPA, &
     &                         NDIM1_L ,NDIM2_L, LNEG, &
     &                         LAVOID, cg_par_array  )


       use cgmass_module

       IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE


      INCLUDE 'COMMON_DECL.inc'
      COMMON/BIAOR/NAMBB,NCYCB,NAMBBH

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

!   NM_L     I    S    NUMBER OF MEASUREMENTS IN THE BLOCK
!   PMPA    I/O   A    PARTIALS OF MEASUREMENTS WRT PARAMETERS
!   NDIM1_L  I    S    FIRST DIMENSION OF MATRIX PMPA
!   NDIM2_L  I    S    SECOND DIMENSION OF MATRIX PMPA
!   LNPNM_L  I    S    TRUE IF THE FIRST DIMENSION OF PMPA IS THE NUMBER

!   LNEG     I    S    TRUE TO COMPUTE NEGATIVE PARTIALS (MEASUR. LINKS)


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

      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST

       INTEGER :: NDIM1_L, NDIM2_L
       double precision, dimension( NDIM1_L, NDIM2_L )  :: PMPA

       double precision, dimension( ndim_cgmass, NM_L )  :: cg_par_array

       double precision, dimension( NM_L )  :: scale_l


       LOGICAL, dimension( MAPARM )  :: LAVOID

       double precision :: FACTX
       LOGICAL :: LNPNM_L
       LOGICAL :: LNEG
       INTEGER  ::  NM_L


!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************

! return if no adjusted FANTOM parameters

       IF(NPVAL0(IXFAGM).LE.0) RETURN


!write(6,'(/A)') 'scgpar: at entry '

!write(6,'(/A)') 'scgpar: cg_par_array '
!do  i = 1,nm_l
!    write(6,'(9(1x,E12.5))') cg_par_array(1:NFGARC, i )
!enddo



       FACTX=1.D0
       IF( LNEG ) FACTX=-1.D0
         DO  I=1,NM_L
           SCALE_L(I) = FACTX
         ENDDO

!write(6,'(/A/(5(1x,E12.5)))') 'scgpar: SCALE_L ', SCALE_L(1)

!---------------------------------------------------------------


! IPT is the pointer for FANTOM parameters in PMPA


       IPT = IPVAL0(IXFAGM)

!write(6,*)'scgpar: IXFAGM   ', IXFAGM
!write(6,*)'scgpar: IPVAL0(IXFAGM), IPT ', IPVAL0(IXFAGM) , IPT


!---------------------------------------------------------------

!write(6,*)'scgpar: LNPNM_L  ', LNPNM_L
!write(6,*)'scgpar: NFGARC ', NFGARC


       do  j = 1, NFGARC
           LAVOID(IPT + j -1) = .FALSE.
       enddo



       IF(LNPNM_L) THEN

       do  I = 1,NM_L

        do  j = 1, NFGARC

            PMPA(IPT+j-1,I) = PMPA(IPT+j-1,I) + &
     &                           cg_par_array(J,I ) * scale_l(I)

            !if( I == 1 .and. mod(j,2) == 0 )then
            !    WRITE(6,'(A,2(1x,I3),2(1x,E12.5))') &
            !          'scgpar: I,j, cg_par_array(J,I), scale_l(i) ', &
            !                   I,j, cg_par_array(J,I), scale_l(i)
            !    WRITE(6,'(A,2(1x,I3),2(1x,E12.5))') &
            !          'scgpar: I,j, PMPA(IPT+j-1, I) ',          &
            !                   I,j, PMPA(IPT+j-1, I)
            !endif

        enddo  ! j


        !if( I == 1 .and. mod(j,2) == 0 )then
        !    WRITE(6,'(/A)')    'scgpar: I,(PMPA(IPT-1+j,I),j=1,NFGARC) '
        !    WRITE(6,'(I3,9(1x,E12.5))') I,(PMPA(IPT-1+j,I),j=1,NFGARC)
        !endif

       enddo  ! I

       ELSE

       do  I=1,NM_L

        do  j = 1, NFGARC

            PMPA( I, IPT+j-1 ) = PMPA( I, IPT+j-1 ) + &
     &                            cg_par_array(J,I ) * scale_l(I)

            !if( I == 1 .and. mod(j,2) == 0 )then
            !    WRITE(6,'(A,2(1x,I3),2(1x,E12.5))') &
            !          'scgpar:2 I,j, cg_par_array(J,I), scale_l(i) ', &
            !                   I,j, cg_par_array(J,I), scale_l(i)
            !    WRITE(6,'(A,2(1x,I3),2(1x,E12.5))') &
            !          'scgpar:2 I,j, PMPA(I,IPT+j-1) ',          &
            !                   I,j, PMPA(I,IPT+j-1)
            !endif

        enddo  ! j

        !if( I == 1 .and. mod(j,2) == 0 )then
        !    WRITE(6,'(/A)')    'scgpar:2 I,(PMPA(I,IPT+j-1),j=1,NFGARC)
        !    WRITE(6,'(I3,9(1x,E12.5))')  I,(PMPA(I,IPT+j-1),j=1,NFGARC)
        !endif


       enddo   ! I

       ENDIF ! LNPNM_L


!write(6,*)' '
!write(6,*)'scgpar: at RETURN '

       RETURN


       END
