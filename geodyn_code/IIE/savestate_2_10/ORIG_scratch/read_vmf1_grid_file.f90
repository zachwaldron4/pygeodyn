
!       END PROGRAM READ_OROGRAPHY
      subroutine read_vmf1_grid_file( mjdsec_in, fsec_in, &
     &                          lon_in, lat_in, &
     &                          ah, aw, zdh, zdw )


      use vmf_module


      implicit none

      INTEGER, intent(in)            :: mjdsec_in
      double precision, intent(in)   :: fsec_in
      double precision, intent(in)   :: lon_in
      double precision, intent(in)   :: lat_in
!     double precision   :: delta_lat
!     double precision   :: delta_lon
!     double precision   :: delta_lat_inv
!     double precision   :: delta_lon_inv

      double precision   :: lon_mod

      double precision  :: t
      double precision  :: u
      double precision  :: t1
      double precision  :: u1


      double precision  :: ah
      double precision  :: aw
      double precision  :: zdh
      double precision  :: zdw

      double precision  :: zdh_mm
      double precision  :: zdh_mp
      double precision  :: zdh_pp
      double precision  :: zdh_pm

      double precision  :: zdw_mm
      double precision  :: zdw_mp
      double precision  :: zdw_pp
      double precision  :: zdw_pm

      double precision  :: ah_interp
      double precision  :: aw_interp
      double precision  :: zdh_interp
      double precision  :: zdw_interp

      INTEGER :: iymd0
      INTEGER :: ihms0

      INTEGER :: iymd_in
      INTEGER :: ihms_in

      double precision  :: date0
      double precision  :: date_rec
      double precision  :: date_rec_input

      double precision  :: lon_minus
      double precision  :: lon_plus
      double precision  :: lat_minus
      double precision  :: lat_plus
!     double precision  :: idelta_lat

      INTEGER(kind=4) :: element_num_mm
      INTEGER(kind=4) :: element_num_mp
      INTEGER(kind=4) :: element_num_pm
      INTEGER(kind=4) :: element_num_pp

      double precision  :: date_plus_hour
      INTEGER ::  record_in

!double precision  :: time_in

! the compiler requires that "len" be a 4-byte integer
! since it is used in the "INQUIRE" statement as the output of
! "iolength"
      INTEGER(kind=4) ::  len
!!    INTEGER ::  len

!! in vmf module
!!type( vmf_def ), dimension( MAX_VMF ) :: vmf_array

      type( vmf_def ), dimension( MAX_VMF ) :: vmf_array_input

      INTEGER ::  istat

      LOGICAL ::  Lopen
      LOGICAL ::  L_exist
      LOGICAL ::  L_rec_prev_ok
      LOGICAL ::  L_rec_ok

      character(20) :: input_filename = 'vmf_interface_file'



!------------------------------------------------------------------------------


! get run start and stop times in mjdsec


      ah_interp    = 0.0D0
      aw_interp    = 0.0D0
      zdh_interp   = 0.0D0
      zdw_interp   = 0.0D0



!write(6,'(/A,1x,I12,1x,F10.6)') &
!      'rvgf: mjdsec_in, fsec_in  ', mjdsec_in, fsec_in

      call MJDYMD(mjdsec_in,IYMD_in,IHMS_in, 4 )

!      write(6,'(A,1x,I8.8,1x,I6.6)') &
!      'rvgf: requested date  IYMD_in, IHMS_in   ', &
!                             IYMD_in, IHMS_in


      if( Lfirst ) then


      date_plus_hour = 0.0D0

      inquire( iolength = len ) date_plus_hour, vmf_array

!      write(6,'(/A,1x,I12)') &
!     &'rvgf: open VMF grid file   len = ', len

      Lopen = .FALSE.
      inquire( file = trim(input_filename), &
     &       exist = L_exist )

      inquire( unit = VMF_output_UnitNum, &
     &       opened = Lopen, exist = L_exist )

!      write(6,'(A,A,A,5x,L1)') &
!     &'rvgf: file ', trim(input_filename), ' : L_exist = ', L_exist

!      write(6,'(A,5x,L1)') &
!     &'rvgf: unit VMF_output_UnitNum :   Lopen   = ', Lopen

      if( .NOT. L_exist )then

        write(6,'(//A//)') 'rvgf: VMF grid file does not exist '
        stop 'rvgf:no VMF grid file'

      endif

!      write(6,'(/A,1x,I12)') &
!     &'rvgf: open VMF grid file   len = ', len

      if( .not. Lopen ) then

        open( VMF_output_UnitNum, &
     &        file = trim(input_filename), &
     &        form = 'unformatted', &
     &        status = 'old'  , &
     &        recl = len,           &
     &        access = 'direct', iostat = istat  )

        if( istat /= 0 )then
            write(6,'(A,1x,I10)') &
     &            'rvgf: bad open of vmf big file istat = ', istat
            stop  'rvgf: bad open'
        endif

      endif ! .not. Lopen

      Lfirst = .false.


! read the first record to find out where this file starts

      read( VMF_output_UnitNum, rec = 1 ) date0

      iymd0 = NINT(      date0 / 100.0D0 )
      ihms0 = NINT( MOD( date0,  100.0D0 ) * 10000.0D0 )


!      write(6,'(A,1x,F15.2)')  'rvgf: first date on file date0 ', date0
!      write(6,'(A,1x,I8.8,1x,I6.6)') &
!     &    'rvgf: start date  IYMD0, IHMS0   ', &
!     &                       IYMD0, IHMS0


      call YMDTIS( IYMD0, IHMS0, mjdsec0       )

      xmjdsec0 =  DBLE( mjdsec0)

!      write(6,'(/A,1x,I12/)') &
!     &        'rvgf: first date mjdsec0   ', mjdsec0

      record_prev = -1

      L_rec_prev_ok  = .TRUE.
      L_rec_ok       = .TRUE.

      endif




!-------------------------------------------------------------

! compute record for the input date and time


      record_in = 1 + &
     &     INT(  ( DBLE(mjdsec_in - mjdsec0) + fsec_in ) / &
     &                                            xsix_hours_sec  )

!write(6,'(A,1x,I12)')   'rvgf: mjdsec0   ', mjdsec0
!write(6,'(A,1x,I12)')   'rvgf: mjdsec_in ', mjdsec_in
!write(6,'(A,1x,I12)')   'rvgf: mjdsec_in - mjdsec0 ', &
!                               mjdsec_in - mjdsec0
!write(6,'(A,1x,I4)')    'rvgf: record_in ', record_in

!-------------------------------------------------------------

! read record containing the current input time
! skip reading if this record is the same as the last one read

      if( record_in /= record_prev )then
!SMCS
      read( 320, rec = record_in, iostat=istat ) &
     &    date_rec_input, vmf_array_input


!      read( VMF_output_UnitNum, rec = record_in, iostat=istat ) &
!     &    date_rec_input, vmf_array_input

      if(  istat /= 0 )then

         write(6,'(A,1x,I12)') &
     &         'rvgf: bad read istat     = ', istat
         write(6,'(A,1x,I12)') &
     &         'rvgf: bad read mjdsec_in = ', mjdsec_in
         write(6,'(A,1x,I12)') &
     &         'rvgf: bad read record_in = ', record_in
         return

      endif  ! istat

!write(6,'(A,1x,F15.2)')  'rvgf: date_rec  ', date_rec

      record_prev = record_in

!      write(6,'(A,1x,2(1x,I12))') &
!      'rvgf: read file mjdsec_in, record_in ', &
!                       mjdsec_in, record_in

      if( ABS( date_rec_input ) > 0.0D0 )then

        date_rec  = date_rec_input
        vmf_array = vmf_array_input

        L_rec_ok = .TRUE.

      else

! Put some code here to handle the case where
! the date_rec is zero and the vmf_array has all zeroes
! This means that there was a missing
! 6-hour vmf file for this time.

! Maybe read the preceding and
! following records and average them

        if( L_rec_prev_ok ) then

            read( VMF_output_UnitNum, &
     &             rec = record_in - 1 , iostat=istat ) &
     &             date_rec_input, vmf_array_input

             date_rec  = date_rec_input
             vmf_array = vmf_array_input

        else

            write(6,'(/A)') &
     &            'rvgf: two consecutive zero records read'
            write(6,'(A/)') &
     &            'rvgf: stopping program in read_vmf1_grid_file'
            stop 'bad vmf file'

        endif     !   L_rec_prev_ok

        L_rec_ok = .FALSE.

       endif !   abs( date_rec_input ) > 0.0d0

       L_rec_prev_ok = L_rec_ok

       endif !   record_in /= record_prev

!-------------------------------------------------------------

! get 4 bounding points for lon, lat

! ( lon_minus, lat_minus )
! ( lon_minus, lat_plus  )
! ( lon_plus , lat_minus )
! ( lon_plus , lat_plus  )

       lon_mod = MOD( lon_in + 360.0D0 , 360.0D0 )

!      write(6,'(/A,2(1x,F8.3 ))')  &
!      'rvgf: lon_in, lon_mod ', lon_in, lon_mod

      lat_minus = lat_in
      lat_plus  = lat_in  + delta_lat
      lon_minus = lon_mod
      lon_plus  = lon_mod + delta_lon

!      lat_minus = dfloat( int( lat_minus * delta_lat_inv ) * &
!     & idelta_lat)
!      lat_plus  = dfloat( int( lat_plus  * delta_lat_inv ) * &
!     & idelta_lat)

      lon_minus = DBLE( INT( lon_minus * delta_lon_inv ))&
     & * delta_lon
      lon_plus  = DBLE( INT( lon_plus  * delta_lon_inv ))&
     & * delta_lon

      if (lat_in >= 0.D0) then
!SMCS removed the (+2) at the end of the equation
          lat_minus = DBLE(( INT( lat_minus * delta_lat_inv ) * &
     &                idelta_lat))

          lat_plus  = DBLE(( INT( lat_plus  * delta_lat_inv ) * &
     &                idelta_lat))

!               write(6,*) "rvgf: lat_minus, lat_plus:", &
!                           lat_minus, lat_plus

      elseif (lat_in < 0.D0) then

          lat_minus = DBLE( (INT( lat_minus * delta_lat_inv ) * &
     &                idelta_lat)-2)

          lat_plus  = DBLE(( INT( lat_plus  * delta_lat_inv ) * &
     &                idelta_lat)-2)

!               write(6,*) "rvgf: lat_minus, lat_plus:", &
!                           lat_minus, lat_plus

       else

          lat_minus = DBLE( INT( lat_minus * delta_lat_inv ) * &
     &                 idelta_lat)
          lat_plus  = DBLE( INT( lat_plus  * delta_lat_inv ) * &
     &                idelta_lat)



       endif




!-----------------------------------------------------

! make sure the numbers are in the proper ranges

      lat_plus  = MIN(  90.0D0, lat_plus  )

      lon_minus = MOD( lon_minus + 360.0D0 , 360.0D0 )
      lon_plus  = MOD( lon_plus  + 360.0D0 , 360.0D0 )

!-----------------------------------------------------

! compute element in vmf_array using lon and lat


      call comp_element_num( lon_minus, lat_minus, element_num_mm )
      call comp_element_num( lon_minus, lat_plus,  element_num_mp )
      call comp_element_num( lon_plus , lat_minus, element_num_pm )
      call comp_element_num( lon_plus , lat_plus,  element_num_pp )


!write(6,'(/A,2(1x,F8.3 ))')  &
!      'rvgf: lon_mod, lat_in              ', &
!             lon_mod, lat_in

!write(6,'(A,2(1x,F8.3),1x,I6)')  &
!      'rvgf: lon_minus, lat_minus, num_mm ', &
!             lon_minus, lat_minus, element_num_mm
!write(6,'(A,2(1x,F8.3 ),1x,I6)')  &
!      'rvgf: lon_minus, lat_plus,  num_mp ', &
!             lon_minus, lat_plus,  element_num_mp
!write(6,'(A,2(1x,F8.3 ),1x,I6)')  &
!      'rvgf: lon_plus , lat_minus, num_pm ', &
!             lon_plus , lat_minus, element_num_pm
!write(6,'(A,2(1x,F8.3 ),1x,I6/)')  &
!      'rvgf: lon_plus , lat_plus,  num_pp ', &
!             lon_plus , lat_plus,  element_num_pp


!----------------------------------------------------------------

! get the VMF quantities for bi-linear interpolation


!call my_int(  lon_minus, lat_minus, &
!              lon_plus , lat_plus,  &
!              y1, y2, y3, y4,       &
!              lon, lat, t, u, t1, u1, &
!              calc_u_t, y_interp )

      call vmf_interpolation(  lon_minus, lat_minus,           &
     &        lon_plus , lat_plus,            &
     &        vmf_array(element_num_mm)%ah,   &
     &        vmf_array(element_num_mp)%ah,   &
     &        vmf_array(element_num_pp)%ah,   &
     &        vmf_array(element_num_pm)%ah,   &
     &        lon_mod, lat_in,  t, u, t1, u1, &
     &        .true., ah_interp )

!write(6,'(/A/2(1x,F7.3),5(1x,E12.5))') &
!  'rvgf: lon_mod, lat_in, ah_mm, ah_mp, ah_pp, ah_pm, ah_interp ', &
!         lon_mod, lat_in, &
!         vmf_array(element_num_mm)%ah,   &
!         vmf_array(element_num_mp)%ah,   &
!         vmf_array(element_num_pp)%ah,   &
!         vmf_array(element_num_pm)%ah,   &
!         ah_interp

!-----------------------------------------------------------------------------

      call vmf_interpolation(  lon_minus, lat_minus,           &
     &        lon_plus , lat_plus,            &
     &        vmf_array(element_num_mm)%aw,   &
     &        vmf_array(element_num_mp)%aw,   &
     &        vmf_array(element_num_pp)%aw,   &
     &        vmf_array(element_num_pm)%aw,   &
     &        lon_mod, lat_in, t, u, t1, u1,  &
     &        .false.,  aw_interp )


!write(6,'(/A/2(1x,F7.3),5(1x,E12.5))') &
!  'rvgf: lon_mod, lat_in, aw_mm, aw_mp, aw_pp, aw_pm, aw_interp ', &
!         lon_mod, lat_in, &
!         vmf_array(element_num_mm)%aw,   &
!         vmf_array(element_num_mp)%aw,   &
!         vmf_array(element_num_pp)%aw,   &
!         vmf_array(element_num_pm)%aw,   &
!         aw_interp

!-----------------------------------------------------------------------------

      call vmf_interpolation(  lon_minus, lat_minus,           &
     &        lon_plus , lat_plus,            &
     &        vmf_array(element_num_mm)%zdh,  &
     &        vmf_array(element_num_mp)%zdh,  &
     &        vmf_array(element_num_pp)%zdh,  &
     &        vmf_array(element_num_pm)%zdh,  &
     &        lon_mod, lat_in, t, u, t1, u1,  &
     &        .false.,  zdh_interp )


       zdh_mm = vmf_array(element_num_mm)%zdh
       zdh_mp = vmf_array(element_num_mp)%zdh
       zdh_pp = vmf_array(element_num_pp)%zdh
       zdh_pm = vmf_array(element_num_pm)%zdh

!      write(6,'(/A/2(1x,F7.3),5(1x,E12.5))') &
!     & 'rvgf: lon_mod, lat_in, zdh_mm, zdh_mp, zdh_pp, zdh_pm,&
!     &  zdh_interp ', &
!     &    lon_mod, lat_in, zdh_mm, zdh_mp, zdh_pp, zdh_pm, zdh_interp

!-----------------------------------------------------------------------------

      call vmf_interpolation(  lon_minus, lat_minus,           &
     &        lon_plus , lat_plus,            &
     &        vmf_array(element_num_mm)%zdw,  &
     &        vmf_array(element_num_mp)%zdw,  &
     &        vmf_array(element_num_pp)%zdw,  &
     &        vmf_array(element_num_pm)%zdw,  &
     &        lon_mod, lat_in, t, u, t1, u1,  &
     &        .false.,  zdw_interp )


       zdw_mm = vmf_array(element_num_mm)%zdw
       zdw_mp = vmf_array(element_num_mp)%zdw
       zdw_pp = vmf_array(element_num_pp)%zdw
       zdw_pm = vmf_array(element_num_pm)%zdw


!      write(6,'(/A/2(1x,F7.3),5(1x,E12.5))') &
!     & 'rvgf: lon_mod, lat_in, zdw_mm, zdw_mp, zdw_pp, zdw_pm,&
!     &  zdw_interp ', &
!     &    lon_mod, lat_in, zdw_mm, zdw_mp, zdw_pp, zdw_pm, zdw_interp

!-----------------------------------------------------------------------------

!      write(6,'(/A/2(1x,F6.2),2(1x,F10.8),2(1x,F7.4)/)') &
!     &'rvgf: lon_in, lat_in, ah_interp, aw_interp, zdh_interp, &
!     & zdw_interp ', &
!     &   lon_in, lat_in, ah_interp, aw_interp, zdh_interp, zdw_interp


       ah = ah_interp
       aw = aw_interp
       zdh = zdh_interp
       zdw = zdw_interp




      return


      end subroutine read_vmf1_grid_file
