      subroutine vmf_interpolation(  lon_minus, lat_minus, &
     &              lon_plus , lat_plus,  &
     &              y1, y2, y3, y4,       &
     &              lon, lat, &
     &              t, u, t1, u1, &
     &              L_compute, y_interp )


       use  vmf_module


! (  lon_minus, lat_minus, y1 )
! (  lon_minus, lat_plus,  y2 )
! (  lon_plus , lat_plus,  y3 )
! (  lon_plus , lat_minus, y4 )

      IMPLICIT NONE


      double precision, intent(in) :: lon_minus
      double precision, intent(in) :: lon_plus
      double precision, intent(in) :: lat_minus
      double precision, intent(in) :: lat_plus

      double precision, intent(in) :: y1
      double precision, intent(in) :: y2
      double precision, intent(in) :: y3
      double precision, intent(in) :: y4

      double precision,intent(in)  :: lon
      double precision,intent(in)  :: lat

      LOGICAL, intent(in) :: L_compute

!double precision :: y_mean
!double precision :: y_sq_sum
!double precision :: y_std
!double precision :: y_min
!double precision :: y_max

      double precision :: y_interp

      double precision :: t
      double precision :: u

      double precision :: t1
      double precision :: u1

      double precision, parameter :: eps = 1.0D-3



!-------------------------------------------------------------------------------


      if( L_compute ) then

!      write(6,'(//A,2(1x,F8.3), 2x, E15.7)') &
!      'myi:  lon_minus, lat_minus, y1 ', lon_minus, lat_minus, y1
!      write(6,'(A,2(1x,F8.3), 2x, E15.7)') &
!      'myi:  lon_minus, lat_plus,  y2 ', lon_minus, lat_plus,  y2
!      write(6,'(A,2(1x,F8.3), 2x, E15.7)') &
!      'myi:  lon_plus , lat_plus,  y3 ', lon_plus , lat_plus,  y3
!      write(6,'(A,2(1x,F8.3), 2x, E15.7)') &
!      'myi:  lon_plus , lat_minus, y4 ', lon_plus , lat_minus, y4
!      write(6,'(/A,2(1x,F8.3), 2x, E15.7)') 'myi: lon, lat ', lon, lat


!write(6,'(/A,2(1x,E15.7))') &
!      'myi:  delta_lon, delta_lat', delta_lon, delta_lat


      t = MOD( lon - lon_minus + 360.0D0 , 360.0D0 )  * delta_lon_inv
      u =    ( lat - lat_minus ) * delta_lat_inv

!write(6,'(/A,2(1x,E15.7))') 'myi:  t, u  ', t, u


! test if t or u out of range [0,1]
! fix t and  u to be in range [0,1]

      if( t < 0.0D0 - eps  .or. t > 1.0D0 + eps )then

!        write(6,'(A,1x, E15.7)') 'myi: t out of bounds  t = ', t

        t = MIN( 1.0D0 , t )
        t = MAX( 0.0D0 , t )

      endif

      if( u < 0.0D0 - eps  .or. u > 1.0D0 + eps )then

!        write(6,'(A,1x, E15.7)') 'myi: u out of bounds  u = ', u

        u = MIN( 1.0D0 , u )
        u = MAX( 0.0D0 , u )

      endif

!write(6,'(/A,2(1x,E15.7))') &
!      'myi:aft bounds check  t, u  ', t, u

      t1 = 1.0D0 - t
      u1 = 1.0D0 - u


      endif  ! L_compute


!y_interp =  y1 * ( 1.0d0 - t ) * ( 1.0d0 - u )  +  &
!            y2 * ( 1.0d0 - t ) *           u    +  &
!            y3 *           t   *           u    +  &
!            y4 *           t   * ( 1.0d0 - u )


      y_interp =  y1 * t1 * u1   +  &
     &      y2 * t1 * u    +  &
     &      y3 * t  * u    +  &
     &      y4 * t  * u1


!write(6,'(/A,4(1x,E15.7))') 'myi: y1, y2, y3, y4 ', &
!                                  y1, y2, y3, y4


!      write(6,'(/A,4(1x,E15.7))') 'myi: y1, y2, y3, y4 ', &
!     &                            y1, y2, y3, y4
!
!      write(6,'(A,1x,E15.7)')     'myi: y_interp       ', &
!     &                            y_interp

!y_min  = min( y1 , y2 , y3 , y4 )
!y_max  = max( y1 , y2 , y3 , y4 )

!y_mean   = ( y1    + y2    + y3    + y4    ) / 4.0d0
!y_sq_sum = ( y1**2 + y2**2 + y3**2 + y4**2 )

!y_std  = sqrt(  y_sq_sum / 4.0d0 - y_mean**2 )



!write(6,'(/A,4(1x,E15.7))') &
!      'myi: y_min, y_interp, y_max        ', &
!            y_min, y_interp, y_max


!write(6,'(A,4(1x,E15.7))') &
!      'myi: mean-std, y_interp, mean+std  ', &
!            y_mean-y_std, y_interp, y_mean+y_std



      return


      end subroutine vmf_interpolation
