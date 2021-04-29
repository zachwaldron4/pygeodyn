      subroutine comp_element_num( lon_in, lat_in, element_num )

      use vmf_module

      implicit none

      double precision,intent(in)  :: lon_in
      double precision,intent(in)  :: lat_in

      INTEGER(kind=4) :: element_num

      INTEGER(kind=4) :: a
      INTEGER(kind=4) :: b

!----------------------------------------------------------------------------


! compute element in vmf_array using lon and lat



!write(6,'(/A,1x,2(1x,F8.3))') 'cen: lon_in, lat_in ', &
!                                    lon_in, lat_in

      a = INT(  ( 90.0D0 - lat_in ) * delta_lat_inv , kind=4 )
      b = INT(  lon_in * delta_lon_inv, kind=4   )

      element_num =  1_4 + a * num_lon  + b


!write(6,'(A,1x,3(1x,I6) )') 'cen: a, b, elem_num ', &
!                                  a, b, element_num


      return


      end subroutine comp_element_num
