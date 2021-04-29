


      subroutine get_index( itarget, iarray, narray, &
                            index_target, L_in_array )

      implicit none

      ! Subroutine get_index looks for the value "itarget"
      ! in the array "iarray" which has dimension "narray"

      ! If itarget is found in iarray,
      ! get_index returns the index in iarray as "index_target"

      ! Also, if itarget is found in iarray,
      ! get_index returns L_in_array = true

      ! Otherwise, get_index returns
      !                      index_target = 0
      !                  and L_in_array = .false.



      INTEGER,intent(in) :: itarget
      INTEGER :: index_target
      INTEGER :: i
      INTEGER,intent(in) :: narray
      INTEGER,dimension(2, narray),intent(in) :: iarray

      LOGICAL :: L_in_array



      !------------------------------------------------------------------

      index_target = 0
      L_in_array   = .FALSE.

      if( narray <= 0 )return


      index_target = 0

      L_in_array   = any( itarget == iarray )

      !write(6,*) 'geti:  itarget, narray ', itarget , narray
      !write(6,'(A/(5(1x,I10)))') 'geti:  iarray(1,:) ', iarray(1,:)
      !write(6,*) 'geti:  L_in_array ', L_in_array

      if( .not. L_in_array ) return


      do  i = 1, narray

          !write(6,'(A,3(1x,I10))') 'geti: i, iarray(1,i), itarget ', &
          !                                i, iarray(1,i), itarget

          if( itarget == iarray(1,i) ) then
              index_target = i
              exit
          endif !  itarget == iarray(1, i)


      enddo ! i

      if( index_target == 0 ) L_in_array = .FALSE.

      return

      end subroutine get_index
