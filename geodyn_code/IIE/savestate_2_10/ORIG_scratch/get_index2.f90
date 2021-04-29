      subroutine get_index2( itarget1, itarget2, iarray, narray, &
                            index_target, L_in_array )

      implicit none

      ! Subroutine get_index2 looks for the pair of values
      !  "itarget1"  and "itarget2"
      ! in the array "iarray" which has dimension "(2,narray)"

      ! since the default antenna number is 1,
      ! if itarget2, the station or satellite antenna number,
      ! is < 1, it is set equal to 1.

      ! If the pair is found in iarray,
      ! get_index2 returns the index in iarray as "index_target"

      ! Also, if the pair is found in iarray,
      ! get_index2 returns L_in_array = true

      ! Otherwise, get_index2 returns
      !                      index_target = 0
      !                  and L_in_array = .false.



      INTEGER,intent(in) :: itarget1
      INTEGER,intent(in) :: itarget2
      INTEGER            :: itarget_test
      INTEGER :: index_target
      INTEGER :: i
      INTEGER,intent(in) :: narray
      INTEGER,dimension(2, narray),intent(in) :: iarray

      LOGICAL :: L_in_array



      !------------------------------------------------------------------

      !write(6,'(/A,1x,I10, 2(1x,I6))') &
      ! 'geti2:  itarget1, itarget2, narray ', &
      !          itarget1, itarget2,  narray

      !do  i = 1, narray
      !    write(6,'(A,1x,I6,1x,1x,I10,1x,I6)') &
      !    'geti2: i, iarray(1:2,i) ', &
      !            i, iarray(1:2,i)
      !enddo


      index_target = 0
      L_in_array   = .FALSE.

      if( narray <= 0 )return


      index_target = 0

      L_in_array   = any( itarget1 == iarray )

      !write(6,'(A,3x,L1)') 'geti2: L_in_array ', L_in_array

      if( .not. L_in_array ) return


      !  if the antenna number is zero,
      !  set it to 1 to match the default antenna

      itarget_test = itarget2
      if( itarget_test < 1 ) itarget_test = 1


      do  i = 1, narray

          !write(6,'(A,1x,I6,2(1x,I10,1x,I6))') &
          !'geti2: i, iarray(1,i), iarray(2,i), itarget1, itarget_test ',
          !        i, iarray(1,i), iarray(2,i), itarget1, itarget_test

          if( itarget1     == iarray(1,i) .and.      &
              itarget_test == iarray(2,i)       ) then
              index_target = i
              exit
          endif !  itarget1 == iarray(1, i) ...


      enddo ! i

      !write(6,'(A,1x,I6/)') 'geti2: index_target ', index_target

      if( index_target == 0 ) L_in_array = .FALSE.

      return

      end subroutine get_index2
