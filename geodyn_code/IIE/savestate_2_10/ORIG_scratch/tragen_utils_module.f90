      module tragen_utils_module
      implicit none

      private
      public :: gpsatt_store_add, gpsatt_store_get

      type :: t_gpsatt_store
          INTEGER :: isatid
          INTEGER :: mjds
          double precision :: fsec
          double precision :: betap
          double precision :: yawang
          double precision :: somega
      end type

      type(t_gpsatt_store), allocatable :: gpsatt_store(:)
      INTEGER :: gpsatt_store_count
      INTEGER :: gpsatt_store_start_index

      INTEGER, parameter :: rad2deg = 180.0 / acos(-1.0d0)

      contains

      subroutine gpsatt_store_add(isatid, mjds, fsec, &
                  betap, yawang, somega)
          INTEGER, intent(in) :: isatid, mjds
          double precision, intent(in) :: fsec, betap, yawang, somega

          type(t_gpsatt_store), allocatable :: tmpgs(:)

          if (.not. allocated(gpsatt_store)) then
              ! initialize
              gpsatt_store_count = 0
              allocate(gpsatt_store(100))
              gpsatt_store_start_index = 1
          end if

          if (gpsatt_store_count >= size(gpsatt_store)) then
              ! reallocate
              allocate(tmpgs(2*size(gpsatt_store)))
              tmpgs(1:size(gpsatt_store)) = gpsatt_store(:)
              deallocate(gpsatt_store)
              call move_alloc(tmpgs, gpsatt_store)
          end if

          gpsatt_store_count = gpsatt_store_count + 1
          gpsatt_store(gpsatt_store_count)%isatid = isatid
          gpsatt_store(gpsatt_store_count)%mjds = mjds
          gpsatt_store(gpsatt_store_count)%fsec = fsec
          gpsatt_store(gpsatt_store_count)%betap = betap
          gpsatt_store(gpsatt_store_count)%yawang = yawang
          gpsatt_store(gpsatt_store_count)%somega = somega
      end subroutine

      subroutine gpsatt_store_get(isatid, mjds, fsec, &
                  betap, yawang, somega, is_found)
          INTEGER, intent(in) :: isatid, mjds
          double precision, intent(in) :: fsec
          double precision, intent(out) :: betap, yawang, somega
          LOGICAL, intent(out) :: is_found

          INTEGER :: i
          double precision :: difftime
          LOGICAL :: first_iter

          is_found = .false.

          if (.not. allocated(gpsatt_store)) then
              !write(*,'(2A)') &
              !       'WARNING: gpsatt_store_get() called before', &
              !       ' any values were stored.'
              return
          end if

          first_iter = .true.
          i = gpsatt_store_start_index
          do while (i /= gpsatt_store_start_index .or. first_iter)
              difftime = (mjds - gpsatt_store(i)%mjds) &
                          + (fsec - gpsatt_store(i)%fsec)
              if (isatid == gpsatt_store(i)%isatid &
                          .and. ABS(difftime) < 1.0D-6) then
                  betap = gpsatt_store(i)%betap * rad2deg
                  yawang = gpsatt_store(i)%yawang ! already in degrees
                  somega = gpsatt_store(i)%somega * rad2deg
                  gpsatt_store_start_index = i
                  is_found = .true.
                  return
              end if
              i = i + 1
              if (i > size(gpsatt_store)) then
                  i = 1
              end if
              first_iter = .false.
          end do

          !write(*,'(A)') &
          !           'WARNING: gpsatt_store_get() failed to find match.'
      end subroutine

      end module
