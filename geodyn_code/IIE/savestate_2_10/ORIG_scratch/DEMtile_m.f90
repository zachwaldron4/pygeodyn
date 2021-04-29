      module DEMtile_m

      use DEMglobals_m, only : OFFSET_K

      implicit none


      INTEGER, parameter :: MAX_CACHED_TILES = 10000


      type :: DEMtile_t
          INTEGER(OFFSET_K) :: file_offset
          INTEGER :: nx
          INTEGER :: ny
          character(len=4096) :: filename
          double precision, allocatable :: grid(:,:)
      end type


      type, private :: CacheEntry_t
          LOGICAL :: is_filled
          INTEGER :: counter_at_insertion
          INTEGER :: bytes_used
          type(DEMtile_t), pointer :: tile_p => null()
      end type


      type, private :: DEMcache_t
          INTEGER :: memory_bytes_used
          INTEGER :: memory_bytes_max
          INTEGER :: max_tile_index
          INTEGER :: insertion_counter
          type(CacheEntry_t), allocatable :: cache(:)
      end type


      type(DEMcache_t) :: GLOBAL_DEMTILE_CACHE


      contains


      subroutine DEMtile_init(DEMtile, filename, file_offset, nx, ny)
          type(DEMtile_t), intent(out) :: DEMtile
          character(len=*), intent(in) :: filename
          INTEGER, intent(in) :: file_offset, nx, ny

          DEMtile%filename = filename
          DEMtile%file_offset = file_offset
          DEMtile%nx = nx
          DEMtile%ny = ny
      end subroutine


      subroutine DEMtile_load(DEMtile)
          type(DEMtile_t), intent(inout) :: DEMtile

          INTEGER :: iunit

!!!!!     open(newunit=iunit, file=DEMtile%filename, status='old', &
          iunit=57
          open(unit=iunit, file=DEMtile%filename, status='old', &
                  form='unformatted', access='stream', action='read')

          allocate(DEMtile%grid(DEMtile%ny, DEMtile%nx))
          read(iunit, pos=DEMtile%file_offset) DEMtile%grid
          close(iunit)
      end subroutine


      subroutine DEMtile_free(DEMtile)
          type(DEMtile_t), intent(inout) :: DEMtile

          deallocate(DEMtile%grid)
      end subroutine


      function DEMtile_getvalue(DEMtile, ix, iy)
          double precision :: DEMtile_getvalue
          type(DEMtile_t), intent(inout) :: DEMtile
          INTEGER, intent(in) :: ix, iy

          if (.not. allocated(DEMtile%grid)) then
              call DEMcache_addtile(GLOBAL_DEMTILE_CACHE, DEMtile)
              call DEMtile_load(DEMtile)
          end if

          DEMtile_getvalue = DEMtile%grid(iy, ix)
      end function


      subroutine DEMcache_init(DEMcache, memory_bytes_max)
          type(DEMcache_t), intent(out) :: DEMcache
          INTEGER, intent(in) :: memory_bytes_max

          INTEGER :: i

          allocate(DEMcache%cache(MAX_CACHED_TILES))
          do i = 1, MAX_CACHED_TILES
              DEMcache%cache(i)%is_filled = .false.
              DEMcache%cache(i)%counter_at_insertion = -1
              DEMcache%cache(i)%bytes_used = 0
              nullify(DEMcache%cache(i)%tile_p)
          end do

          DEMcache%memory_bytes_max = memory_bytes_max
          DEMcache%memory_bytes_used = 0
          DEMcache%max_tile_index = 0
          DEMcache%insertion_counter = 0
      end subroutine


      subroutine DEMcache_addtile(DEMcache, tile)
          type(DEMcache_t), intent(inout) :: DEMcache
          type(DEMtile_t), target, intent(inout) :: tile

          INTEGER :: i, idx, newtile_bytes

          newtile_bytes = tile%nx * tile%ny * 8

          ! Free up memory until the new tile can fit
          do while (DEMcache%memory_bytes_used + newtile_bytes &
                      > DEMcache%memory_bytes_max)
              idx = DEMcache_get_next_tile_to_remove(DEMcache)
              if (idx <= 0) then
                  write(*,'(3A)') 'error: requires more memory for ', &
                                  'DEM tiles, but unable to find ', &
                                  'any tiles to remove.'
                  stop 1
              end if
              call DEMcache_remove(DEMcache, idx)
          end do

          ! Add the new tile to the cache in the first free slot
          DEMcache%insertion_counter = DEMcache%insertion_counter + 1
          do i = 1, DEMcache%max_tile_index+1
              if (i > size(DEMcache%cache)) then
                  write(*,'(2A)') 'error: need to increase ', &
                                  'MAX_CACHED_TILES'
                  stop 1
              end if

              if (.not. DEMcache%cache(i)%is_filled) then
                  DEMcache%cache(i)%is_filled = .true.
                  DEMcache%cache(i)%counter_at_insertion &
                          = DEMcache%insertion_counter
                  DEMcache%cache(i)%bytes_used = newtile_bytes
                  DEMcache%cache(i)%tile_p => tile
                  exit
              end if
          end do
      end subroutine


      ! Return the cache index of the tile that should be removed next
      ! when memory is needed.
      function DEMcache_get_next_tile_to_remove(DEMcache)
          type(DEMcache_t), intent(in) :: DEMcache
          INTEGER :: DEMcache_get_next_tile_to_remove

          INTEGER :: i
          INTEGER :: oldest_tile_index
          INTEGER :: oldest_tile_insertion_counter

          ! Find the oldest tile
          oldest_tile_index = -1
          oldest_tile_insertion_counter &
                  = huge(oldest_tile_insertion_counter)
          do i = 1, DEMcache%max_tile_index
              if (DEMcache%cache(i)%is_filled) then
                  if (DEMcache%cache(i)%counter_at_insertion &
                          < oldest_tile_insertion_counter) then
                      oldest_tile_index = i
                      oldest_tile_insertion_counter &
                            = DEMcache%cache(i)%counter_at_insertion
                  end if
              end if
          end do

          DEMcache_get_next_tile_to_remove = oldest_tile_index
      end function


      ! Deallocate the tile specified by cache_index.
      subroutine DEMcache_remove(DEMcache, cache_index)
          type(DEMcache_t), intent(inout) :: DEMcache
          INTEGER, intent(in) :: cache_index

          INTEGER :: i, max_index

          DEMcache%cache(cache_index)%is_filled = .false.
          DEMcache%memory_bytes_used = DEMcache%memory_bytes_used &
                  - DEMcache%cache(cache_index)%bytes_used
          call DEMtile_free(DEMcache%cache(cache_index)%tile_p)

          ! Update max_tile_index
          max_index = DEMcache%max_tile_index
          do i = DEMcache%max_tile_index, 1, -1
              if (DEMcache%cache(i)%is_filled) then
                  max_index = i
                  exit
              end if
          end do
          DEMcache%max_tile_index = max_index
      end subroutine


      end module
