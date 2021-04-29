      module DEMinterpolate

      use DEMglobals_m
      use DEMmap_m
      implicit none

      INTEGER nbgrd
      type(DEMmap_t), allocatable :: dems(:)

      contains

      subroutine load_DEMgrids()
          character(len=200) :: input_filename
          INTEGER :: iunit
          INTEGER :: ix, iy, i, itrans, iproj
          INTEGER :: idgrd
          LOGICAL :: L_exist
          DOUBLE PRECISION :: ztest

          ! determine how many DEM_GRIDXX files there are
          do idgrd=1,nbgrdmax
              write(input_filename,'(A,I2.2)') 'DEM_GRID',idgrd
              write(*,*) 'searching for ',trim(input_filename)
              inquire( file = trim(input_filename), exist = L_exist )
              if (.not.L_exist) exit
          end do
          if (.not.L_exist) idgrd=idgrd-1
          nbgrd=idgrd
          write(*,*) 'There appear to be ',nbgrd,' input DEMs.'
          allocate(dems(nbgrd))

          ! read input grids
          do idgrd=1,nbgrd
              write(*,*) ' - reading DEM # ',idgrd
              write(input_filename,'(A,I2.2)') 'DEM_GRID',idgrd

              call DEMmap_load(dems(idgrd), input_filename)

              write(*,*) 'Done reading grid # ',idgrd
          end do
      end subroutine


      function interpolate_DEMgrids(DLA,PHI,igrid) result(VALINT)
      ! igrid forces to use grid # igrid ; if 0, use all
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      !                                                                C
      !     PROGRAM CREATION BY...   J.B. NICHOLAS      JUN 17, 2015   C
      !                              E.   MAZARICO                     C
      !                                                                C
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DOUBLE PRECISION, intent(in) :: PHI
      DOUBLE PRECISION, intent(in) :: DLA
      INTEGER, optional, intent(in) :: igrid
      DOUBLE PRECISION :: VALINT

      INTEGER :: ig0, ig1, ig


      ! initialize output value
      VALINT = INTERP_NAN

      ! will go through all grids and stop at first non-NaN value found
      ig0=1
      ig1=nbgrd
      if (present(igrid)) then
          if (igrid >= 1 .and. igrid <= nbgrd) then
              ! forcing to use this grid
              ig0=igrid
              ig1=igrid
          end if
      end if

      ig=ig0
      do while ( (ig <= ig1) .and. (VALINT == INTERP_NAN) )
          VALINT = DEMmap_interpolate(dems(ig), DLA, PHI)
          ig=ig+1
      end do
      write(28,*) DLA, PHI, VALINT
      end function

      end module
