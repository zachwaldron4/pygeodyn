      module DEMtypes_m
      use DEMglobals_m
      implicit none

      type :: demmap_t
          character(len=80) :: gridname
          INTEGER :: nx, ny
          INTEGER :: itrans
          DOUBLE PRECISION :: offx, modx
          DOUBLE PRECISION :: offy, mody
          DOUBLE PRECISION :: offz, sclz
          DOUBLE PRECISION :: rottrans(3,3)
          INTEGER :: iproj, nproj
          DOUBLE PRECISION, allocatable :: projparm(:)
          DOUBLE PRECISION :: eps_xy, eps_z
          INTEGER :: ntest
          DOUBLE PRECISION, allocatable :: testxyz(:,:)
          DOUBLE PRECISION :: val_nan, thr_nan
          DOUBLE PRECISION :: xmin, xmax, ymin, ymax
          DOUBLE PRECISION, allocatable :: vecx(:), vecy(:), grid(:,:)
          DOUBLE PRECISION :: step_x, step_y
      end type

      end module
