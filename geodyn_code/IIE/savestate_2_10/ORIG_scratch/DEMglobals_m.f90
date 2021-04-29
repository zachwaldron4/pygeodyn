      module DEMglobals_m
      implicit none

      ! Kind for storing file offsets
      INTEGER, parameter :: OFFSET_K = selected_int_kind(18)

      ! Maximum number of DEM_GRIDXX files
      INTEGER, parameter :: nbgrdmax = 10

      ! Maximum number of defined projections
      INTEGER, parameter :: iprojmax = 4

      character(len=23), parameter :: projname(iprojmax) = [ &
                                          'cylindrical            ' , &
                                          'stereographic          ' , &
                                          'gnomonic               ' , &
                                          'lambert conformal conic' ]
      INTEGER, parameter :: nprojreq(iprojmax) = [0 , 3 , 3 , 9]

      DOUBLE PRECISION, parameter :: pi = ACOS(-1.0D0)

      INTEGER, parameter :: PROJTYPE_CYLINDRICAL = 1
      INTEGER, parameter :: PROJTYPE_STEREOGRAPHIC = 2
      INTEGER, parameter :: PROJTYPE_GNOMONIC = 3
      INTEGER, parameter :: PROJTYPE_LAMBERT_CONFORMAL_CONIC = 4

      INTEGER, parameter :: COORDTRANSFORM_NONE = 0
      INTEGER, parameter :: COORDTRANSFORM_OFFSET = 1
      INTEGER, parameter :: COORDTRANSFORM_ROTATION_MATRIX = 2

      double precision, parameter :: INTERP_NAN = 999999999.0D0

      INTEGER, parameter :: debug_level = 0

      end module
