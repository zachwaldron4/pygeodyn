      module vmf_module


      IMPLICIT NONE

      LOGICAL ::  L_vmf

      INTEGER(kind=4), PARAMETER :: VMF_input_UnitNum    = 310
      INTEGER(kind=4), PARAMETER :: VMF_output_UnitNum   = 320
      INTEGER(kind=4), PARAMETER :: data_input_UnitNum   = 330


      INTEGER, parameter  :: line_length = 100

      TYPE :: vmf_def

      REAL lat,lon
      DOUBLE PRECISION ah,aw,zdh,zdw


      END TYPE

      INTEGER,parameter :: MAX_VMF = 13104

      type( vmf_def ), dimension( MAX_VMF ) :: vmf_array


      INTEGER,parameter ::            six_hours_sec = 21600
      double precision, parameter :: xsix_hours_sec = 21600.0D0


      INTEGER          ::   mjdsec0
      double precision ::  xmjdsec0

      LOGICAL :: Lfirst = .TRUE.

      INTEGER  :: record_prev

      double precision,parameter :: idelta_lat = 2
      double precision,parameter ::  delta_lat = 2.0D0
      double precision,parameter ::  delta_lon = 2.5D0

      double precision,parameter ::  delta_lat_inv = 0.5D0
      double precision,parameter ::  delta_lon_inv = 0.4D0
      integer*4 num_lon

      PARAMETER (NUM_LON = 144 )

!     integer(kind=4),parameter :: num_lon = 144_4



      end module vmf_module
