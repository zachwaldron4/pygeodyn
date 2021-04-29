!===================================================================
      SUBROUTINE UTCSRI( G,NDIM1,NDIM2,NX,NY,TITLE,LU,                  &
     &                   LATMIN,LATMAX,LONMIN,LONMAX,UNDEF )
!
!  Reads an ascii gridfile.
!  This format uses 80-byte card-image records in ascii.
!
!  Calling arguments:
!
!   G     - O - two-dimensional array to contain the gridded data.
!               The grid will be loaded in the following way:
!               G(i,j), with i going west to east and j south to north.
!               G(1,1) is thus the southwest corner of grid.
!               G(NX,NY) is the northeast corner.
!               User must make certain G is dimensioned to at least
!               the expected values of NX,NY.
!
!   NDIM1,2 - I - dimensions of G in calling program.
!               NDIM must not be less than the expected value of NX.
!
!   NX,NY - O - size of the grid G.
!
!   TITLE - O - 160-byte character-string title that describes data.
!
!   LU    - I - fortran unit to use for reading input data.
!
!   LATMIN,LATMAX,LONMIN,LONMAX - O - area limits for grid,
!               in decimal degrees.
!               Note: these are REAL variables.
!             The grid intervals are therefore:
!             DX = (LONMAX-LONMIN)/(NX-1)  and
!             DY = (LATMAX-LATMIN)/(NY-1).
!
!   UNDEF - O - value to denote a null or missing value in G.
!
!
!  Written by R. Ray      Aug. 1990
!  Modified 6/3/93 - added NDIM2 in calling arguments to test size of NY
!

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION   G(NDIM1,NDIM2)
      DOUBLE PRECISION        LATMIN,LATMAX,LONMIN,LONMAX
      CHARACTER   TITLE*160, FORMAT*80

      READ(LU,1) TITLE(1:80)
      READ(LU,1) TITLE(81:160)
    1 FORMAT(A80)

      READ(LU,2) NY,NX
    2 FORMAT(16X,I5,16X,I5)
      IF (NX.GT.NDIM1 .OR. NY.GT.NDIM2) THEN
         WRITE(6,*) 'Increase dimensions in call to UTCSRI'
         STOP
      ENDIF
      READ(LU,3) LATMIN,LATMAX
      READ(LU,3) LONMIN,LONMAX
    3 FORMAT(16X,F9.0,16X,F9.0)
                                 ! 2 masks may be here
      READ(LU,3) UNDEF
      READ(LU,1) FORMAT
      DO 4 J=1,NY
         READ(LU,FORMAT) (G(I,J),I=1,NX)
    4 END DO
      RETURN
      END
