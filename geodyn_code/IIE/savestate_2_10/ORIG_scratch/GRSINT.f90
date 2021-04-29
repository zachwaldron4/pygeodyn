!
!--------------------------------------------------------------------
      SUBROUTINE GRSINT( GRID,ND1,ND2,NGRIDS,NX,NY,UNDEF,               &
     &                   LATMIN,LATMAX,LONMIN,LONMAX,WRAP,              &
     &                   DLAT,DLON,VAL,ISDATA )
!
!  Interpolates a value from a grid of data at the desired location.
!  Interpolation is bilinear.
!  First 12 arguments above all describe the grid.
!  WRAP is T if grid allows wrap-around in longitude.
!  DLAT,DLON - location of desired position for interpolation.
!  VAL - returns interpolated value.
!  ISDATA is returned F if no valid data at position (DLAT,DLON).
!
!  R. Ray   3/13/91
!
!  Revised 6/3/93 to allow multiple grids to be handled.
!     VAL is now returned as an array; GRID is changed to 3 dimensions.
!     ISDATA is still a scalar (all grids assumed to have same nulls).
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION       LATMIN,LATMAX,LONMIN,LONMAX
      DIMENSION  GRID(ND1,ND2,*), VAL(*)
      LOGICAL    ISDATA,WRAP
      ISDATA = .TRUE.
      DX = (LONMAX - LONMIN)/DBLE(NX - 1)
      DY = (LATMAX - LATMIN)/DBLE(NY - 1)

!     compute indices for desired position
!     ------------------------------------
      JLAT1 = INT((DLAT - LATMIN)/DY) + 1
      JLAT2 = JLAT1 + 1
      IF (JLAT1.LT.1 .OR. JLAT2.GT.NY) THEN
         ISDATA = .FALSE.
         RETURN
      ENDIF
      XLON = DLON
      IF (WRAP.AND.XLON.LT.LONMIN) XLON = XLON + 360.0
      IF (WRAP.AND.XLON.GT.LONMAX) XLON = XLON - 360.0
      ILON1 = INT((XLON - LONMIN)/DX) + 1
      ILON2 = ILON1 + 1
      IF (ILON2.GT.NX) THEN
         IF (WRAP) THEN
            ILON2 = 1
         ELSE
            ISDATA = .FALSE.
            RETURN
         END IF
      ENDIF
                                                ! should never happen.
      IF (ILON1.LT.1 .OR. ILON1.GT.NX) STOP 301

      IF (GRID(ILON1,JLAT1,1).EQ.UNDEF .AND.                            &
     &    GRID(ILON2,JLAT1,1).EQ.UNDEF .AND.                            &
     &    GRID(ILON1,JLAT2,1).EQ.UNDEF .AND.                            &
     &    GRID(ILON2,JLAT2,1).EQ.UNDEF)      THEN
         ISDATA = .FALSE.
         RETURN
      ENDIF
      W1 = 0.0
      W2 = 0.0
      WX1 = (DX - (XLON - DBLE(ILON1-1)*DX - LONMIN))/DX
      WX2 = 1.0 - WX1
      WY1 = (DY - (DLAT - DBLE(JLAT1-1)*DY - LATMIN))/DY
      WY2 = 1.0 - WY1
!  Interpolation weights:
!  W1,W2,W3,W4 are for northwest,northeast,southeast,southwest corners.
      W1 = WX1*WY2
      W2 = WX2*WY2
      W3 = WX2*WY1
      W4 = WX1*WY1
      W = 0.0
      DO 10 I=1,NGRIDS
   10 VAL(I) = 0.0

!     get weights & interpolate
!     -------------------------
      IF (GRID(ILON1,JLAT1,1).NE.UNDEF) THEN
         W = W4
         DO 20 I=1,NGRIDS
   20    VAL(I) = W4*GRID(ILON1,JLAT1,I)
      ENDIF
      IF (GRID(ILON1,JLAT2,1).NE.UNDEF) THEN
         W = W + W1
         DO 30 I=1,NGRIDS
   30    VAL(I) = VAL(I) + W1*GRID(ILON1,JLAT2,I)
      ENDIF
      IF (GRID(ILON2,JLAT2,1).NE.UNDEF) THEN
         W = W + W2
         DO 40 I=1,NGRIDS
   40    VAL(I) = VAL(I) + W2*GRID(ILON2,JLAT2,I)
      ENDIF
      IF (GRID(ILON2,JLAT1,1).NE.UNDEF) THEN
         W = W + W3
         DO 50 I=1,NGRIDS
   50    VAL(I) = VAL(I) + W3*GRID(ILON2,JLAT1,I)
      ENDIF
      IF (W.GT.0.5) THEN
         DO 60 I=1,NGRIDS
   60    VAL(I) = VAL(I)/W
      ELSE
         ISDATA = .FALSE.
      ENDIF
      RETURN
      END
