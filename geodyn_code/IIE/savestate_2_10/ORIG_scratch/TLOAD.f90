!
!----------------------------------------------------------------------
      SUBROUTINE TLOAD( ARRAY,MX,MY, SELECT, LU, ZNULL,                 &
     &                  NX,NY,LATMIN,LATMAX,LONMIN,LONMAX,              &
     &                  NGRIDS )
!
!  Loads ARRAY with the data from the input files of harmonic constants.
!
!  ARRAY  - holds inphase & quadrature components for all tides.
!  ZNULL  - desired value of NULL data.
!  LU     - tide coeffs will be read on fortran unit LU.
!  SELECT - 8-element logical array. NOT USED WITH PERTH2.
!
!  All remaining arguments describe the tidal grids, except:
!  NGRIDS - returns the total number of constituents loaded.
!
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION       ARRAY(MX,MY,2,*)
      DOUBLE PRECISION       LATMIN,LATMAX,LONMIN,LONMAX
      PARAMETER (NT=8)
      LOGICAL    SELECT(NT)
      PARAMETER (D2R=1.745329E-2,  MXX=720,MXY=361)
!      DIMENSION  AMP(MXX,MXY), PHA(MXX,MXY)
      ALLOCATABLE AMP(:,:), PHA(:,:)
      CHARACTER  TITLE(2,8)*160

!
!     ALLOCATE AMP and PHA
!     ------------------------

       print *,'Tload: allocating amp and pha'

      ALLOCATE(AMP(MXX,MXY),PHA(MXX,MXY),STAT=memgot)

      print *,'Tload: memgot: ',memgot
      IF(MEMGOT.NE.0) THEN
       WRITE(6,*) ''
       WRITE(6,*) '*****************************************'
       WRITE(6,*) 'ABNORMAL TERMINATION IN TLOAD'
       WRITE(6,*) 'TROUBLE ALLOCATING MEMORY FOR GOT99 GRID'
       WRITE(6,*) 'MEMGOT IS: ', MEMGOT
       WRITE(6,*) '*****************************************'
       WRITE(6,*) ''
       STOP 69
      ENDIF

!
!     Read grids of ascii data
!     ------------------------
      NGRIDS = 0
      DO 300 IT=1,NT
         IF (SELECT(IT)) THEN
            CALL UTCSRI( AMP,MXX,MXY,NX,NY,TITLE(1,IT),LU,              &
     &                   LATMIN,LATMAX,LONMIN,LONMAX,UNDEF )
            CALL UTCSRI( PHA,MXX,MXY,NX,NY,TITLE(2,IT),LU,              &
     &                   LATMIN,LATMAX,LONMIN,LONMAX,UNDEF )
            NGRIDS = NGRIDS + 1
            IF (NX.GT.MX .OR. NY.GT.MY) THEN
               WRITE(6,5) MX,MY
    5          FORMAT('Size of ARRAY(',I4,',',I4,') must be increased.')
               STOP
            ENDIF
            DO 200 J=1,NY
            DO 200 I=1,NX
               IF (PHA(I,J).EQ.UNDEF .OR. AMP(I,J).EQ.UNDEF) THEN
                  ARRAY(I,J,1,NGRIDS) = ZNULL
                  ARRAY(I,J,2,NGRIDS) = ZNULL
                  ARRAY(I,J,1,1     ) = ZNULL
               ELSE
                  ARRAY(I,J,1,NGRIDS) = AMP(I,J) * COS(PHA(I,J)*D2R)
                  ARRAY(I,J,2,NGRIDS) = AMP(I,J) * SIN(PHA(I,J)*D2R)
               ENDIF
  200       CONTINUE
         ENDIF
  300 END DO
  400 CONTINUE
!
!     Check for possible user setup error
!     -----------------------------------
      CALL CHECK1( SELECT, TITLE )
!
!     DEALLOCATE AMP and PHA
!     ------------------------

       print *,'Tload: deallocating amp and pha'

      DEALLOCATE(AMP,PHA,STAT=memgot)

      print *,'Tload: memgot: ',memgot
      IF(MEMGOT.NE.0) THEN
       WRITE(6,*) ''
       WRITE(6,*) '*****************************************'
       WRITE(6,*) 'ABNORMAL TERMINATION IN TLOAD'
       WRITE(6,*) 'TROUBLE DEALLOCATING MEMORY FOR GOT99 GRID'
       WRITE(6,*) 'MEMGOT IS: ', MEMGOT
       WRITE(6,*) '*****************************************'
       WRITE(6,*) ''
       STOP 69
      ENDIF

      RETURN
      END
