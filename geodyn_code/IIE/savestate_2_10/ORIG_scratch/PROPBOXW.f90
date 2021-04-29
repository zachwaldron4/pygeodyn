      SUBROUTINE PROPBOXW(BLKNUM,AREA,REFL,DIFU,ABSP,AREA2,REFL2,   &
     &                 DIFU2,ABSP2,REFLIR,DIFUIR,ABSPIR)
!
!  NAME       :  PROPBOXW
!
!  PURPOSE    :  SATELLITE DIMENSIONS AND OPTICAL PROPERTIES FROM ROCK MODELS
!                BOX-WING MODELS FOR GPS AND GLONASS SATELLITES
!
!  REFERENCES :  Fliegel H, Gallini T, Swift E (1992) Global Positioning System
!                   Force Model for Geodetic Applications. Journal of Geophysica
!                   97(B1): 559-568
!                Fliegel H, Gallini T (1996) Solar Force Modelling of Block IIR
!                   Positioning System satellites. Journal of Spacecraft and Roc
!                   33(6): 863-866
!                Ziebart M (2001) High Precision Analytical Solar Radiation Pres
!                   Modelling for GNSS Spacecraft. PhD Thesis, University of Eas
!                [IGEXMAIL-0086] GLONASS S/C mass and dimension
!                [IGSMAIL-5104] GLONASS-M dimensions and center-of-mass correcti
!                http://acc.igs.org/orbits/IIF_SV_DimensionsConfiguration.ppt
!
!  PARAMETERS :
!          IN :  BLKNUM     : BLOCK NUMBER
!                              1 = GPS-I
!                              2 = GPS-II
!                              3 = GPS-IIA
!                              4 = GPS-IIR
!                              5 = GPS-IIR-A
!                              6 = GPS-IIR-B
!                              7 = GPS-IIR-M
!                              8 = GPS-IIF
!                            101 = GLONASS
!                            102 = GLONASS-M
!                            103 = GLONASS-K (not yet available)
!
!         OUT :  AREA(I,J)  : AREAS OF FLAT SURFACES [m^2]
!                REFL(I,J)  : REFLEXION COEFFICIENT
!                DIFU(I,J)  : DIFFUSION COEFFICIENT
!                ABSP(I,J)  : ABSORPTION COEFFICIENT
!                             I = 1 +Z (TOWARDS THE EARTH)
!                             I = 2 +Y (ALONG SOLAR PANELS BEAMS)
!                             I = 3 +X (ALONG BUS DIRECTION ALWAYS ILLUMINATED B
!                             I = 4 SOLAR PANELS
!                             J = 1 POSITIVE DIRECTION
!                             J = 2 NEGATIVE DIRECTION
!                ????2(I,J) : INDEX 2 INDICATES AREAS AND OPTICAL PROPERTIES OF
!                             SURFACES, SAME MEANING OF (I,J) AS BEFORE
!                ????IR(I,J): OPTICAL PROPERTIES IN THE INFRARED (ASSUMED), NO S
!                             BETWEEN FLAT AND CYLINDRICAL SURFACES
!
!  AUTHOR     :  C.J. RODRIGUEZ-SOLANO
!                rodriguez@bv.tum.de
!
!  VERSION    :  1.0 (OCT 2010)
!
!  CREATED    :  2010/10/18             LAST MODIFIED :  17-MAR-11
!
!  CHANGES    :  17-MAR-11 : CR: ADD BOX-WING MODELS FOR GLONASS, GLONASS-M, GPS
!*
      IMPLICIT NONE
      save ! Added by OLC, 30 Jan. 2013.
!
      INTEGER*4 BLKNUM,II,JJ,KK,SS
!
      DOUBLE PRECISION AREA(4,2),REFL(4,2),DIFU(4,2),ABSP(4,2)
      DOUBLE PRECISION AREA2(4,2),REFL2(4,2),DIFU2(4,2),ABSP2(4,2)
      DOUBLE PRECISION REFLIR(4,2),DIFUIR(4,2),ABSPIR(4,2)
      DOUBLE PRECISION X_SIDE(5,4),Z_SIDE(4,4),S_SIDE(2,4),Y_SIDE(2,4)
      DOUBLE PRECISION SURFALL(13,4)
      DOUBLE PRECISION ALLREFL(13),ALLDIFU(13),REFL_AREA(13),DIFU_AREA(13)
      DOUBLE PRECISION G_AREA1(5),G_REFL1(5),G_DIFU1(5),G_ABSP1(5)
      DOUBLE PRECISION G_AREA2(5),G_REFL2(5),G_DIFU2(5),G_ABSP2(5)
      DOUBLE PRECISION BUSFAC

! SATELLITE PROPERTIES FROM ROCK MODEL
! VALUES GIVEN FOR +X, -Z, +Z AND SOLAR PANELS
! X_SIDE(I,J) :   I     DIFFERENT SURFACE COMPONENTS
!                 J = 1 AREA
!                 J = 2 SPECULARITY
!                 J = 3 REFLECTIVITY
!                 J = 4 SHAPE: 1 = flat, 2 = cylindrical

      DO JJ = 1,4
         DO II = 1,5
            X_SIDE(II,JJ) = 0D0
         ENDDO
         DO II = 1,4
            Z_SIDE(II,JJ) = 0D0
         ENDDO
         DO II = 1,2
            S_SIDE(II,JJ) = 0D0
         ENDDO
         DO II = 1,2
            Y_SIDE(II,JJ) = 0D0
         ENDDO
      ENDDO


!     -----------
!     GPS BLOCK I
!     -----------
!     SEE FLIEGEL ET AL (1992)
      IF(BLKNUM.EQ.1)THEN

!     +X SIDE
          X_SIDE(1,1) = 1.055D0
          X_SIDE(1,2) = 0.80D0
          X_SIDE(1,3) = 0.50D0
          X_SIDE(1,4) = 1D0

!     ENGINE SIDE
          X_SIDE(2,1) = 0.570D0
          X_SIDE(2,2) = 0.75D0
          X_SIDE(2,3) = 0.86D0
          X_SIDE(2,4) = 2D0

!     TT&C ANTENNA SIDE
          X_SIDE(3,1) = 0.055D0
          X_SIDE(3,2) = 0.05D0
          X_SIDE(3,3) = 0.28D0
          X_SIDE(3,4) = 2D0

!     TT&C ANTENNA TIP
          X_SIDE(4,1) = 0.019D0
          X_SIDE(4,2) = 0.85D0
          X_SIDE(4,3) = 0.28D0
          X_SIDE(4,4) = 2D0

!     EACH NAVIGATIONAL ANTENNA ADAPTER
          X_SIDE(5,1) = 0.029D0
          X_SIDE(5,2) = 0.75D0
          X_SIDE(5,3) = 0.36D0
          X_SIDE(5,4) = 2D0

!     BODY AFT END (-Z)
          Z_SIDE(1,1) = 0.816D0
          Z_SIDE(1,2) = 0.80D0
          Z_SIDE(1,3) = 0.86D0
          Z_SIDE(1,4) = 1D0

!     ENGINE AFT END (-Z)
          Z_SIDE(2,1) = 0.694D0
          Z_SIDE(2,2) = 0D0
          Z_SIDE(2,3) = 0D0
          Z_SIDE(2,4) = 1D0

!     FORWARD END (+Z)
          Z_SIDE(3,1) = 1.510D0
          Z_SIDE(3,2) = 0.75D0
          Z_SIDE(3,3) = 0.86D0
          Z_SIDE(3,4) = 1D0

!     ALL SOLAR PANELS
          S_SIDE(1,1) = 5.583D0
          S_SIDE(1,2) = 0.85D0
          S_SIDE(1,3) = 0.23D0
          S_SIDE(1,4) = 1D0

!     SOLAR PANELS MASTS
          S_SIDE(2,1) = 0.470D0
          S_SIDE(2,2) = 0.85D0
          S_SIDE(2,3) = 0.85D0
          S_SIDE(2,4) = 2D0

!     -----------------
!     GPS BLOCK II, IIA
!     -----------------
!     SEE FLIEGEL ET AL (1992)
      ELSEIF((BLKNUM.EQ.2).OR.(BLKNUM.EQ.3))THEN

!     +X SIDE
          X_SIDE(1,1) = 1.553D0
          X_SIDE(1,2) = 0.20D0
          X_SIDE(1,3) = 0.56D0
          X_SIDE(1,4) = 1D0

!     ENGINE SIDE (INCLUDES PLUME SHILED 0.22*1.84 m^2)
          X_SIDE(2,1) = 1.054D0
          X_SIDE(2,2) = 0.20D0
          X_SIDE(2,3) = 0.56D0
          X_SIDE(2,4) = 2D0

!     TT&C ANTENNA SIDE
          X_SIDE(3,1) = 0.105D0
          X_SIDE(3,2) = 0.20D0
          X_SIDE(3,3) = 0.28D0
          X_SIDE(3,4) = 2D0

!     EACH NAVIGATIONAL ANTENNA ADAPTER
          X_SIDE(5,1) = 0.181D0
          X_SIDE(5,2) = 0.20D0
          X_SIDE(5,3) = 0.36D0
          X_SIDE(5,4) = 2D0

!     BODY AFT END (-Z)
          Z_SIDE(1,1) = 2.152D0
          Z_SIDE(1,2) = 0.20D0
          Z_SIDE(1,3) = 0.56D0
          Z_SIDE(1,4) = 1D0

!     ENGINE AFT END (-Z)
          Z_SIDE(2,1) = 0.729D0
          Z_SIDE(2,2) = 0D0
          Z_SIDE(2,3) = 0D0
          Z_SIDE(2,4) = 1D0

!     FORWARD END (+Z)
          Z_SIDE(3,1) = 2.881D0
          Z_SIDE(3,2) = 0.20D0
          Z_SIDE(3,3) = 0.56D0
          Z_SIDE(3,4) = 1D0

!     ALL SOLAR PANELS
          S_SIDE(1,1) = 10.886D0
          S_SIDE(1,2) = 0.85D0
          S_SIDE(1,3) = 0.23D0
          S_SIDE(1,4) = 1D0

!     SOLAR PANELS MASTS
          S_SIDE(2,1) = 0.985D0
          S_SIDE(2,2) = 0.41D0
          S_SIDE(2,3) = 0.52D0
          S_SIDE(2,4) = 2D0

!     ----------------------------------
!     GPS BLOCK IIR,  IIR-M
!     ----------------------------------
!     SEE FLIEGEL AND GALINI (1996)
      ELSEIF((BLKNUM.GE.4).AND.(BLKNUM.LE.5))THEN

!     + AND -X FACES
         X_SIDE(1,1) = 3.05D0
         X_SIDE(1,2) = 0D0
         X_SIDE(1,3) = 0.06D0
         X_SIDE(1,4) = 1D0

!     PLUME SHILED
         X_SIDE(2,1) = 0.17D0
         X_SIDE(2,2) = 0D0
         X_SIDE(2,3) = 0.06D0
         X_SIDE(2,4) = 2D0

!     ANTENNA SHROUD
         X_SIDE(3,1) = 0.89D0
         X_SIDE(3,2) = 0D0
         X_SIDE(3,3) = 0.06D0
         X_SIDE(3,4) = 2D0

!     -Z FACE
         Z_SIDE(1,1) = 3.75D0
         Z_SIDE(1,2) = 0D0
         Z_SIDE(1,3) = 0.06D0
         Z_SIDE(1,4) = 1D0

!     -Z W-SENSOR
         Z_SIDE(2,1) = 0.50D0
         Z_SIDE(2,2) = 0D0
         Z_SIDE(2,3) = 0.06D0
         Z_SIDE(2,4) = 1D0

!     +Z FACE
         Z_SIDE(3,1) = 3.75D0
         Z_SIDE(3,2) = 0D0
         Z_SIDE(3,3) = 0.06D0
         Z_SIDE(3,4) = 1D0

!     +Z W-SENSOR
         Z_SIDE(4,1) = 0.50D0
         Z_SIDE(4,2) = 0D0
         Z_SIDE(4,3) = 0.06D0
         Z_SIDE(4,4) = 1D0

!     ALL SOLAR PANELS
         S_SIDE(1,1) = 13.60D0
         S_SIDE(1,2) = 0.85D0
         S_SIDE(1,3) = 0.28D0
         S_SIDE(1,4) = 1D0

!     SOLAR PANELS BEAMS
         S_SIDE(2,1) = 0.32D0
         S_SIDE(2,2) = 0.85D0
         S_SIDE(2,3) = 0.85D0
         S_SIDE(2,4) = 1D0

!     -------------
!     GPS BLOCK IIF
!     -------------
!     SEE IIF PRESENTATION
!     OPTICAL PROPERTIES NOT KNOWN, SAME ASSUMPTIONS AS ZIEBART (2001)
      ELSEIF(BLKNUM.EQ.6)THEN
!     +/- X SIDE
         X_SIDE(1,1) = 5.72D0
         X_SIDE(1,2) = 0.20D0
         X_SIDE(1,3) = 0.56D0
         X_SIDE(1,4) = 1D0

!     +/- Y SIDE
         Y_SIDE(1,1) = 7.01D0
         Y_SIDE(1,2) = 0.20D0
         Y_SIDE(1,3) = 0.56D0
         Y_SIDE(1,4) = 1D0

!     -Z SIDE
         Z_SIDE(1,1) = 5.40D0
         Z_SIDE(1,2) = 0D0
         Z_SIDE(1,3) = 0D0
         Z_SIDE(1,4) = 1D0

!     +Z SIDE
         Z_SIDE(3,1) = 5.40D0
         Z_SIDE(3,2) = 0D0
         Z_SIDE(3,3) = 0D0
         Z_SIDE(3,4) = 1D0

!     SOLAR PANELS
         S_SIDE(1,1) = 22.25D0
         S_SIDE(1,2) = 0.85D0
         S_SIDE(1,3) = 0.23D0
         S_SIDE(1,4) = 1D0

!     -------
!     GLONASS
!     -------
!     SEE ZIEBART (2001)
!     OPTICAL PROPERTIES NOT KNOWN, SAME ASSUMPTIONS AS ZIEBART (2001)
!     ASSUMED SAME SHAPE FOR GLONASS-M, SEE END OF SUBROUTINE
      ELSEIF((BLKNUM.EQ.101).OR.(BLKNUM.EQ.102))THEN
!     +/- X SIDE (FLAT)
         X_SIDE(1,1) = 1.258D0
         X_SIDE(1,2) = 0.20D0
         X_SIDE(1,3) = 0.56D0
         X_SIDE(1,4) = 1D0

!     +/- X SIDE (CYLINDRICAL)
         X_SIDE(2,1) = 2.052D0
         X_SIDE(2,2) = 0.20D0
         X_SIDE(2,3) = 0.56D0
         X_SIDE(2,4) = 2D0

!     +/- Y SIDE (FLAT)
         Y_SIDE(1,1) = 2.591D0
         Y_SIDE(1,2) = 0.20D0
         Y_SIDE(1,3) = 0.56D0
         Y_SIDE(1,4) = 1D0

!     +/- Y SIDE (CYLINDRICAL)
         Y_SIDE(2,1) = 2.532D0
         Y_SIDE(2,2) = 0.20D0
         Y_SIDE(2,3) = 0.56D0
         Y_SIDE(2,4) = 2D0

!     -Z SIDE (BUS)
         Z_SIDE(1,1) = 0.877D0
         Z_SIDE(1,2) = 0.20D0
         Z_SIDE(1,3) = 0.56D0
         Z_SIDE(1,4) = 1D0

!     -Z SIDE (APOGEE ENGINE)
         Z_SIDE(2,1) = 0.785D0
         Z_SIDE(2,2) = 0D0
         Z_SIDE(2,3) = 0D0
         Z_SIDE(2,4) = 1D0

!     +Z SIDE (BUS)
         Z_SIDE(3,1) = 1.412D0
         Z_SIDE(3,2) = 0.20D0
         Z_SIDE(3,3) = 0.56D0
         Z_SIDE(3,4) = 1D0

!     +Z SIDE (RETRO REFLECTOR ARRAY)
         Z_SIDE(4,1) = 0.250D0
         Z_SIDE(4,2) = 1D0
         Z_SIDE(4,3) = 1D0
         Z_SIDE(4,4) = 1D0

!     SOLAR PANELS
         S_SIDE(1,1) = 23.616D0
         S_SIDE(1,2) = 0.85D0
         S_SIDE(1,3) = 0.23D0
         S_SIDE(1,4) = 1D0

      ENDIF


! ---------------------------
! BOX-WING MODEL APROXIMATION
! ---------------------------

! NAVIGATION ANTENNAS
      X_SIDE(5,1) = X_SIDE(5,1)*12

! MATRIX WITH ALL SURFACES
      DO II = 1,13
         DO JJ = 1,4
            IF(II.LE.5)THEN
               SURFALL(II,JJ) = X_SIDE(II,JJ)
            ELSEIF((II.GE.6).AND.(II.LE.9))THEN
               SURFALL(II,JJ) = Z_SIDE(II-5,JJ)
            ELSEIF((II.GE.10).AND.(II.LE.11))THEN
               SURFALL(II,JJ) = S_SIDE(II-9,JJ)
            ELSEIF(II.GE.12)THEN
               SURFALL(II,JJ) = Y_SIDE(II-11,JJ)
            ENDIF
         ENDDO
      ENDDO


! COMPUTATION OF FRACTION OF REFLECTED, DIFFUSSED AND ABSORVED PHOTONS
! FROM SPECULARITY AND REFLECTIVITY
      DO II = 1,13
         ALLREFL(II) = SURFALL(II,2)*SURFALL(II,3)
         ALLDIFU(II) = SURFALL(II,3)*(1-SURFALL(II,2))
      ENDDO

! MULTIPLICATION OF OPTICAL PROPERTIES WITH SURFACE AREA
      DO II = 1,13
         REFL_AREA(II) = ALLREFL(II)*SURFALL(II,1)
         DIFU_AREA(II) = ALLDIFU(II)*SURFALL(II,1)
      ENDDO

! AVERAGE PER SURFACE (SEPARATE FOR FLAT AND CYLINDRICAL SURFACES)
      DO SS = 1,5
         G_AREA1(SS) = 0D0
         G_REFL1(SS) = 0D0
         G_DIFU1(SS) = 0D0
         G_ABSP1(SS) = 0D0
         G_AREA2(SS) = 0D0
         G_REFL2(SS) = 0D0
         G_DIFU2(SS) = 0D0
         G_ABSP2(SS) = 0D0
      ENDDO


      DO II = 1,13
!        +X SIDE
         IF(II.LE.5)THEN
            SS = 1
!        -Z SIDE
         ELSEIF((II.GE.6).AND.(II.LE.7))THEN
            SS = 2
!        +Z SIDE
         ELSEIF((II.GE.8).AND.(II.LE.9))THEN
            SS = 3
!        SOLAR PANELS
         ELSEIF((II.GE.10).AND.(II.LE.11))THEN
            SS = 4
!        +/- Y SIDE
         ELSEIF(II.GE.12)THEN
            SS = 5
         ENDIF

         IF(SURFALL(II,4).EQ.1D0)THEN
            G_AREA1(SS) = G_AREA1(SS) + SURFALL(II,1)
            G_REFL1(SS) = G_REFL1(SS) + REFL_AREA(II)
            G_DIFU1(SS) = G_DIFU1(SS) + DIFU_AREA(II)
         ELSEIF(SURFALL(II,4).EQ.2D0)THEN
            G_AREA2(SS) = G_AREA2(SS) + SURFALL(II,1)
            G_REFL2(SS) = G_REFL2(SS) + REFL_AREA(II)
            G_DIFU2(SS) = G_DIFU2(SS) + DIFU_AREA(II)
         ENDIF
      ENDDO

! AVERAGE OF OPTICAL PROPERTIES A! ORDING TO SURFACES AREA
      DO SS = 1,5
         IF(G_AREA1(SS).GT.0D0)THEN
            G_REFL1(SS) = G_REFL1(SS)/G_AREA1(SS)
            G_DIFU1(SS) = G_DIFU1(SS)/G_AREA1(SS)
            G_ABSP1(SS) = 1D0 - (G_REFL1(SS) + G_DIFU1(SS))
         ENDIF
         IF(G_AREA2(SS).GT.0D0)THEN
            G_REFL2(SS) = G_REFL2(SS)/G_AREA2(SS)
            G_DIFU2(SS) = G_DIFU2(SS)/G_AREA2(SS)
            G_ABSP2(SS) = 1D0 -(G_REFL2(SS) + G_DIFU2(SS))
         ENDIF
      ENDDO


! --------------------------------------------
! ARRAYS OF OPTICAL PROPERTIES AND DIMENSIONS
! --------------------------------------------

      DO SS = 1,4
         DO KK = 1,2
            AREA(SS,KK) = 0D0
            REFL(SS,KK) = 0D0
            DIFU(SS,KK) = 0D0
            ABSP(SS,KK) = 0D0
            AREA2(SS,KK) = 0D0
            REFL2(SS,KK) = 0D0
            DIFU2(SS,KK) = 0D0
            ABSP2(SS,KK) = 0D0
         ENDDO
      ENDDO

!     +Z FACE
      AREA(1,1) = G_AREA1(3)
      REFL(1,1) = G_REFL1(3)
      DIFU(1,1) = G_DIFU1(3)
      ABSP(1,1) = G_ABSP1(3)

!     -Z FACE
      AREA(1,2) = G_AREA1(2)
      REFL(1,2) = G_REFL1(2)
      DIFU(1,2) = G_DIFU1(2)
      ABSP(1,2) = G_ABSP1(2)

!     +X FACE
      AREA(3,1) = G_AREA1(1)
      REFL(3,1) = G_REFL1(1)
      DIFU(3,1) = G_DIFU1(1)
      ABSP(3,1) = G_ABSP1(1)

!     +X CYLINDRICAL
      AREA2(3,1) = G_AREA2(1)
      REFL2(3,1) = G_REFL2(1)
      DIFU2(3,1) = G_DIFU2(1)
      ABSP2(3,1) = G_ABSP2(1)

!     -X FACE (ASSUMED FOR ALL BLOCKS)
      AREA(3,2) = G_AREA1(1)
      REFL(3,2) = G_REFL1(1)
      DIFU(3,2) = G_DIFU1(1)
      ABSP(3,2) = G_ABSP1(1)

!     -X CYLINDRICAL (ASSUMED FOR ALL BLOCKS)
      AREA2(3,2) = G_AREA2(1)
      REFL2(3,2) = G_REFL2(1)
      DIFU2(3,2) = G_DIFU2(1)
      ABSP2(3,2) = G_ABSP2(1)

!     +Y FACE (ASSUMED FOR BLKNUM = 1...5)
      IF((BLKNUM.GE.1).AND.(BLKNUM.LE.5))THEN
!        DUE TO BLOCK IIR W-SENSOR
         IF((BLKNUM.GE.4).AND.(BLKNUM.LE.5))THEN
            AREA(2,1) = ((AREA(1,1)-0.5D0) + AREA(3,1))/(2D0)
         ELSE
            AREA(2,1) = (AREA(1,1) + AREA(3,1))/(2D0)
         ENDIF
         REFL(2,1) = (REFL(1,1) + REFL(1,2) + REFL(3,1) + REFL(3,2))/4
         DIFU(2,1) = (DIFU(1,1) + DIFU(1,2) + DIFU(3,1) + DIFU(3,2))/4
         ABSP(2,1) = (ABSP(1,1) + ABSP(1,2) + ABSP(3,1) + ABSP(3,2))/4

!        CYLINDRICAL PART EQUAL TO +/- X CYLINDRICAL
         AREA2(2,1) = AREA2(3,1)
         REFL2(2,1) = REFL2(3,1)
         DIFU2(2,1) = DIFU2(3,1)
         ABSP2(2,1) = ABSP2(3,1)

!     FOR OTHER SATELLITE BLOCKS
      ELSE
         AREA(2,1) = G_AREA1(5)
         REFL(2,1) = G_REFL1(5)
         DIFU(2,1) = G_DIFU1(5)
         ABSP(2,1) = G_ABSP1(5)

         AREA2(2,1) = G_AREA2(5)
         REFL2(2,1) = G_REFL2(5)
         DIFU2(2,1) = G_DIFU2(5)
         ABSP2(2,1) = G_ABSP2(5)
      ENDIF

!     -Y FACE (ASSUMED FOR ALL BLOCKS)
      AREA(2,2) = AREA(2,1)
      REFL(2,2) = REFL(2,1)
      DIFU(2,2) = DIFU(2,1)
      ABSP(2,2) = ABSP(2,1)

!     -Y CYLINDRICAL (ASSUMED FOR ALL BLOCKS)
      AREA2(2,2) = AREA2(2,1)
      REFL2(2,2) = REFL2(2,1)
      DIFU2(2,2) = DIFU2(2,1)
      ABSP2(2,2) = ABSP2(2,1)

!     FRONT OF SOLAR PANELS
      AREA(4,1) = G_AREA1(4)
      REFL(4,1) = G_REFL1(4)
      DIFU(4,1) = G_DIFU1(4)
      ABSP(4,1) = G_ABSP1(4)

!     FRONT OF SOLAR PANELS (CYLINDER)
      AREA2(4,1) = G_AREA2(4)
      REFL2(4,1) = G_REFL2(4)
      DIFU2(4,1) = G_DIFU2(4)
      ABSP2(4,1) = G_ABSP2(4)

!     BACK OF SOLAR PANELS (ASSUMED FOR ALL BLOCKS)
      AREA(4,2) = AREA(4,1)
      REFL(4,2) = 0.055D0
      DIFU(4,2) = 0.055D0
      ABSP(4,2) = 0.890D0


!     BACK OF SOLAR PANELS (CYLINDER)
      AREA2(4,2) = G_AREA2(4)
      REFL2(4,2) = G_REFL2(4)
      DIFU2(4,2) = G_DIFU2(4)
      ABSP2(4,2) = G_ABSP2(4)


!     OPTICAL PROPERTIES IN THE INFRARED
!     ASSUMED FOR ALL BLOCKS AND ALL SURFACES
      DO SS=1,4
         DO II=1,2
            REFLIR(SS,II) = 0.10D0
            DIFUIR(SS,II) = 0.10D0
            ABSPIR(SS,II) = 0.80D0
         ENDDO
      ENDDO

!     NOT ENOUGH INFORMATION AVAILABLE FOR GLONASS-M
!     SEE IGSMAIL-5104
      IF(BLKNUM.EQ.102)THEN
         BUSFAC = 4.2D0/3.31D0
         DO SS=1,3
            DO II=1,2
               AREA(SS,II) = BUSFAC*AREA(SS,II)
               AREA2(SS,II) = BUSFAC*AREA2(SS,II)
            ENDDO
         ENDDO
         AREA(4,1) = 30.85D0
         AREA(4,2) = 30.85D0
      ENDIF

!     NO (YET) INFORMATION AVAILABLE FOR GLONASS-K
      IF(((BLKNUM.GE.9).AND.(BLKNUM.LE.100))   &
     &  .OR.(BLKNUM.GE.103))THEN
         DO SS=1,4
            DO II=1,2
               AREA(SS,II) = 0D0
               REFL(SS,II) = 0D0
               DIFU(SS,II) = 0D0
               ABSP(SS,II) = 0D0

               AREA2(SS,II) = 0D0
               REFL2(SS,II) = 0D0
               DIFU2(SS,II) = 0D0
               ABSP2(SS,II) = 0D0

               REFLIR(SS,II) = 0D0
               DIFUIR(SS,II) = 0D0
               ABSPIR(SS,II) = 0D0
            ENDDO
         ENDDO
      ENDIF

      END SUBROUTINE
