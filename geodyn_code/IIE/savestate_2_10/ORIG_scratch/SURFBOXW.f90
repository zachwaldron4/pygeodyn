      SUBROUTINE SURFBOXW(AREA,REFL,DIFU,ABSP,AREA2,REFL2,DIFU2,       &
     &                    ABSP2,REFLIR,DIFUIR,ABSPIR,ABSNCFVI,ABSNCFIR,&
     &                    NCFVEC,ATTSURF,FORCE)
!
!  NAME       :  SURFBOXW
!
!  PURPOSE    :  COMPUTES THE FORCE IN THE SATELLITE SURFACES, FOR A GIVEN RADIA
!
!  PARAMETERS :
!          IN :  AREA(I,J)   : AREAS OF FLAT SURFACES [m^2]
!                REFL(I,J)   : REFLEXION COEFFICIENT
!                DIFU(I,J)   : DIFFUSION COEFFICIENT
!                ABSP(I,J)   : ABSORPTION COEFFICIENT
!                              I = 1 +Z (TOWARDS THE EARTH)
!                              I = 2 +Y (ALONG SOLAR PANELS BEAMS)
!                              I = 3 +X (ALONG BUS DIRECTION ALWAYS ILLUMINATED
!                              I = 4 SOLAR PANELS
!                              J = 1 POSITIVE DIRECTION
!                              J = 2 NEGATIVE DIRECTION
!                ????2(I,J)  : INDEX 2 INDICATES AREAS AND OPTICAL PROPERTIES OF
!                              SURFACES, SAME MEANING OF (I,J) AS BEFORE
!                ????IR(I,J ): OPTICAL PROPERTIES IN THE INFRARED, NO SEPARATION
!                              BETWEEN FLAT AND CYLINDRICAL SURFACES
!                ABSNCFVI    : MAGNITUDE OF INCIDENT VISIBLE RADIATION [N/m^2]
!                ABSNCFIR    : MAGNITUDE OF INCIDENT INFRARED RADIATION [N/m^2]
!                NCFVEC      : UNITARY INCIDENT RADIATION VECTOR IN INERTIAL REF
!                ATTSURF(K,I): ATTITUDE VECTORS OF SATELLITE SURFACES
!                              K = 1,2,3 VECTOR COMPONENTS IN INERTIAL REFERENCE
!                              I = 1 +Z (TOWARDS THE EARTH)
!                              I = 2 +Y (ALONG SOLAR PANELS BEAMS)
!                              I = 3 +X (ALONG BUS DIRECTION ALWAYS ILLUMINATED
!                              I = 4 SOLAR PANELS
!
!         OUT : FORCE        : RESULTING FORCE VECTOR [Newtons] IN INERTIAL REFE
!
!  AUTHOR     :  C.J. RODRIGUEZ-SOLANO
!                rodriguez@bv.tum.de
!
!  VERSION    :  1.0 (OCT 2010)
!
!  CREATED    :  2010/10/18
!
!!
      IMPLICIT NONE
      save ! Added by OLC, 30 Jan. 2013.
!
      INTEGER*4 K,I
      INTEGER*4 SIDE(4)


      DOUBLE PRECISION ABSNCFVI,ABSNCFIR,PI
      DOUBLE PRECISION FACABDI,FACREFL
      DOUBLE PRECISION NCFVEC(3),FORCE(3)
      DOUBLE PRECISION ATTSURF(3,4),NORVEC(3,4)
      DOUBLE PRECISION AREA(4,2),REFL(4,2),DIFU(4,2),ABSP(4,2)
      DOUBLE PRECISION AREA2(4,2),REFL2(4,2),DIFU2(4,2),ABSP2(4,2)
      DOUBLE PRECISION REFLIR(4,2),DIFUIR(4,2),ABSPIR(4,2)
      DOUBLE PRECISION COSANG(4)
      DOUBLE PRECISION TABSP(3),TREFL(3),TDIFU(3)
      DOUBLE PRECISION TABSP2(3),TREFL2(3),TDIFU2(3)
      DOUBLE PRECISION PRT_FLT(3,3),PRT_CYL(3,3),PRT_FLTIR(3,3),PRT_CYLIR(3,3)
      DOUBLE PRECISION FVI_FLT(3),FVI_CYL(3),FIR_FLT(3),FIR_CYL(3)
      DOUBLE PRECISION FVI(3),FIR(3)

! Initialization of variables
      PI = 4D0*ATAN(1D0)

      FORCE(1) = 0D0
      FORCE(2) = 0D0
      FORCE(3) = 0D0

! --------------------------------
! ANGLE BETWEEN FORCE AND SURFACES
! --------------------------------
      SIDE(1) = 1
      SIDE(2) = 1
      SIDE(3) = 1
      SIDE(4) = 1

      DO K=1,4
         COSANG(K) = ATTSURF(1,K)*NCFVEC(1)+ATTSURF(2,K)*NCFVEC(2)  &
     &            +ATTSURF(3,K)*NCFVEC(3)

         IF(COSANG(K).GE.0D0)THEN
            SIDE(K) = 2
            NORVEC(1,K) = ATTSURF(1,K)
            NORVEC(2,K) = ATTSURF(2,K)
            NORVEC(3,K) = ATTSURF(3,K)
         ELSEIF(COSANG(K).LT.0D0)THEN
            SIDE(K) = 1
            NORVEC(1,K) = -ATTSURF(1,K)
            NORVEC(2,K) = -ATTSURF(2,K)
            NORVEC(3,K) = -ATTSURF(3,K)
         ENDIF
      ENDDO


! --------------------------------------
! FORCE ACTING ON THE SATELLITE SURFACES
! --------------------------------------
      DO K=1,4
         FACABDI = ABS(COSANG(K))
         FACREFL = COSANG(K)**2

         DO I=1,3
            TABSP(I) = FACABDI*NCFVEC(I)
            TABSP2(I)= TABSP(I)

            TREFL(I)   =   (2D0)*FACREFL*NORVEC(I,K)
            TREFL2(I)= (4D0/3D0)*FACREFL*NORVEC(I,K)

            TDIFU(I) = FACABDI*(NCFVEC(I)+(2D0/3D0)*NORVEC(I,K))
            TDIFU2(I)= FACABDI*(NCFVEC(I) +(PI/6D0)*NORVEC(I,K))

!           SATELLITE BUS, INCLUDES DIFFUSE THERMAL RE-RADIATION
            IF(K.LE.3)THEN
               PRT_FLT(1,I) = TDIFU(I)*(ABSP(K,SIDE(K))+DIFU(K,SIDE(K)))
               PRT_FLT(2,I) = TREFL(I)*REFL(K,SIDE(K))
               PRT_FLT(3,I) = 0D0
               PRT_CYL(1,I) = TDIFU2(I)*(ABSP2(K,SIDE(K))  &
     &                                  +DIFU2(K,SIDE(K)))
               PRT_CYL(2,I) = TREFL2(I)*REFL2(K,SIDE(K))
               PRT_CYL(3,I) = 0D0

               PRT_FLTIR(1,I) = TDIFU(I)*(ABSPIR(K,SIDE(K)) &
     &                                   +DIFUIR(K,SIDE(K)))
               PRT_FLTIR(2,I) = TREFL(I)*REFLIR(K,SIDE(K))
               PRT_FLTIR(3,I) = 0D0
               PRT_CYLIR(1,I) = TDIFU2(I)*(ABSPIR(K,SIDE(K)) &
     &                                    +DIFUIR(K,SIDE(K)))
               PRT_CYLIR(2,I) = TREFL2(I)*REFLIR(K,SIDE(K))
               PRT_CYLIR(3,I) = 0D0

!        SOLAR PANELS, THERMAL RE-RADIATION ALMOST CANCELS (FRONT AND BACK)
            ELSEIF(K.EQ.4)THEN
               PRT_FLT(1,I) = TABSP(I)*ABSP(K,SIDE(K))
               PRT_FLT(2,I) = TREFL(I)*REFL(K,SIDE(K))
               PRT_FLT(3,I) = TDIFU(I)*DIFU(K,SIDE(K))
               PRT_CYL(1,I) = TABSP2(I)*ABSP2(K,SIDE(K))
               PRT_CYL(2,I) = TREFL2(I)*REFL2(K,SIDE(K))
               PRT_CYL(3,I) = TDIFU2(I)*DIFU2(K,SIDE(K))
               PRT_FLTIR(1,I) = TABSP(I)*ABSPIR(K,SIDE(K))
               PRT_FLTIR(2,I) = TREFL(I)*REFLIR(K,SIDE(K))
               PRT_FLTIR(3,I) = TDIFU(I)*DIFUIR(K,SIDE(K))
               PRT_CYLIR(1,I) = TABSP2(I)*ABSPIR(K,SIDE(K))
               PRT_CYLIR(2,I) = TREFL2(I)*REFLIR(K,SIDE(K))
               PRT_CYLIR(3,I) = TDIFU2(I)*DIFUIR(K,SIDE(K))
            ENDIF

!           TOTAL FORCE COMPUTATION
            FVI_FLT(I) = PRT_FLT(1,I)  + PRT_FLT(2,I)  + PRT_FLT(3,I)
            FVI_CYL(I) = PRT_CYL(1,I)  + PRT_CYL(2,I)  + PRT_CYL(3,I)
            FIR_FLT(I) = PRT_FLTIR(1,I)+ PRT_FLTIR(2,I)+ PRT_FLTIR(3,I)
            FIR_CYL(I) = PRT_CYLIR(1,I)+ PRT_CYLIR(2,I)+ PRT_CYLIR(3,I)

            FVI_FLT(I) = FVI_FLT(I)*AREA(K,SIDE(K))
            FVI_CYL(I) = FVI_CYL(I)*AREA2(K,SIDE(K))
            FIR_FLT(I) = FIR_FLT(I)*AREA(K,SIDE(K))
            FIR_CYL(I) = FIR_CYL(I)*AREA2(K,SIDE(K))


            FVI(I) = (FVI_FLT(I)+FVI_CYL(I))*ABSNCFVI
            FIR(I) = (FIR_FLT(I)+FIR_CYL(I))*ABSNCFIR

            FORCE(I) = FORCE(I) + FVI(I) + FIR(I)
         ENDDO
      ENDDO

      END SUBROUTINE
