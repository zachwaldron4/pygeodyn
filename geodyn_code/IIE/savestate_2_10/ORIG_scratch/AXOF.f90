!$AXOF
      SUBROUTINE AXOF ( ISTA, ISOU, AXOF_CRS, AXOF_UP, TYP,           &
     &    S_CRS,NQUABL,ITRPXM,VTD__YES,CALCTRP,UEN_TO_TRS,            &
     &    HEI_ELL,LONG,LAT_GDT,TRS_TO_CRS,PLAN,AXIS_OFFSET,           &
     &    DELAY_AXOF_VTD,DELAY_AXOF_CALC,IUER)
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_AXOF  computes the vector of the antenna axis offset. *
! *   The vector of antenna's axis offset is determine as the vector     *
! *   originating from the point on the fixed antenna's axis to the      *
! *   point on the moving antennas axis along the line which is          *
! *   orthogonal to both axes. The point on the moving axis where the    *
! *   vector of the antenna's offset ends is the physical point for      *
! *   which geometrical pat delay is computed.                           *
! *                                                                      *
! *   This module supports two modes for computation of antenna axis     *
! *   offset: intrinsic and mode compatibility with bug in Calc versions *
! *   1994-2004. If VTD%CONF%AXOF_MODEL == VTD__YES, then intrinsic      *
! *   mode is used. If VTD%CONF%AXOF_MODEL == CALCTRP  , then in         *
! *   addition to computation of the vector of antenna axis offset       *
! *   AXOF_CTS and progection of this vector to local zenith AXOF_UP     *
! *   two quantities are computed:                                       *
! *   VTD%STA(ISTA)%DELAY_AXOF_VTD  -- contribution to delay due to      *
! *                                    antenna axis offset according to  *
! *                                    intrinsic formula;                *
! *   VTD%STA(ISTA)%DELAY_AXOF_CALC -- contribution to delay due to      *
! *                                    antenna axis offset in a mode of  *
! *                                    compatibility with bug in Calc.   *
! *                                                                      *
! *   Keep in mind: AXOF_CRS and AXOF_UP are always computed according   *
! *   to instrinsic formula, which is considered as the only correct     *
! *   way of computations.                                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     ISTA ( INTEGER*4 ) -- Index of the station in the VTD station    *
! *                           list.                                      *
! *     ISOU ( INTEGER*4 ) -- Index of the source in the VTD source list.*
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  AXOF_CRS ( REAL*8    ) -- Antenna's axis offset vector in the       *
! *                            celestial reference system.               *
! *                            Dimension: 3. Units: meters.              *
! *   AXOF_UP ( REAL*8    ) -- Displacement of the point on the moving   *
! *                            antenna's axis in vertical direction.     *
! *                            This point is used for modeling geometric *
! *                            path delay. Units: meters.                *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 28-JAN-2004     VTD_AXOF  v2.0 (c)  L. Petrov  13-OCT-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  ISTA, ISOU, IUER
      INTEGER  NQUABL,ITRPXM,VTD__YES,CALCTRP
      DOUBLE PRECISION AXOF_CRS(3), AXOF_UP
      DOUBLE PRECISION AVEC_REN(3), AVEC_TRS(3), AVEC_CRS(3), LAT, LNG, &
     &           RZ_TRS(3), RZ_CRS(3), AXOF_TRS(3)
      DOUBLE PRECISION VEC_TMP(3), SV, SP(3), SAPP(3), CTG_E, RN
      INTEGER  J1
      DOUBLE PRECISION      EL_ABR, BEND_ANG, S_ABBR(3), AXOF_CRS_CALC(3), N_AIR
      DOUBLE PRECISION      DP_VV_V, SBEND_CALC
      DOUBLE PRECISION S_CRS(3,NQUABL),UEN_TO_TRS(3,3,2),PLAN(3,2,11)
      DOUBLE PRECISION HEI_ELL(2),LONG(2),LAT_GDT(2),TRS_TO_CRS(3,3),   &
     &           AXIS_OFFSET(2)
      DOUBLE PRECISION      DELAY_AXOF_VTD(2),DELAY_AXOF_CALC(2)
      DOUBLE PRECISION RFR
      PARAMETER  ( RFR = 3.13D-4 ) ! Refractivity parameter


      CHARACTER TYP(2)*4
      INCLUDE 'COMMON_DECL.inc'
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE

!
! --- Determine the unit vector of the fixed antenna's axis in local UEN
! --- coordinate system. It depends on mounting type
!
!     write(6,12123)TYP(ISTA)
!12123 format('AXOFF ',A4)
      IF ( TYP(ISTA)  .EQ. 'AZEL' ) THEN
!
! -------- Azimuthal mounting
!
           AVEC_REN(1) = 1.0D0
           AVEC_REN(2) = 0.0D0
           AVEC_REN(3) = 0.0D0

           CALL MUL_MV_IV_V ( 3, 3, UEN_TO_TRS(1,1,ISTA), 3, AVEC_REN, &
     &                        3, AVEC_TRS, -1 )
        ELSE IF ( TYP(ISTA) .EQ. 'EQUA' ) THEN
!
! -------- Equatorial mounting
!
           AVEC_TRS(1) = 0.0D0
           AVEC_TRS(2) = 0.0D0
           AVEC_TRS(3) = 1.0D0
        ELSE IF ( TYP(ISTA) .EQ. 'X_YN' ) THEN
!
! -------- X-Y mounting in North direction
!
           AVEC_REN(1) = 0.0D0
           AVEC_REN(2) = 0.0D0
           AVEC_REN(3) = 1.0D0
           CALL MUL_MV_IV_V ( 3, 3, UEN_TO_TRS(1,1,ISTA), 3, AVEC_REN, &
     &                        3, AVEC_TRS, -1 )
        ELSE IF ( TYP(ISTA) .EQ. 'X_YE' ) THEN
!
! -------- X-Y mounting in East direction
!
           AVEC_REN(1) = 0.0D0
           AVEC_REN(2) = 1.0D0
           AVEC_REN(3) = 0.0D0
           CALL MUL_MV_IV_V ( 3, 3, UEN_TO_TRS(1,1,ISTA), 3, AVEC_REN, &
     &                        3, AVEC_TRS, -1 )
        ELSE IF ( TYP(ISTA) .EQ. 'RICH' ) THEN
!
! -------- Special case of Richmond antenna
!
           CALL GR_TAT ( '+39_03_36.0', LAT, -1 )
           CALL GR_TAT ( '-00_07_12.0', LNG, -1 )
           AVEC_REN(1) = COS(LAT)*COS(LNG)
           AVEC_REN(2) = COS(LAT)*SIN(LNG)
           AVEC_REN(3) = SIN(LAT)
           CALL MUL_MV_IV_V ( 3, 3, UEN_TO_TRS(1,1,ISTA), 3, AVEC_REN, &
     &                        3, AVEC_TRS, -1 )
      END IF
!
! --- Rotate the vector of anntenna's fixed axis to the celestial reference
! --- system
!
      CALL MUL_MV_IV_V ( 3, 3, TRS_TO_CRS, 3, AVEC_TRS, &
     &                      3, AVEC_CRS, -1 )
!
! --- Compute the vector of local zenith in local UEN system
!
      RZ_TRS(1) =  COS(LAT_GDT(ISTA))*COS(LONG(ISTA))
      RZ_TRS(2) =  COS(LAT_GDT(ISTA))*SIN(LONG(ISTA))
      RZ_TRS(3) =  SIN(LAT_GDT(ISTA))
!
! --- ... and then rotate it to the celestial reference system
!
      CALL MUL_MV_IV_V ( 3, 3, TRS_TO_CRS, 3, RZ_TRS, 3, RZ_CRS, -1 )
!
! --- Compute intermediate vectors for refraction computation
!
      CALL VM83 ( S_CRS(1,ISOU), RZ_CRS,  VEC_TMP )
      CALL VM83 ( VEC_TMP, S_CRS(1,ISOU), SP      )
      CALL NORM_VEC ( 3, SP, RN )
!
! --- CTG_E -- cotangent of elevation
!
      CTG_E = SQRT ( 1.0D0 -    &
     &        DP_VV_V ( 3, RZ_CRS, S_CRS(1,ISOU) )**2 )/ &
     &        DP_VV_V ( 3, RZ_CRS, S_CRS (1,ISOU))
      SV = DP_VV_V ( 3, S_CRS(1,ISOU), PLAN(1,2,4) )
!
! --- Compute SAPP -- apparent vector on the source corrected for annual
! --- aberration and refraction
!
      DO 410 J1=1,3
         SAPP(J1) =   PLAN(1,2,4)/VLIGHT            &
     &              - SV*S_CRS(J1,ISOU)/VLIGHT                    &
     &              + COS ( RFR*CTG_E )*S_CRS(J1,ISOU) &
     &              + SIN ( RFR*CTG_E )*SP(J1)
 410  CONTINUE
!
! --- Finally compute antenna's axis offset
!
      CALL VM83 ( SAPP, AVEC_CRS, VEC_TMP )
      CALL VM83 ( AVEC_CRS, VEC_TMP, AXOF_CRS )
      CALL NORM_VEC ( 3, AXOF_CRS, RN )
      CALL MUL_VC_V ( 3, AXOF_CRS, AXIS_OFFSET(ISTA) )
!
      IF ( ITRPXM == CALCTRP ) THEN
!
! -------- Special trick for compatibility with a bug in Calc
!
! -------- Store contribution to delay due to antenna axis offset at this
! -------- station according to correct model
!
           DELAY_AXOF_VTD(ISTA) = &
     &             DP_VV_V ( 3, AXOF_CRS, S_CRS(1,ISOU) )/VLIGHT/ &
     &             (1.0D0 + SV/VLIGHT)
!
! -------- Compute aberrate unit source vector (S_ABRR)
!
           CALL ADDC_VV ( 3, 1.D0/VLIGHT, PLAN(1,2,4), &
     &                   -SV/VLIGHT, S_CRS(1,ISOU), S_ABBR )
           CALL ADD_VV   ( 3, S_ABBR, S_CRS(1,ISOU) )
           CALL NORM_VEC ( 3, S_ABBR, RN )
!
! -------- Compute the elecation of the abberated source
!
           EL_ABR = ASIN ( DP_VV_V ( 3, S_ABBR, RZ_CRS ) )
!
! -------- Comppute bending angle due to refraction
!
           BEND_ANG = SBEND_CALC ( &
     &           EL_ABR, &
     &           293.15D0 - 6.5D-3*HEI_ELL(ISTA), &
     &           0.5D0, &
     &           760.0D0*(1.D0 - 6.5D-3*HEI_ELL(ISTA)/293.15D0)**5.26D0 )
!
! ------- Compute apparent source position vector whcih takes into account
! ------- both annual aberration and refraction (SAPP)
!
          CALL ADDC_VV ( 3, COS ( BEND_ANG ), S_ABBR, &
     &                      SIN ( BEND_ANG ), SP,     SAPP )
!
! ------- Compute vector an antenna offsetin CRS (AXOF_CRS_CALC)
!
          CALL VM83 ( SAPP, AVEC_CRS, VEC_TMP )
          CALL VM83 ( AVEC_CRS, VEC_TMP, AXOF_CRS_CALC )
          CALL NORM_VEC ( 3, AXOF_CRS_CALC, RN )
          CALL MUL_VC_V ( 3, AXOF_CRS_CALC, AXIS_OFFSET(ISTA) )
!
! ------- Compute air index of refraction
!
          N_AIR = 77.6D-6*1013.25D0* &
     &            (1.D0 - 6.5D-3*HEI_ELL(ISTA)/293.15D0)**5.26D0/ &
     &            (293.15D0 - 6.5D-3*HEI_ELL(ISTA)) + 1.D0
!
! ------- Finally compute contribution to delay due to antenna axis offset
!
          DELAY_AXOF_CALC(ISTA) = DP_VV_V ( 3, AXOF_CRS_CALC, SAPP )/ &
     &                                              VLIGHT*N_AIR
      END IF
!
! --- Now compute projection of the anntenna's axis offset into local zenith
! --- This will be needed later for computing a coupling term for geometric
! --- delay
!
      IF ( TYP(ISTA) .EQ. 'AZEL' ) THEN
           AXOF_UP = 0.0D0 ! of course, for azimuthal mounting
         ELSE
           CALL MUL_MV_TV_V  ( 3, 3, TRS_TO_CRS, 3, AXOF_CRS, &
     &                         3, AXOF_TRS, -1 )
           AXOF_UP = DP_VV_V ( 3, AXOF_TRS, RZ_TRS )
      END IF
!
      RETURN
      END
