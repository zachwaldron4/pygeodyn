      SUBROUTINE VLBI_DELAY( SOU_NAM, STA1_NAM, STA2_NAM, MJD, TAI,     &
     &                    DELAY, RATE_PH, ACCL_PH,                      &
     &                    DER_DEL, DER_RAT, S_CRS, NQUAB_L,C_STA,NSTA_L,&
     &                    C_SOU, ATM_PRES,COO_TRS,VEL_CRS,UEN_TO_TRS,   &
     &                    REN_TO_TRS,ELEV,LAT_GCN,LAT_GDT,HEI_ELL,LONG, &
     &                    TRS_TO_CRS,COO_CRS,PLAN,VEL_TRS,MOUNT_TYP,    &
     &                    AXIS_OFFSET,QUANFO,ATM_TEMP,LTHERD)
! ************************************************************************
! *                                                                      *
! *   Routine VLBI_DELAY is the main routine for computation              *
! *   of the VLBI Time Delay (VTD). It computes group delay, phase delay *
! *   rate and phase acceleration as well as partial derivatives of      *
! *   delay and delay rate with respect to parameters of the modek at    *
! *   the specified moment of time, the specified pair of stations and   *
! *   the source using the model specified in the control file.          *
! *                                                                      *
! *   VLBI Time delay is defined as the difference of two intervals of   *
! *   proper time: 1) the interval of proper time of station #1 between  *
! *   events: coming the wave front to the reference point on the moving *
! *   axis and clock synchronization; 2) the interval of proper time of  *
! *   station #2 between events: coming the wave front to the reference  *
! *   point on the moving axis and clock synchronization. The time delay *
! *   is referred to the moment of coming the wave front to the          *
! *   reference point on the moving axis of the first antenna at time    *
! *   measured by the timescale TAI. The reference point of the station  *
! *   for which modeling is done is defined as the point on the moving   *
! *   axes which has the minimal distance to the fixed axis. In the      *
! *   case if axes intersect, this is the point of intersection.         *
! *                                                                      *
! *   Precision of computation of VLBI time delay is deemed to be no     *
! *   worse than 1.D-12 seconds.                                         *
! *                                                                      *
! *   Caveat: as of 2004.07.07 computation of delay rate and             *
! *           acceleration is not yet implemented. It will be            *
! *           implemented later.                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  SOU_NAM ( CHARACTER ) -- Name of the source. The source should      *
! *                           be in the input catalogue defined in the   *
! *                           control file.                              *
! * STA1_NAM ( CHARACTER ) -- Name of the first station of the baseline. *
! *                           The station should be in the input         *
! *                           catalogue defined in the control file.     *
! * STA2_NAM ( CHARACTER ) -- Name of the second station of the baseline.*
! *                           The station should be in the input         *
! *                           catalogue defined in the control file.     *
! *      MJD ( INTEGER*4 ) -- Modified Julian date of the midnight of    *
! *                           the observation.                           *
! *      TAI ( INTEGER*4 ) -- Time of the observations in seconds at     *
! *                           time scale TAI elapsed from the midnight.  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   DELAY ( REAL*8    ) -- Group delay. Units: seconds.               *
! *  RATE_PH ( REAL*8    ) -- Phase delay date. Units: dimensionless.    *
! *  ACCL_PH ( REAL*8    ) -- Phase acceleration. Units: 1/sec           *
! *  DER_DEL ( REAL*8    ) -- Vector of partial derivatives of group     *
! *                           delay with respect to parameters of the    *
! *                           model. Dimension: NDER.               *
! *  DER_RAT ( REAL*8    ) -- Vector of partial derivatives of phase     *
! *                           delay rate with respect to parameters of   *
! *                           the model. Dimension: NDER.           *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  SOU_NAM*8, STA1_NAM*8, STA2_NAM*8
      INTEGER  NDER
      PARAMETER ( NDER = 19 )
      INTEGER  MJD, NQUAB_L,IVRB
      INTEGER  CALCTRP,ITRPXM
      DOUBLE PRECISION TAI, DELAY,  RATE_PH, ACCL_PH, &
     &           DER_DEL(NDER), DER_RAT(NDER)
      DOUBLE PRECISION AXOF_CRS(3,2), AXOF_TRS(3,2),PTD_TRS(3,2), &
     &           RT_CRS(3,2),  &
     &           UP_RATE_CRS(3),AXOF_UP(2)
      INTEGER  ISTA(2), ISOU, NN, J3, IER
      DOUBLE PRECISION TAU_GEOM, RATE_GEOM, ACCL_GEOM,   &
     &           TROP_GEOM_TAU, TROP_AXOF_TAU(2)
      DOUBLE PRECISION S_CRS(3,NQUAB_L)
      CHARACTER  STA_NAM*8
      DOUBLE PRECISION UP_CRS(3), UP_UEN(3), UP_TRS(3), S_APP(3), DT, &
     &           VEC1(3), VEC2(3), VEC_PROJ_EN(3),VAL,NORTH_CRS(3), &
     &           EAST_CRS(3), E_PROJ, NORTH_UEN(3),NORTH_TRS(3)
      DOUBLE PRECISION SV, SV2, SB1, SB2
      DOUBLE PRECISION      DP_VV_V ,ATAN_CS
      LOGICAL*4  PROBE_WRITE_ADDRESS
      LOGICAL*4  LTHERD(2)
      LOGICAL*4  LMOD(2)
      EXTERNAL   ILEN
      DOUBLE PRECISION NMF_W,NMF_H,DMJD
      INTEGER  ILEN, I_LEN, STA_INDEX, SOU_INDEX
      CHARACTER  MOUNT_TYPE(2)*4,AZEL*4,EQUA*4,X_YN*4,X_YE*4,RICH*4
      INTEGER  MOUNT
      INTEGER  NSTA_L
      INTEGER  TAIST(2),TROP_AXOF_COUPLE
      CHARACTER  C_STA(NSTA_L)*8, STATION_NAM*8
      CHARACTER  C_SOU(NQUAB_L)*8
      DOUBLE PRECISION ATM_PRES(2),COO_TRS(3,2),VEL_CRS(3,2),VEL_TRS(3,2),    &
     &        UEN_TO_TRS(3,3,2),ATM_TEMP(2)
      DOUBLE PRECISION REN_TO_TRS(3,3,2),TRS_TO_CRS(3,3),COO_CRS(3,2)
      DOUBLE PRECISION   ELEV(2),LAT_GCN(2),LAT_GDT(2),HEI_ELL(2),LONG(2),AZ(2)
!SMCS
      DOUBLE PRECISION   ELEV_DER(2),DECL,THERM_DELAY(2)
      DOUBLE PRECISION TROP_HZD(2),TROP_WZD(2),TROP_DEL(2),TROP_DEL_RATE(2),  &
     &        TROP_ZEN_DER(2),TRS_TO_CRS_DER1(3,3),TRS_TO_CRS_DER2(3,3),&
     &        DTRS_TO_CRS_DEOP(3,3,3),TROP_TILT_N_DER(2),          &
     &        TROP_TILT_E_DER(2),N_PROJ
      DOUBLE PRECISION RC2Keps(3,3,2),RC2Kpsi(3,3,2)
      DOUBLE PRECISION PLAN(3,2,11),AXIS_OFFSET(2),TIME_TOL
      DOUBLE PRECISION   DELAY_AXOF_VTD(2),DELAY_AXOF_CALC(2)
      DOUBLE PRECISION RD,QUANFO(12)
      INTEGER  TROP_GEOM__COUPL,MOUNT_TYP(2)

      INTEGER  DER_E1,   DER_E2,   DER_E3,   &
     &           DER_ST1X, DER_ST1Y, DER_ST1Z, &
     &           DER_ST2X, DER_ST2Y, DER_ST2Z, &
     &           DER_RA,   DER_DL,                  &
     &           DER_AT1,  DER_AT2,                 &
     &           DER_ATN1,  DER_ATN2,               &
     &           DER_ATE1,  DER_ATE2
      INTEGER  DER_EOP(3),  DER_STA1(3),  DER_STA2(3)
      INTEGER  DER_NUT(2),  DER_DPSI   , DER_EPST
!     PARAMETER ( PI=3.141592653589793D0)

      PARAMETER ( TIME_TOL = 1.D-4 )
      PARAMETER ( DER_E1   =  1 ) ! Earth rotation Euler angle 1
      PARAMETER ( DER_E2   =  2 ) ! Earth rotation Euler angle 2
      PARAMETER ( DER_E3   =  3 ) ! Earth rotation Euler angle 3
      PARAMETER ( DER_ST1X =  4 ) ! Station 1, X coordinate
      PARAMETER ( DER_ST1Y =  5 ) ! Station 1, Y coordinate
      PARAMETER ( DER_ST1Z =  6 ) ! Station 1, Z coordinate
      PARAMETER ( DER_ST2X =  7 ) ! Station 2, X coordinate
      PARAMETER ( DER_ST2Y =  8 ) ! Station 2, Y coordinate
      PARAMETER ( DER_ST2Z =  9 ) ! Station 2, Z coordinate
      PARAMETER ( DER_RA   = 10 ) ! Right ascension
      PARAMETER ( DER_DL   = 11 ) ! Declination
      PARAMETER ( DER_AT1  = 12 ) ! Station 1, atmospheric path delay
      PARAMETER ( DER_AT2  = 13 ) ! Station 2, atmospheric path delay
      PARAMETER ( DER_ATN1 = 14 ) ! Station 1, atmospheric north tilt
      PARAMETER ( DER_ATE1 = 15 ) ! Station 1, atmospheric east  tilt
      PARAMETER ( DER_ATN2 = 16 ) ! Station 2, atmospheric north tilt
      PARAMETER ( DER_ATE2 = 17 ) ! Station 2, atmospheric east  tilt
      PARAMETER ( DER_DPSI = 18 ) ! NUTATION dpsi
      PARAMETER ( DER_EPST = 19 ) ! NUTATION epst


      DATA         DER_EOP &
     &           / &
     &             DER_E1, &
     &             DER_E2, &
     &             DER_E3  &
     &           /
      DATA         DER_NUT &
     &           / &
     &             DER_DPSI, &
     &             DER_EPST  &
     &           /
      DATA         DER_STA1  &
     &           / &
     &             DER_ST1X, &
     &             DER_ST1Y, &
     &             DER_ST1Z  &
     &           /
      DATA         DER_STA2  &
     &           / &
     &             DER_ST2X, &
     &             DER_ST2Y, &
     &             DER_ST2Z  &
     &           /
!



      INCLUDE 'COMMON_DECL.inc'
      COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
     &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
!
!     write(6,*)' db DEBUG PLAN '
!     write(6,*)PLAN(1,1,1),PLAN(2,1,1),PLAN(3,1,1)
!     write(6,*)PLAN(1,1,2),PLAN(2,1,2),PLAN(3,1,2)
!     write(6,*)PLAN(1,1,3),PLAN(2,1,3),PLAN(3,1,3)
!     write(6,*)PLAN(1,1,4),PLAN(2,1,4),PLAN(3,1,4)
!     write(6,*)PLAN(1,1,5),PLAN(2,1,5),PLAN(3,1,5)
!     write(6,*)PLAN(1,1,6),PLAN(2,1,6),PLAN(3,1,6)
!     write(6,*)PLAN(1,1,7),PLAN(2,1,7),PLAN(3,1,7)
!     write(6,*)PLAN(1,1,8),PLAN(2,1,8),PLAN(3,1,8)
!     write(6,*)PLAN(1,1,9),PLAN(2,1,9),PLAN(3,1,9)
!     write(6,*)PLAN(1,1,10),PLAN(2,1,10),PLAN(3,1,10)
!     write(6,*)PLAN(1,1,11),PLAN(2,1,11),PLAN(3,1,11)
!     write(6,*)' dbg DEBUG PLAN RATE '
!     write(6,*)PLAN(1,2,1),PLAN(2,2,1),PLAN(3,2,1)
!     write(6,*)PLAN(1,2,2),PLAN(2,2,2),PLAN(3,2,2)
!     write(6,*)PLAN(1,2,3),PLAN(2,2,3),PLAN(3,2,3)
!     write(6,*)PLAN(1,2,4),PLAN(2,2,4),PLAN(3,2,4)
!     write(6,*)PLAN(1,2,5),PLAN(2,2,5),PLAN(3,2,5)
!     write(6,*)PLAN(1,2,6),PLAN(2,2,6),PLAN(3,2,6)
!     write(6,*)PLAN(1,2,7),PLAN(2,2,7),PLAN(3,2,7)
!     write(6,*)PLAN(1,2,8),PLAN(2,2,8),PLAN(3,2,8)
!     write(6,*)PLAN(1,2,9),PLAN(2,2,9),PLAN(3,2,9)
!     write(6,*)PLAN(1,2,10),PLAN(2,2,10),PLAN(3,2,10)
!     write(6,*)PLAN(1,2,11),PLAN(2,2,11),PLAN(3,2,11)
!
! --- Compute parameters which depends only on time, but do not depend on
! --- station
      IVRB=1
      LMOD(1)=.FALSE.
      LMOD(2)=.TRUE.
      CALCTRP=1
      ITRPXM=4801
      TROP_GEOM__COUPL=1
      TROP_AXOF_COUPLE=0
!
      CALL ERM_NA     ( MJD, TAI,    &
     &                  TRS_TO_CRS, TRS_TO_CRS_DER1, &
     &                  TRS_TO_CRS_DER2,     &
     &                  DTRS_TO_CRS_DEOP, RC2Keps, RC2Kpsi,IER )

!          WRITE ( 6, 210 ) TRS_TO_CRS(1,1), &
!    &                      TRS_TO_CRS(2,1), &
!    &                      TRS_TO_CRS(3,1), &
!    &                      TRS_TO_CRS(1,2), &
!    &                      TRS_TO_CRS(2,2), &
!    &                      TRS_TO_CRS(3,2), &
!    &                      TRS_TO_CRS(1,3), &
!    &                      TRS_TO_CRS(2,3), &
!    &                      TRS_TO_CRS(3,3)
!210     FORMAT ( 3('TRS_TO_CRS = ', 3(F15.12,2X)/) )

      ISTA(1) = STA_INDEX (  STA1_NAM,C_STA,NSTA_L )
      ISTA(2) = STA_INDEX (  STA2_NAM,C_STA,NSTA_L )
      ISOU    = SOU_INDEX (  SOU_NAM,C_SOU,NQUAB_L)
!
      DO 430 J3=1,2 ! Cycle over stations: the first and the second
!
         IF ( J3 .EQ. 1 ) STA_NAM = STA1_NAM
         IF ( J3 .EQ. 2 ) STA_NAM = STA2_NAM
!
         CALL CLEARA ( AXOF_CRS(1,J3),3 )
         CALL CLEARA ( AXOF_TRS(1,J3),3 )
!
! TEMPORARY  NORMALLY GET MOUNT TYPE FROM COMMON BLOCK CSTA
!
!        MOUNT=TMOUNT
         IF(MOUNT_TYP(J3).EQ.1)MOUNT_TYPE(J3)='X_YE'
         IF(MOUNT_TYP(J3).EQ.2)MOUNT_TYPE(J3)='X_YN'
         IF(MOUNT_TYP(J3).EQ.3)MOUNT_TYPE(J3)='AZEL'
         IF(MOUNT_TYP(J3).EQ.4)MOUNT_TYPE(J3)='EQUA'
         IF(MOUNT_TYP(J3).EQ.5)MOUNT_TYPE(J3)='RICH'
         write(6,12345)MOUNT_TYPE(J3),J3
12345   format(' MOUNT_TYP 1  ',a4,I5)

         AXOF_UP = 0.0D0

!
              CALL AXOF     ( J3, ISOU, AXOF_CRS(1,J3),      &
     &                        AXOF_UP(J3), MOUNT_TYPE,S_CRS,NQUAB_L,&
     &                        ITRPXM,1,CALCTRP,UEN_TO_TRS,          &
     &                        HEI_ELL,LONG, LAT_GDT,TRS_TO_CRS,     &
     &                        PLAN,AXIS_OFFSET,DELAY_AXOF_VTD,      &
     &                        DELAY_AXOF_CALC,IER )
!
! ----------- Transform the vector of antenna axis offset from CRS to TRS
!
              CALL MUL_MV_TV_V ( 3, 3, TRS_TO_CRS, &
     &                       3, AXOF_CRS(1,J3), 3, AXOF_TRS(1,J3), -1 )
!
!             IF ( IVRB .GE. 1 ) THEN
!                  WRITE ( 6, 110 ) STA_NAM, &
!    &                              (AXOF_CRS(NN,J3), NN=1,3)
!110               FORMAT ( 1X,'Axof vector      ',A, ' (CRS): ', &
!    &                      3(F12.9,', '),' m ' )
!             END IF
!        END IF
!
!       COO_TRS(1,J3)=COO_TRS(1,J3)+AXOF_TRS(1,J3)
!       COO_TRS(2,J3)=COO_TRS(2,J3)+AXOF_TRS(2,J3)
!       COO_TRS(3,J3)=COO_TRS(3,J3)+AXOF_TRS(3,J3)
!DEP EXTRA
!        CALL MUL_MV_IV_V ( 3, 3, TRS_TO_CRS, &
!    &                         3, COO_TRS(1,J3), &
!    &                         3, COO_CRS(1,J3), -1 )
!DEP
!
!  ADD THE EFFECT OF AXOF_CRS TO THE INERTIAL COORDINATES
!       write(6,*)' dbg COO_CRS ',COO_CRS(1,J3),COO_CRS(2,J3),    &
!    &  COO_CRS(3,J3)
!     goto 999
        COO_CRS(1,J3)=COO_CRS(1,J3)+AXOF_CRS(1,J3)
        COO_CRS(2,J3)=COO_CRS(2,J3)+AXOF_CRS(2,J3)
        COO_CRS(3,J3)=COO_CRS(3,J3)+AXOF_CRS(3,J3)
!999  continue

!      write(6,*)' COO_CRS ',COO_CRS(1,J3),COO_CRS(2,J3), &
!    &  COO_CRS(3,J3),AXOF_CRS(1,J3),AXOF_CRS(2,J3),AXOF_CRS(3,J3)

! ------ Compute the vector of local zenith in CRS reference system and the
! ------ rate of its change
!
         UP_UEN(1) = 1.0D0
         UP_UEN(2) = 0.0D0
         UP_UEN(3) = 0.0D0
!
!     write(6,*)'                                                     '
!     write(6,*)' dbg UEN UP EAST NORTH  MATRIX'
!     write(6,*)'                                                     '
!     write(6,*)UEN_TO_TRS(1,1,J3), &
!    &          UEN_TO_TRS(2,1,J3), &
!    &          UEN_TO_TRS(3,1,J3)
!     write(6,*)UEN_TO_TRS(1,2,J3), &
!    &          UEN_TO_TRS(2,2,J3), &
!    &          UEN_TO_TRS(3,2,J3)
!     write(6,*)UEN_TO_TRS(1,3,J3), &
!    &          UEN_TO_TRS(2,3,J3), &
!    &          UEN_TO_TRS(3,3,J3)
!     write(6,*)'                                                     '
!     write(6,*)' dbg REN RADIAL EAST NORT MATRIX '
!     write(6,*)'                                                     '
!     write(6,*)REN_TO_TRS(1,1,J3), &
!    &          REN_TO_TRS(2,1,J3), &
!    &          REN_TO_TRS(3,1,J3)
!     write(6,*)REN_TO_TRS(1,2,J3), &
!    &          REN_TO_TRS(2,2,J3), &
!    &          REN_TO_TRS(3,2,J3)
!     write(6,*)REN_TO_TRS(1,3,J3), &
!    &          REN_TO_TRS(2,3,J3), &
!    &          REN_TO_TRS(3,3,J3)
!     write(6,*)'                                                     '
!
         CALL MUL_MV_IV_V ( 3, 3, UEN_TO_TRS(1,1,J3),  &
     &                         3, UP_UEN, 3, UP_TRS, -1 )
         CALL MUL_MV_IV_V ( 3, 3, TRS_TO_CRS, &
     &                         3, UP_TRS, 3, UP_CRS, -1 )
         CALL MUL_MV_IV_V ( 3, 3, TRS_TO_CRS_DER1, &
     &                         3, UP_TRS, 3, UP_RATE_CRS, -1 )
!
         NORTH_UEN(1) = 0.0D0
         NORTH_UEN(2) = 0.0D0
         NORTH_UEN(3) = 1.0D0
         CALL MUL_MV_IV_V ( 3, 3, UEN_TO_TRS(1,1,J3),  &
     &                         3, NORTH_UEN, 3, NORTH_TRS, IER )
         CALL MUL_MV_IV_V ( 3, 3, TRS_TO_CRS,          &
     &                         3, NORTH_TRS, 3, NORTH_CRS, IER )


      SV = DP_VV_V ( 3, S_CRS(1,ISOU), PLAN(1,2,4))
      CALL ADDC_VV ( 3, 1.0D0/VLIGHT, PLAN(1,2,4), &
     &             (1.0D0 - SV/VLIGHT), S_CRS(1,ISOU), &
     &             S_APP )
      CALL NORM_VEC ( 3, S_APP, RD )
      ELEV(J3) = ASIN ( DP_VV_V ( 3, UP_CRS, S_APP ) )
      ELEV_DER(J3) = DP_VV_V( 3, UP_RATE_CRS, S_APP)/ &
     &           SQRT ( 1.0D0 - DP_VV_V( 3, UP_RATE_CRS, S_APP)**2 )

! ------ Now let us compute azimuth. In order to do it first compute
! ------ the projection of the source vector to the horizontal plane
!
         CALL ADDC_VV ( 3, 1.0D0, S_APP, -DP_VV_V( 3, UP_CRS, S_APP ), &
     &                   UP_CRS, &
     &                  VEC_PROJ_EN )
         CALL NORM_VEC ( 3, VEC_PROJ_EN, VAL )
!
! ------ Then compute the north projection of that projection ...
!
       N_PROJ = DP_VV_V ( 3, VEC_PROJ_EN, NORTH_CRS )
!
! ------ ... and east projection of that projection.
!
       CALL VM83 ( NORTH_CRS, UP_CRS, EAST_CRS )
       E_PROJ = DP_VV_V ( 3, VEC_PROJ_EN, EAST_CRS  )
!
! ------ From these two projections we get the azimuth. Ugh!
!
        AZ(J3) = ATAN_CS ( N_PROJ, E_PROJ )


! ------ Compute troposperic path delay
!SMCS
!         write(6,*) "VLBI_DELAY: DMJD is :", DMJD
         CALL TROPDEL     ( J3, TROP_HZD(J3), &
     &                      TROP_WZD(J3), &
     &                      TROP_DEL(J3), &
     &                      TROP_DEL_RATE(J3),ATM_PRES, &
     &                      ELEV,ELEV_DER,LAT_GCN,LAT_GDT,HEI_ELL, &
     &                      MJD,TAI,IER,LONG)
!
        TROP_ZEN_DER (J3)= NMF_W ( LAT_GDT(J3), &
     &                                        ELEV(J3)  )
        TROP_TILT_N_DER(J3)= NMF_H ( MJD, TAI ,LAT_GDT(J3), &
     &                          HEI_ELL(J3),ELEV(J3))/     &
     &                          TAN(ELEV(J3))*COS(AZ(J3))
!     write(6,*)' dbg EXEC ',TROP_TILT_N_DER(J3),MJD,TAI,LAT_GDT(J3),  &
!    & HEI_ELL(J3),ELEV(J3),DTAN(ELEV(J3)),DCOS(AZ(J3))
        TROP_TILT_E_DER(J3)= NMF_H ( MJD, TAI ,LAT_GDT(J3), &
     &                          HEI_ELL(J3),ELEV(J3))/     &
     &                          TAN(ELEV(J3))*SIN(AZ(J3))
!
!        IF ( IVRB .GE. 1 ) THEN
!             WRITE ( 6, 170 ) C_STA(ISTA(J3)), &
!    &                         ELEV(J3)*180.0D0/PI, &
!    &                         TROP_HZD(J3)*1.D9, &
!    &                         TROP_DEL(J3)*1.D9
!170          FORMAT ( 1X, A, ' Elevation: ', F10.6,' deg ', &
!    &                 ' TROP_HZD = ', F9.4,' TROP_DEL = ',F9.4 )
!        END IF
!
! ------ Compute compling between troposphere path delay and antenna's
! ------ axis offset
!
         IF ( TROP_AXOF_COUPLE .EQ. 1 ) THEN
              CALL TROP_AXOF_COUPL ( AXOF_UP, &
     &                                   ( TROP_HZD(J3) +  &
     &                                     TROP_WZD(J3) ), &
     &                                     TROP_AXOF_TAU(J3) )
         END IF

      IF(LTHERD(J3)) THEN


!       write(6,*) "VLBI_DELAY: ATM_TEMP is ", ATM_TEMP(J3)
!       write(6,*) "VLBI_DELAY: DECLINATION is :", QUANFO(4)
!       write(6,*) "VLBI_DELAY: AZIMUTH is :", AZ(J3)
!       write(6,*) "VLBI_DELAY: ELEVATION is :", ELEV(J3)
!       write(6,*) "VLBI_DELAY: MJD is :", MJD
!       write(6,*) "VLBI_DELAY: TAI is :", TAI

       STATION_NAM = C_STA(ISTA(J3))
       DECL = QUANFO(4)
!       write(6,*) "VLBI_DELAY: STATION_NAME is:", STATION_NAM

      CALL ANTHERDEF(AZ(J3),ELEV(J3),DECL,ATM_TEMP(J3),MJD,TAI,   &
     &                 STATION_NAM,THERM_DELAY(J3))

!        write(6,*) "VLBI_DELAY: THERM_DELAY is :", THERM_DELAY(J3)


      ENDIF


 430  CONTINUE
!
! --- Get partial derivaties
!
      DER_DEL(DER_AT1) = -TROP_ZEN_DER(1)
      DER_DEL(DER_AT2) =  TROP_ZEN_DER(2)
      DER_DEL(DER_ATN1) = -TROP_TILT_N_DER(1)
      DER_DEL(DER_ATE1) = -TROP_TILT_E_DER(1)
      DER_DEL(DER_ATN2) =  TROP_TILT_N_DER(2)
      DER_DEL(DER_ATE2) =  TROP_TILT_E_DER(2)

!     write(6,*)'  DER_DEL(DER_AT1) ', DER_DEL(DER_AT1)
!     write(6,*)'  DER_DEL(DER_AT2) ', DER_DEL(DER_AT2)
!
! --- Compute geometrical path delay from a point-like source
!
      IF ( LMOD(1) ) THEN
           CALL KS1999 (     1, 2, ISOU, &
     &                       TRS_TO_CRS, DTRS_TO_CRS_DEOP, &
     &                       TAU_GEOM, RATE_GEOM,  &
     &                       DER_DEL(DER_EOP(1)),  &
     &                       DER_DEL(DER_STA1(1)), &
     &                       DER_DEL(DER_STA2(1)), &
     &                       DER_DEL(DER_RA),      &
     &                       DER_DEL(DER_DL), S_CRS,NQUAB_L,COO_TRS,  &
     &                       VEL_CRS,COO_CRS,PLAN,IER )
!     write(6,*)'  TAU_GEOM ', TAU_GEOM
!     write(6,*)'  RATE_GEOM ', RATE_GEOM
!     write(6,*)' DER_DEL(DER_EOP(1)) ',DER_DEL(DER_EOP(1))
!     write(6,*)' DER_DEL(DER_STA1(1)) ',DER_DEL(DER_STA1(1))
!     write(6,*)' DER_DEL(DER_STA1(2)) ',DER_DEL(DER_STA1(2))
!     write(6,*)' DER_DEL(DER_RA) ',DER_DEL(DER_RA)
!     write(6,*)' DER_DEL(DER_DL) ',DER_DEL(DER_DL)


         ELSE IF ( LMOD(2) ) THEN
           CALL PK2001      (1, 2, ISOU, &
     &                       TAU_GEOM, RATE_GEOM,  &
     &                       DER_DEL(DER_EOP(1)),  &
     &                       DER_DEL(DER_NUT(1)),  &
     &                       DER_DEL(DER_STA1(1)), &
     &                       DER_DEL(DER_STA2(1)), &
     &                       DER_DEL(DER_RA),      &
     &                       DER_DEL(DER_DL), S_CRS,NQUAB_L,COO_CRS,  &
     &                       PLAN,VEL_CRS,COO_TRS,TRS_TO_CRS,         &
     &                       DTRS_TO_CRS_DEOP, RC2Keps, RC2Kpsi,IER )
      END IF
!
! --- Compute hte compling betweenb tropospheric path delay and geometric
! --- delay
!
      IF ( TROP_GEOM__COUPL .EQ. 1 ) THEN
           CALL TROP_GEOM_COUPL (      ISOU, &
     &              TROP_DEL(2) - TROP_DEL(1), &
     &              TROP_GEOM_TAU,S_CRS,NQUAB_L,PLAN )
         ELSE
           TROP_GEOM_TAU = 0.0D0
      END IF
!
! --- Summ the terms
!
      DELAY =   TAU_GEOM    &
     &         + TROP_GEOM_TAU &
     &         - TROP_DEL(1) &
     &         + TROP_DEL(2) &
     &         - TROP_AXOF_TAU(1) &
     &         + TROP_AXOF_TAU(2) &
     &         -THERM_DELAY(1)   &
     &         +THERM_DELAY(2)
!        write(6,*)' dbg DELAY ',TAU_GEOM,TROP_GEOM_TAU,TROP_DEL(1), &
!    &    TROP_DEL(2),TROP_AXOF_TAU(1),TROP_AXOF_TAU(2)

!
      IF ( ITRPXM == CALCTRP ) THEN
           DELAY = DELAY &
     &              - DELAY_AXOF_VTD(1)  &
     &              + DELAY_AXOF_VTD(2)  &
     &              + DELAY_AXOF_CALC(1) &
     &              - DELAY_AXOF_CALC(2)
      END IF
!
      IF ( IVRB .GE. 2 ) THEN
           SV  = DP_VV_V ( 3, S_CRS(1,ISOU), PLAN(1,2,4))
           SV2 = DP_VV_V ( 3, S_CRS(1,ISOU), VEL_CRS(1,2) )
           SB1 = DP_VV_V ( 3, PLAN(1,2,4), &
     &                        COO_TRS(1,1)  )
           SB2 = DP_VV_V ( 3, PLAN(1,2,4), &
     &                        COO_TRS(1,2)  )
!
           VEC1(1) = PTD_TRS(1,1) - PTD_TRS(1,2)
           VEC1(2) = PTD_TRS(2,1) - PTD_TRS(2,2)
           VEC1(3) = PTD_TRS(3,1) - PTD_TRS(3,2)
           CALL MUL_MV_IV_V ( 3, 3, TRS_TO_CRS, 3, VEC1, 3, VEC2, -1 )
           DT  = DP_VV_V ( 3, S_CRS(1,ISOU), VEC2 )/VLIGHT/ &
     &           (1.0D0 + (SV+SV2)/VLIGHT)
!          WRITE ( 6, * ) 'Contribution due to pole tide:    DT = ',DT
!
           IF ( ITRPXM .EQ. 1  ) THEN
                VEC1(1) = AXOF_CRS(1,1) - AXOF_CRS(1,2)
                VEC1(2) = AXOF_CRS(2,1) - AXOF_CRS(2,2)
                VEC1(3) = AXOF_CRS(3,1) - AXOF_CRS(3,2)
                DT  = DP_VV_V ( 3, S_CRS(1,ISOU), VEC1 )/VLIGHT/ &
     &                          (1.0D0 + (SV+SV2)/VLIGHT)
                WRITE ( 6, 220 ) 'Contribution Axof vtd  ', DT
              ELSE
                WRITE ( 6, 220 ) 'Contribution Axof Calc ', &
     &                         DELAY_AXOF_CALC(1) - &
     &                         DELAY_AXOF_CALC (2)
 220            FORMAT ( 1X,A,1X, 1PD22.15 )
           END IF
!
!      WRITE(6,*) 'Contribution of trop_geom_coupl: =', TROP_GEOM_TAU
!          WRITE ( 6, * ) 'Contribution of trop_axof_coupl: =', &
!    &                    TROP_AXOF_TAU(2) - TROP_AXOF_TAU(1)
      END IF
!
      RETURN
      END  SUBROUTINE  VLBI_DELAY
