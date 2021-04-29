      SUBROUTINE PK2001 (     ISTA1, ISTA2, ISOU, TAU_GEOM, RATE_GEOM, &
     &                        TAU_DER_EOP, TAU_DER_NUT,TAU_DER_STA1, &
     &                        TAU_DER_STA2, TAU_DER_RA, TAU_DER_DL, &
     &                        S_CRS,NQUABL,COO_CRS,PLAN,VEL_CRS,COO_TRS,&
     &                        TRS_TO_CRS,DTRS_TO_CRS_DEOP, RC2Keps,    &
     &                        RC2Kpsi,IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_PK2001  computes VLBI time delay, its first and second *
! *   time derivative according to an algorithm presented in memo        *
! *   Petrov and Kopeikin 2001. Precision of the expression is better    *
! *   than 5.D-13 seconds. is assumed that intermediate quantities, such *
! *   as instantaneous site positions, coordinates of celestial bodies   *
! *   etc. have been computed beforehand and stored in the internal      *
! *   fields of the object VTD.                                          *
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
! *   axis which has the minimal distance to the fixed axis. In the      *
! *   case if axes intersect, this is the point of intersection.         *
! *                                                                      *
! *   References:                                                        *
! *   1) S.M. Kopeikin, G. Schaefer, Lorenz covariant theory of light    *
! *      propagation in gravitational field of arbitrary-moving bodies,  *
! *      Physical Review D, vol. 60, 12402, 1999.                        *
! *                                                                      *
! *   Caveat: currently RATE_GEOM is  not computed.        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ISTA1 ( INTEGER*4 ) -- Index in the list of station of the first *
! *                            station of the baseline.                  *
! *     ISTA2 ( INTEGER*4 ) -- Index in the list of station of the       *
! *                            second station of the baseline.           *
! *      ISOU ( INTEGER*4 ) -- Index in the list of sources of the       *
! *                            source under consideration.               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  TAU_GEOM ( REAL*8    ) -- Geometric delay. Units: seconds.          *
! * RATE_GEOM ( REAL*8    ) -- First time derivative of geometric time  *
! *                            delay. Units: dimensionless.              *
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
! *  ### 29-JAN-2004   VTD_PK2001  v2.0 (c)  L. Petrov  20-SEP-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  ISTA1, ISTA2, ISOU, IUER, IER
      INTEGER  NQUABL
      DOUBLE PRECISION TAU_GEOM, RATE_GEOM,  &
     &           TAU_DER_EOP(3), TAU_DER_NUT(2),TAU_DER_STA1(3), &
     &           TAU_DER_STA2(3), TAU_DER_RA, TAU_DER_DL
      DOUBLE PRECISION pBL_eps(3,2),pBL_psi(3,2),PEPST(2),PDPSI(2)
      INTEGER  J1,J2
      DOUBLE PRECISION R1A(3), R2A(3), R1A_LEN, R2A_LEN, &
     &           TAU_GRAV, TRS_METRIC, &
     &           B_CRS(3), V_CRS(3), COO1_BRS(3), COO2_BRS(3),  &
     &           DIST1_BRS, DIST2_BRS, &
     &           COO_PLAN(3), VEL_PLAN(3), DIST_SUN_EARTH, &
     &           TAU_RETR1, &
     &           TAU_RETR2
      DOUBLE PRECISION TAU_GRAV_PK, TAU_GRAV_0, TAU_GRAV_CNS
      DOUBLE PRECISION VEL1_BRS(3), VEL2_BRS(3), ACC_PLAN(3),V1A(3),V2A(3)
      DOUBLE PRECISION      DP_VV_V,RAT_GRAV, B_TRS(3)
      DOUBLE PRECISION S_CRS(3,NQUABL),COO_CRS(3,2),PLAN(3,2,11),VEL_CRS(3,2)
      DOUBLE PRECISION VEC_TMP(3)
      DOUBLE PRECISION COO_TRS(3,2), TRS_TO_CRS(3,3),DTRS_TO_CRS_DEOP(3,3,3)
      DOUBLE PRECISION      DS_DALPHA(3), DS_DDELTA(3),C_FACTOR
      DOUBLE PRECISION GML(10)
      DOUBLE PRECISION RC2Keps(3,3,2), RC2Kpsi(3,3,2)
      DOUBLE PRECISION VTD__REA
      PARAMETER  ( VTD__REA     = 6378136.7D0 )       ! Earth's radius

      INCLUDE 'COMMON_DECL.inc'
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE

!
      TAU_GRAV = 0.0D0
      RAT_GRAV = 0.0D0

      GML(1)=BDSTAT(7,8)
      GML(2)=BDSTAT(7,10)
      GML(3)=BDSTAT(7,1)
      GML(4)=BDSTAT(7,9)
      GML(5)=BDSTAT(7,2)
      GML(6)=BDSTAT(7,3)
      GML(7)=BDSTAT(7,4)
      GML(8)=BDSTAT(7,5)
      GML(9)=BDSTAT(7,6)
      GML(10)=BDSTAT(7,7)

      DO 410 J1=1,10
!
! ------ Compute COO1_BRS and COO2_BRS -- baricentric vectors of the
! ------ the first and the second station at the time of arrival of
! ------ the photon at the first station
!
         CALL ADD_VV_V ( 3, COO_CRS(1,ISTA1), &
     &                      PLAN(1,1,4), COO1_BRS )
         CALL ADD_VV_V ( 3, COO_CRS(1,ISTA2), &
     &                      PLAN(1,1,4), COO2_BRS )
         DIST1_BRS = SQRT  &
     &   ( COO1_BRS(1)**2 + COO1_BRS(2)**2 + COO1_BRS(3)**2 )
         DIST2_BRS = SQRT  &
     &   ( COO2_BRS(1)**2 + COO2_BRS(2)**2 + COO2_BRS(3)**2 )
!
! ------ Get cordinates of the planet at the moment of time of arrival of
! ------ the photon at the station
!
         CALL COPY_R8  ( 3, PLAN(1,1,J1), COO_PLAN )
         CALL COPY_R8  ( 3, PLAN(1,2,J1), VEL_PLAN )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!           if ( j1 .eq.1 ) then  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!                write ( 6,*) 'sun: ', &                              ! %%%
!     &             coo_plan(1)-vtd%mom%plan(1,vtd__coo,vtd__eart), & ! %%%
!     &             coo_plan(2)-vtd%mom%plan(2,vtd__coo,vtd__eart), & ! %%%
!     &             coo_plan(3)-vtd%mom%plan(3,vtd__coo,vtd__eart)    ! %%%
!           endif ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Solve light cone eqation by iteration
!
         CALL SUB_VV_V ( 3, COO1_BRS, COO_PLAN, R1A )
         CALL SUB_VV_V ( 3, COO2_BRS, COO_PLAN, R2A )
         CALL NORM_VEC ( 3, R1A, R1A_LEN )
         CALL NORM_VEC ( 3, R2A, R2A_LEN )
         TAU_RETR1 = -R1A_LEN/VLIGHT ! Get time retardation
         TAU_RETR2 = -R2A_LEN/VLIGHT
!
! ------ Compute position of the gravitating body at retarded moment of time
!
         CALL ADDC_VV  ( 3, 1.0D0, PLAN(1,1,J1), TAU_RETR1, &
     &                   VEL_PLAN, COO_PLAN )
         CALL SUB_VV_V ( 3, COO1_BRS, COO_PLAN, R1A )
         CALL NORM_VEC ( 3, R1A, R1A_LEN )
!
         CALL ADDC_VV  ( 3, 1.0D0, PLAN(1,1,J1), TAU_RETR2, &
     &                   VEL_PLAN, COO_PLAN )
         CALL SUB_VV_V ( 3, COO2_BRS, COO_PLAN, R2A )
         CALL NORM_VEC ( 3, R2A, R2A_LEN )
!
         TAU_GRAV = TAU_GRAV + &
     &           2.D0*GML(J1)/VLIGHT**3*(1.0D0 + &
     &           DP_VV_V ( 3, S_CRS(1,ISOU), PLAN(1,2,J1) &
     &                   )/ VLIGHT)* &
     &      LOG ( (R1A_LEN*(1.0D0 + &
     &      DP_VV_V ( 3, S_CRS(1,ISOU), R1A)))/ &
     &      (R2A_LEN*(1.0D0 + DP_VV_V ( 3, S_CRS(1,ISOU), R2A)))  )

!
! ----------- Compute VEL1_BRS and VEL2_BRS -- baricentric vectors of the
! ----------- the first and the second station at the time of arrival of
! ----------- the photon at the first station
!
              CALL ADD_VV_V ( 3, VEL_CRS(1,ISTA1), &
     &                        PLAN(1,2,4), VEL1_BRS )
              CALL ADD_VV_V ( 3, VEL_CRS(1,ISTA2), &
     &                        PLAN(1,2,4), VEL2_BRS )

!
! ----------- Compute velocity of the gravitating body at retarded
! ----------- moment of time
!

      CALL ADDC_VV  ( 3, 1.0D0, PLAN(1,2,J1), &
     &                        TAU_RETR1, ACC_PLAN, VEL_PLAN )
              CALL SUB_VV_V ( 3, VEL1_BRS, VEL_PLAN, V1A )
!
      CALL ADDC_VV  ( 3, 1.0D0, PLAN(1,2,J1), &
     &                        TAU_RETR2, ACC_PLAN, VEL_PLAN )
              CALL SUB_VV_V ( 3, VEL2_BRS, VEL_PLAN, V2A )

              RAT_GRAV = RAT_GRAV + &
     &          2.D0*GML(J1)/VLIGHT**3* &
     &          ( DP_VV_V ( 3, S_CRS(1,ISOU), PLAN(1,2,J1)      &
     &                    )/ VLIGHT * &
     &    LOG ( (R1A_LEN*(1.0D0 + DP_VV_V( 3, S_CRS(1,ISOU),R1A)))/  &
     &           (R2A_LEN*(1.0D0 + DP_VV_V( 3, S_CRS(1,ISOU),R2A))) ) &
     &           + &
     &          (1.0D0 + &
     &           DP_VV_V ( 3, S_CRS(1,ISOU), PLAN(1,2,J1))/VLIGHT) *  &
     &           ( ( DP_VV_V ( 3, V1A, R1A ) + &
     &               DP_VV_V ( 3, V1A, S_CRS(1,ISOU) ) &
     &             )/ ( R1A_LEN*(1.0D0 + DP_VV_V( 3, S_CRS(1,ISOU),   &
     &             R1A))) &
     &             - &
     &             ( DP_VV_V ( 3, V2A, R2A ) + &
     &               DP_VV_V ( 3, V2A, S_CRS(1,ISOU) ) &
     &             )/ ( R2A_LEN*(1.0D0 + DP_VV_V( 3, S_CRS(1,ISOU),   &
     &             R2A))) &
     &           ) &
     &        )



! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!          if ( j1 .eq. 1 ) write (6,*) ' Sun grav_del: ', tau_grav ! %%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 410  CONTINUE
!
      CALL SUB_VV_V ( 3, COO_CRS(1,ISTA2), COO_CRS(1,ISTA1), B_CRS )
      CALL SUB_VV_V ( 3, COO_CRS(1,ISTA2), VEL_CRS(1,ISTA1), V_CRS )
      DIST_SUN_EARTH = SQRT (   PLAN(1,1,4)**2 &
     &                         + PLAN(2,1,4)**2 &
     &                         + PLAN(3,1,4)**2 )
           TRS_METRIC = 2.0D0
!
! --- Compute geometric path delay
!
      TAU_GEOM = ( -DP_VV_V ( 3, B_CRS, S_CRS(1,ISOU) )/VLIGHT* &
     &         ( 1.0D0                                                &
     &          -2.0D0*GML(1)/DIST_SUN_EARTH/VLIGHT**2 &
     &          -TRS_METRIC*GML(4)/VTD__REA/VLIGHT**2 &
     &          -DP_VV_V ( 3, PLAN(1,2,4), &
     &                        PLAN(1,2,4)  )/2.0D0/VLIGHT**2 &
     &          -DP_VV_V ( 3, PLAN(1,2,4), &
     &                        VEL_CRS(1,ISTA2) )/VLIGHT**2  &
     &         ) &
     &       - DP_VV_V ( 3, PLAN(1,2,4), B_CRS )*    &
     &            (1.0D0 + DP_VV_V ( 3, PLAN(1,2,4), &
     &                 S_CRS(1,ISOU))/2.0D0/VLIGHT   &
     &      )/ VLIGHT**2                             &
     &       + TAU_GRAV )/ &
     &       (1.0D0 + (   DP_VV_V ( 3, PLAN(1,2,4),  &
     &                S_CRS (1,ISOU)              )  &
     & + DP_VV_V ( 3, VEL_CRS(1,ISTA2),              &
     &                S_CRS(1,ISOU)               )  &
     &                )/VLIGHT  &
     &       )
!
!
! --- Compute derivatives with respect to EOP
!
!
      C_FACTOR = 1.0D0/(                                     &
     &      DP_VV_V ( 3, PLAN(1,2,4),                        &
     &                   S_CRS(1,ISOU) )                     &
     &    + DP_VV_V ( 3, VEL_CRS(1,ISTA2),                   &
     &                   S_CRS(1,ISOU) )                     &
     &    + VLIGHT                                           &
     &                 )

      CALL SUB_VV_V ( 3, COO_TRS(1,ISTA2), &
     &                   COO_TRS(1,ISTA1), B_TRS )


      DO 420 J2=1,3
      TAU_DER_STA1(J2) =   C_FACTOR* DP_VV_V ( 3, TRS_TO_CRS(1,J2), &
     &                     S_CRS(1,ISOU) )                          &
     &             + 1.D0/VLIGHT**2* DP_VV_V ( 3, TRS_TO_CRS(1,J2), &
     &                     PLAN(1,2,4) )
       TAU_DER_STA2(J2) = - TAU_DER_STA1(J2)
!
      CALL MUL_MV_IV_V ( 3, 3, DTRS_TO_CRS_DEOP(1,1,J2), &
     &                         3, B_TRS,                    &
     &                         3, VEC_TMP, IER )
      TAU_DER_EOP(J2)  = - C_FACTOR* DP_VV_V ( 3, S_CRS(1,ISOU),    &
     &                                 VEC_TMP )                    &
     &                  - C_FACTOR/VLIGHT*                          &
     &                    DP_VV_V ( 3, PLAN(1,2,4),VEC_TMP)
 420  CONTINUE

! *DEF* FORM NUTATION PARTIALS

      DO 430 J2=1,2
      CALL MUL_MV_TV_V(3,3,RC2Keps(1,1,J2),3,B_CRS,3,pBL_eps(1,J2),&
     &                 IER)
      CALL DOTPRD(pBL_eps(1,J2),S_CRS,PEPST(J2),3,3,1,1)

      CALL MUL_MV_TV_V(3,3,RC2Kpsi(1,1,J2),3,B_CRS,3,pBL_psi(1,J2),&
     &                 IER)
      CALL DOTPRD(pBL_psi(1,J2),S_CRS,PDPSI(J2),3,3,1,1)
 430  CONTINUE
      TAU_DER_NUT(1)=PDPSI(1)/VLIGHT
      TAU_DER_NUT(2)=PEPST(1)/VLIGHT
!     write(6,*)'dbg PK partials ',TAU_DER_NUT(1),TAU_DER_NUT(2)
!
      DS_DALPHA(1) = -S_CRS(2,ISOU)
      DS_DALPHA(2) =  S_CRS(1,ISOU)
      DS_DALPHA(3) =  0.0D0

!
      DS_DDELTA(1) = -S_CRS(1,ISOU)*S_CRS(3,ISOU)/                   &
     &                SQRT( 1.0D0 - S_CRS(3,ISOU)**2 )
      DS_DDELTA(2) = -S_CRS(2,ISOU)*S_CRS(3,ISOU)/                   &
     &                SQRT( 1.0D0 - S_CRS(3,ISOU)**2 )
      DS_DDELTA(3) =  SQRT( 1.0D0 - S_CRS(3,ISOU)**2 )
!
      TAU_DER_RA =-C_FACTOR    * DP_VV_V ( 3, B_CRS, DS_DALPHA )     &
     &            +C_FACTOR**2 * DP_VV_V ( 3, B_CRS, S_CRS(1,ISOU))* &
     &      (                                                        &
     &          DP_VV_V ( 3, PLAN(1,2,4), DS_DALPHA )                &
     &        + DP_VV_V ( 3, VEL_CRS(1,ISTA2), DS_DALPHA )           &
     &      )
!
      TAU_DER_DL =-C_FACTOR    * DP_VV_V ( 3, B_CRS, DS_DDELTA )     &
     &            +C_FACTOR**2 * DP_VV_V ( 3, B_CRS, S_CRS(1,ISOU))* &
     &      (                                                        &
     &          DP_VV_V ( 3, PLAN(1,2,4), DS_DDELTA )                &
     &        + DP_VV_V ( 3, VEL_CRS(1,ISTA2), DS_DDELTA )           &
     &      )


!
      RETURN
      END  SUBROUTINE  PK2001
