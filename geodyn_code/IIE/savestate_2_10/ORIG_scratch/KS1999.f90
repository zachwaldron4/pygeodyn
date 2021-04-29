      SUBROUTINE KS1999 ( ISTA1, ISTA2, ISOU, &
     &                    TRS_TO_CRS, DTRS_TO_CRS_DEOP, &
     &                    TAU_GEOM, RATE_GEOM,  TAU_DER_EOP, &
     &                    TAU_DER_STA1, TAU_DER_STA2, TAU_DER_RA, &
     &                    TAU_DER_DL, S_CRS,NQUABL,COO_TRS,VEL_CRS, &
     &                    COO_CRS,PLAN,IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_KS1999 computes VLBI time delay, its first and second  *
! *   time derivatives as well as some partial derivatgies on parameters *
! *   of the model according to expression of Kopeikin, Schaefer, 1999.  *
! *   Precision of the expression is better than 5.D-13 seconds.         *
! *   Relative precision of partial derivaties is better than 1.D-6.     *
! *   It is assumed that intermediate quantities, such as instantaneous  *
! *   site positions, coordinates of celestial bodies etc. have been     *
! *   computed beforehand and stored in the internal fields of the       *
! *   object VTD.                                                        *
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
! *   Caveat: currently RATE_GEOM is not computed.        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ISTA1 ( INTEGER*4 ) -- Index in the list of station of the first *
! *                            station of the baseline.                  *
! *     ISTA2 ( INTEGER*4 ) -- Index in the list of station of the       *
! *                            second station of the baseline.           *
! *      ISOU ( INTEGER*4 ) -- Index in the list of sources of the       *
! *                            source under consideration.               *
! * TRS_TO_CRS      ( REAL*8    ) -- The matrix of transformation from   *
! *                                  the terrestrial reference system to *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI.         *
! *                                  Dimension: 3,3.                     *
! * DTRS_TO_CRS_DEOP ( REAL*8   ) -- Array of time derivatives of the    *
! *                                  transformation matrix from the      *
! *                                  terrestrial reference system to     *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI with     *
! *                                  respect to the small vector of      *
! *                                  rotation with Euler angles E1, E2,  *
! *                                  end E3. Dimension: 3,3,3. The last  *
! *                                  dimension runs over E1, E2 and E3   *
! *                                  Euler angles.                       *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     TAU_GEOM ( REAL*8    ) -- Geometric delay. Units: seconds.       *
! *    RATE_GEOM ( REAL*8    ) -- First time derivative of geometric     *
! *                               time delay. Units: dimensionless.      *
! * TAU_DER_EOP  ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to the vector of a small       *
! *                               perturbing Earth's rotation.           *
! *                               Dimension: 3. Indexes run over Euler   *
! *                               angles of the Earth orientation.       *
! *                               Units: sec.                            *
! * TAU_DER_STA1 ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to coordinates vector of       *
! *                               station #1. Units: sec/m.              *
! * TAU_DER_STA2 ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to coordinates vector of       *
! *                               station #2. Units: sec/m.              *
! *   TAU_DER_RA ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to right ascension of the      *
! *                               observed source. Units: sec.           *
! *   TAU_DER_RA ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to declination of the observed *
! *                               source. Units: sec.                    *
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
! *  ### 01-FEB-2004   VTD_KS1999  v2.0 (c)  L. Petrov  21-SEP-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  ISTA1, ISTA2, ISOU, IUER,I
      INTEGER  NQUABL
      DOUBLE PRECISION TRS_TO_CRS(3,3), DTRS_TO_CRS_DEOP(3,3,3),  &
     &           TAU_GEOM, &
     &           RATE_GEOM,  TAU_DER_EOP(3),   &
     &           TAU_DER_STA1(3), &
     &           TAU_DER_STA2(3), TAU_DER_RA, TAU_DER_DL
      INTEGER  J1, J2
      DOUBLE PRECISION R1A(3), R2A(3), R1A_LEN, R2A_LEN, TAU_GRAV,   &
     &           TAU_GRAV_PLAN, &
     &           TRS_METRIC, B_CRS(3), COO1_BRS(3), COO2_BRS(3), &
     &           DIST1_BRS, &
     &           DIST2_BRS, DIST_SUN_EARTH, DS_DALPHA(3),  &
     &           DS_DDELTA(3), &
     &           C_FACTOR, B_TRS(3), VEC_TMP(3)
      DOUBLE PRECISION      DP_VV_V
      DOUBLE PRECISION S_CRS(3,NQUABL)
      DOUBLE PRECISION COO_TRS(3,2),VEL_CRS(3,2),GML(10),COO_CRS(3,2), &
     &       PLAN(3,2,11)
      DOUBLE PRECISION VTD__REA
      PARAMETER  ( VTD__REA     = 6378136.7D0 )       ! Earth's radius

      INCLUDE 'COMMON_DECL.inc'
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CRELAT/AER(11),GRLFC2(11),GRLFC3(11),SECCOR
!
!     write(6,*)' dbg KS AER(3) ',AER(3)
      RATE_GEOM = 0.0D0
!
! --- Computation of gravitation delay.
!
!     DO 409 J1=1, 10! define geodyn GM for planets
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
!409  CONTINUE
!     DO 409 J1=1,10
!     WRITE(6,*)' GM FOR PLANETS ',GML(J1),J1
!409  CONTINUE
!
      TAU_GRAV = 0.0D0
      DO 410 J1=1,10 ! Cycle over all planets, Sun and Moon
!
! ------ Compute COO1_BRS and COO2_BRS -- baricentric vectors of the
! ------ the first and the second station at the time of arrival of
! ------ the photon at the first station
!
         CALL ADD_VV_V ( 3, COO_CRS(1,ISTA1), &
     &                      PLAN(1,1,4), COO1_BRS )
         CALL ADD_VV_V ( 3, COO_CRS(1,ISTA2), &
     &                      PLAN(1,1,4), COO2_BRS )

         CALL SUB_VV_V ( 3, COO1_BRS, PLAN(1,1,J1), R1A )
         CALL SUB_VV_V ( 3, COO2_BRS, PLAN(1,1,J1), R2A )
         CALL NORM_VEC ( 3, R1A, R1A_LEN )
         CALL NORM_VEC ( 3, R2A, R2A_LEN )
!
!      write(6,*)' dbg ',S_CRS(1,ISOU),ISOU
!


!     write(6,*)'___________________ TAU GRAV _________________________'
!     write(6,*)'dbg COO_CRS(1,ISTA1)',COO_CRS(1,ISTA1)
!     write(6,*)'dbg COO_CRS(2,ISTA1)',COO_CRS(2,ISTA1)
!     write(6,*)'dbg COO_CRS(3,ISTA1)',COO_CRS(3,ISTA1)
!     write(6,*)'dbg COO_CRS(1,ISTA2)',COO_CRS(1,ISTA2)
!     write(6,*)'dbg COO_CRS(2,ISTA2)',COO_CRS(2,ISTA2)
!     write(6,*)'dbg COO_CRS(3,ISTA2)',COO_CRS(3,ISTA2)
!     write(6,*)'dbg COO1_BRS ',COO1_BRS
!     write(6,*)'dbg COO2_BRS ',COO2_BRS
!     write(6,*)'PLAN(1,1,4)',PLAN(1,1,4)
!     write(6,*)'PLAN(1,1,J1)',PLAN(1,1,J1)
!     write(6,*)'dbg R1A ',R1A
!     write(6,*)'dbg R2A ',R2A
!     write(6,*)'dbg R1A_LEN ',R1A_LEN
!     write(6,*)'dbg PLAN(1,2,J1) ',PLAN(1,2,J1)
!     write(6,*)'dbg R1A ',R1A
!     write(6,*)'dbg R2A ',R2A
!     write(6,*)'___________________ TAU GRAV _________________________'

         TAU_GRAV_PLAN = 2.D0*GML(J1)/VLIGHT**3*(1.0D0 + &
     &           DP_VV_V ( 3, S_CRS(1,ISOU), PLAN(1,2,J1) &
     &                   )/VLIGHT )* &
     &      LOG ( (R1A_LEN*(1.0D0 &
     &              + DP_VV_V ( 3, S_CRS(1,ISOU), R1A )  &
     &              + DP_VV_V ( 3, PLAN(1,2,J1), R1A )/VLIGHT &
     &              - DP_VV_V ( 3, S_CRS(1,ISOU), R1A )* &
     &                DP_VV_V ( 3, PLAN(1,2,J1), &
     &                             S_CRS(1,ISOU) )/VLIGHT ) &
     &             )/ &
     &             ( R2A_LEN*(1.0D0 &
     &              + DP_VV_V ( 3, S_CRS(1,ISOU), R2A ) &
     &              + DP_VV_V ( 3, PLAN(1,2,J1), R2A )/VLIGHT &
     &              - DP_VV_V ( 3, S_CRS(1,ISOU), R2A )* &
     &                DP_VV_V ( 3, PLAN(1,2,J1), &
     &                             S_CRS(1,ISOU) )/VLIGHT ) &
     &             ) &
     &           )
          TAU_GRAV = TAU_GRAV + TAU_GRAV_PLAN
 410  CONTINUE
!
      CALL SUB_VV_V ( 3, COO_CRS(1,ISTA2), COO_CRS(1,ISTA1), B_CRS )
      DIST_SUN_EARTH = SQRT (   PLAN(1,1,4)**2 &
     &                         + PLAN(2,1,4)**2 &
     &                         + PLAN(3,1,4)**2 )
!     write(6,*)'___________________ TAU GEOM ________________________'
!     write(6,*)'dbg B_CRS ',B_CRS
!     write(6,*)'dbg S_CRS(1,ISOU) ',S_CRS(1,ISOU)
!     write(6,*)'dbg VLIGHT ',VLIGHT
!     write(6,*)'dbg TRS_METRIC ',TRS_METRIC
!     write(6,*)'dbg GML(4) ',GML(4)
!     write(6,*)'dbg VTD__REA ',VTD__REA
!     write(6,*)'dbg PLAN(1,2,4) ',PLAN(1,2,4)
!     write(6,*)'dbg  VEL_CRS(1,ISTA2) ', VEL_CRS(1,ISTA2)
!     write(6,*)'dbg  TAU_GRAV ',TAU_GRAV
!     write(6,*)'___________________ TAU GEOM ________________________'

!
      TRS_METRIC=0.D0
!
! --- Compute geometric path delay
!
      TAU_GEOM = ( -DP_VV_V ( 3, B_CRS, S_CRS(1,ISOU) )/VLIGHT*    &
     &               ( 1.0D0                                       &
     &                 -2.0D0*GML(1)/DIST_SUN_EARTH/VLIGHT**2       &
     &                 -TRS_METRIC*GML(4)/VTD__REA/VLIGHT**2        &
     &                 -DP_VV_V ( 3, PLAN(1,2,4),       &
     &                               PLAN(1,2,4)        &
     &                          )/2.0D0/VLIGHT**2       &
     &                 -DP_VV_V ( 3, PLAN(1,2,4),       &
     &                            VEL_CRS(1,ISTA2)      &
     &                          )/VLIGHT**2             &
     &               )                                  &
     &             - DP_VV_V ( 3, PLAN(1,2,4), B_CRS )* &
     &               (1.0D0 + DP_VV_V ( 3, PLAN(1,2,4), &
     &                                     S_CRS(1,ISOU)      &
     &                                )/2.0D0/VLIGHT          &
     &               )/VLIGHT**2                              &
     &             + TAU_GRAV                                 &
     &            )/                                          &
     &            (1.0D0 + (  DP_VV_V ( 3, PLAN(1,2,4), &
     &                                     S_CRS(1,ISOU)     ) &
     &                      + DP_VV_V ( 3, VEL_CRS(1,ISTA2),   &
     &                                     S_CRS(1,ISOU)     ) &
     &                     )/VLIGHT                            &
     &            )
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!@        write ( 6, * ) ' vel_crs=', VEL_CRS (1,ISTA2) ! %%%%%%%%
!@        write ( 6, * ) ' vel_earth=', PLAN(1,2,4), &
!@     &                                PLAN(2,2,4), &
!@     &                                PLAN(3,2,4)
!@        write ( 6, * ) ' tau_grav=',tau_grav ! %%%
!@      call err_log ( 0, iuer ) ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!@      if ( vtd%sou(isou)%s_crs(1) .ne. -1232131.23 ) return ! %%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      C_FACTOR = 1.0D0/(                                 &
     &                     DP_VV_V ( 3, PLAN(1,2,4),     &
     &                                  S_CRS(1,ISOU) )  &
     &                   + DP_VV_V ( 3, VEL_CRS(1,ISTA2),&
     &                                  S_CRS(1,ISOU) )  &
     &                   + VLIGHT                        &
     &                 )
!
      CALL SUB_VV_V ( 3, COO_TRS(1,ISTA2), &
     &                   COO_TRS(1,ISTA1), B_TRS )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      call sub_vv_v ( 3, vtd%sta(ista2)%beg_trs, &
!     &                   vtd%sta(ista1)%beg_trs, b_trs )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      DO 420 J2=1,3
         TAU_DER_STA1(J2) =   C_FACTOR* DP_VV_V ( 3, TRS_TO_CRS(1,J2), &
     &                        S_CRS(1,ISOU) )                          &
     &                + 1.D0/VLIGHT**2* DP_VV_V ( 3, TRS_TO_CRS(1,J2), &
     &                  PLAN(1,2,4) )
         TAU_DER_STA2(J2) = - TAU_DER_STA1(J2)
!
         CALL MUL_MV_IV_V ( 3, 3, DTRS_TO_CRS_DEOP(1,1,J2), &
     &                         3, B_TRS,                    &
     &                         3, VEC_TMP, -1 )
         TAU_DER_EOP(J2)  =   C_FACTOR* DP_VV_V ( 3, S_CRS(1,ISOU),    &
     &                                            VEC_TMP )            &
     &                      + C_FACTOR/VLIGHT*                         &
     &                        DP_VV_V ( 3, PLAN(1,2,4), &
     &                                     VEC_TMP )
 420  CONTINUE
!
      DS_DALPHA(1) = -S_CRS(2,ISOU)
      DS_DALPHA(2) =  S_CRS(1,ISOU)
      DS_DALPHA(3) =  0.0D0
!
      DS_DDELTA(1) = -S_CRS(1,ISOU)*S_CRS(3,ISOU)/ &
     &                SQRT( 1.0D0 - S_CRS(3,ISOU)**2 )
      DS_DDELTA(2) = -S_CRS(2,ISOU)*S_CRS(3,ISOU)/ &
     &                SQRT( 1.0D0 - S_CRS(3,ISOU)**2 )
      DS_DDELTA(3) =  SQRT( 1.0D0 - S_CRS(3,ISOU)**2 )
!
      TAU_DER_RA = - C_FACTOR    * DP_VV_V ( 3, B_CRS, DS_DALPHA )  &
     &             + C_FACTOR**2 * DP_VV_V ( 3, B_CRS, S_CRS(1,ISOU))*&
     &      (                                             &
     &          DP_VV_V ( 3, PLAN(1,2,4), DS_DALPHA )     &
     &        + DP_VV_V ( 3, VEL_CRS(1,ISTA2), DS_DALPHA )       &
     &      )
!
      TAU_DER_DL = - C_FACTOR    * DP_VV_V ( 3, B_CRS, DS_DDELTA )  &
     &             + C_FACTOR**2 * DP_VV_V ( 3, B_CRS, S_CRS(1,ISOU))*&
     &      (                                             &
     &          DP_VV_V ( 3, PLAN(1,2,4), DS_DDELTA )     &
     &        + DP_VV_V ( 3, VEL_CRS(1,ISTA2), DS_DDELTA )       &
     &      )
!
      RETURN
      END  SUBROUTINE  KS1999
