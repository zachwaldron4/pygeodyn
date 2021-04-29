      SUBROUTINE TROP_AXOF_COUPL ( AXOF_UP, TROP_DEL, TROP_AXOF_TAU )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_TROP_AXOF_COUPL  computes an additional time delay    *
! *   which stems from the coupling between troposphere path delay and   *
! *   antenna axis offset delay. It is zero for antennas with azimuthal  *
! *   mounting.                                                          *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   AXOF_UP ( REAL*8    ) -- Displacement of the point on the moving   *
! *                            antenna's axis in vertical direction.     *
! *                            This point is used for modeling geometric *
! *                            path delay. Units: meters.                *
! *  TROP_DEL ( REAL*8    ) -- Troposphere path delay for this antenna.  *
! *                            Units: seconds.                           *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  TROP_AXOF_TAU ( REAL*8    ) -- Additional time delay due to         *
! *                                 coupling between the antenna's axis  *
! *                                 offset and troposphere path delay.   *
! *                                 Units: seconds.                      *
! *                                                                      *
! * ## 25-JUN-2004 VTD_TROP_AXOF_COUPL  v1.0(c) L. Petrov 25-JUN-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      DOUBLE PRECISION AXOF_UP, TROP_DEL, TROP_AXOF_TAU
      INTEGER  ISOU
      DOUBLE PRECISION      DP_VV_V
!
      TROP_AXOF_TAU = -1.1859D-4 * AXOF_UP * TROP_DEL
!
      RETURN
      END  SUBROUTINE TROP_AXOF_COUPL
