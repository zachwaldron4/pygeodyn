
      SUBROUTINE TROP_GEOM_COUPL (  ISOU, TROP_DEL, TROP_GEOM_TAU,&
     &     S_CRS,NQUABL,PLAN )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_TROP_GEOM_COUPL  computes an additional time delay    *
! *   which stems from the coupling between the troposphere path delay   *
! *   and the geometrical delay.                                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     ISOU ( INTEGER*4 ) -- Index of the source in the VTD source list.*
! * TROP_DEL ( REAL*8    ) -- Total path delay in the direction to the   *
! *                           source. Units: seconds.                    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  TROP_GEOM_TAU ( REAL*8    ) -- Additional time delay due to         *
! *                                 coupling between the geometrical     *
! *                                 delay and the troposphere path delay.*
! *                                 Units: seconds.                      *
! *                                                                      *
! * ## 25-JUN-2004 VTD_TROP_GEOM_COUPL  v1.0(c) L. Petrov 25-JUN-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      DOUBLE PRECISION TROP_DEL, TROP_GEOM_TAU
      INTEGER  ISOU,NQUABL
      DOUBLE PRECISION      DP_VV_V
      DOUBLE PRECISION S_CRS(3,NQUABL),PLAN(3,2,11)
      INCLUDE 'COMMON_DECL.inc'
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
!
      TROP_GEOM_TAU = - DP_VV_V ( 3, PLAN(1,2,4), &
     &                            S_CRS(1,ISOU) )*TROP_DEL/VLIGHT
      RETURN
      END  SUBROUTINE TROP_GEOM_COUPL
