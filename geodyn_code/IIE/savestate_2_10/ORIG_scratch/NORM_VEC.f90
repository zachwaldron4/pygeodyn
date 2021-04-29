!
! ------------------------------------------------------------------------
!
      SUBROUTINE NORM_VEC ( N, VEC, RD )
! ************************************************************************
! *                                                                      *
! *   Routine  NORM_VEC  calculate the Euclid norm of the vecot VEC      *
! *   and normalize it.                                                  *
! *                                                                      *
! *  ###  18-AUG-98     NORM_VEC   v1.1  (c)  L. Petrov  09-JUN-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  N
      DOUBLE PRECISION VEC(N), RD, DP_VV_V
!
      RD = SQRT ( DP_VV_V ( N, VEC, VEC ) )
      CALL MUL_VC_V ( N, VEC, 1.D0/RD )
!
      RETURN
      END
