!$DP_VV_V
      FUNCTION DP_VV_V ( N, VEC1, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Function  DP_VV_V  calculates dot product VEC1 and VEC2            *
! *   DP_VV_V = VEC1 * VEC2                                              *
! *                                                                      *
! *  ###  12-DEC-96   DP_VV_V      v2.0  (c)  L. Petrov 05-APR-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
!     INCLUDE   'matvec.i' ! definition of DB__DP_VV_V
      INTEGER  N
      DOUBLE PRECISION      DP_VV_V, VEC1(N), VEC2(N), VEC_dol_DDOT, DDOT
!
!
! --- Generic version of the program
!
      INTEGER  J1
      DP_VV_V = 0.0D0
      IF ( N .GT. 0 ) THEN
           DO 410 J1=1,N
              DP_VV_V = DP_VV_V + VEC1(J1)*VEC2(J1)
 410       CONTINUE
      END IF
!
      RETURN
      END
