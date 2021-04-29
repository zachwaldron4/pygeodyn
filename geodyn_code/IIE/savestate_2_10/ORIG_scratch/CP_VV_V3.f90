!
! ------------------------------------------------------------------------
!
      SUBROUTINE CP_VV_V3 ( VEC1, VEC2, VECO )
! ************************************************************************
! *                                                                      *
! *   Subroutine  CP_VV_V  calculates cross product VEC1 and VEC2 of     *
! *   dimension 3.  CP_VV_V = VEC1 x VEC2                                *
! *                                                                      *
! *  ###  12-Dec-96   CP_VV_V      v1.0  (c)  L. Petrov   12-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      DOUBLE PRECISION VEC1(3), VEC2(3), VECO(3)
!
      VECO(1) = VEC1(2)*VEC2(3) - VEC1(3)*VEC2(2)
      VECO(2) = VEC1(3)*VEC2(1) - VEC2(1)*VEC1(3)
      VECO(3) = VEC1(1)*VEC2(2) - VEC2(2)*VEC1(1)
!
      RETURN
      END
