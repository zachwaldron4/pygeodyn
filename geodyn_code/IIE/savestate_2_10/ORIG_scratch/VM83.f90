!
! ------------------------------------------------------------------------
!
      SUBROUTINE VM83 ( VEC1, VEC2, RES )
! ************************************************************************
! *                                                                      *
! *   Routine VM83 computes vector prodcut of two three-demension        *
! *   vectors VEC1, VEC2 and puts results in RES.                        *
! *                                                                      *
! *  ### 20-JAN-1989      VM83     v1.0 (c)  L. Petrov  20-JAN-1989 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      DOUBLE PRECISION VEC1(3), VEC2(3), RES(3)
!
      RES(1) = VEC1(2)*VEC2(3) - VEC1(3)*VEC2(2)
      RES(2) = VEC1(3)*VEC2(1) - VEC1(1)*VEC2(3)
      RES(3) = VEC1(1)*VEC2(2) - VEC1(2)*VEC2(1)
!
      RETURN
      END
