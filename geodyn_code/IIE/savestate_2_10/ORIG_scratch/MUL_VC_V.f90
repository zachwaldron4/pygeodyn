      SUBROUTINE MUL_VC_V ( N, VEC, C )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_VC_V  multilies vector  VEC  by constant  C  and   *
! *   put the result to VEC.                                             *
! *                                                                      *
! *  ###  12-Dec-96    MUL_VC_V     v1.0  (c)  L. Petrov  12-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  N, J1
      DOUBLE PRECISION VEC(N), C
!
      DO 410 J1=1,N
         VEC(J1) = VEC(J1)*C
 410  CONTINUE
!
      RETURN
      END
