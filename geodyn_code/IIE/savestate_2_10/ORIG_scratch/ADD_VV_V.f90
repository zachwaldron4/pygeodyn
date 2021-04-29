!$ADD_VV_V
      SUBROUTINE ADD_VV_V ( N, VEC1, VEC2, VEC3 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  ADD_VV_V  adds vector VEC2 to vector VEC1 and puts sum *
! *   to vector VEC3.                                                    *
! *                                                                      *
! *  ###  12-Dec-96   ADD_VV_V     v1.1  (c)  L. Petrov  10-MAR-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  N
      DOUBLE PRECISION VEC1(N), VEC2(N), VEC3(N)
      INTEGER  J1
!
      DO 410 J1=1,N
         VEC3(J1) = VEC1(J1) + VEC2(J1)
 410  CONTINUE
!
      RETURN
      END
