!$ADD_VV
      SUBROUTINE ADD_VV ( N, VEC1, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  ADD_VV  adds vector VEC2 to vector VEC1 and puts sum   *
! *   to vector VEC1.                                                    *
! *                                                                      *
! *  ###  12-Dec-96    ADD_VV      v1.0  (c)  L. Petrov   12-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  N, J1
      DOUBLE PRECISION VEC1(N), VEC2(N)
!
      DO 410 J1=1,N
         VEC1(J1) = VEC1(J1) + VEC2(J1)
 410  CONTINUE
!
      RETURN
      END
