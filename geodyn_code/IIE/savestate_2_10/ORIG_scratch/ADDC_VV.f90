!$ADDC_VV
      SUBROUTINE ADDC_VV ( N, C1, VEC1, C2, VEC2, VEC3 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  ADDC_VV   multiplies vector VEC1 by constant C1,       *
! *   multiply vector VEC2 by constant C2, adds reults and put the summa *
! *   to vector VEC3.                                                    *
! *                                                                      *
! *  ###  12-Dec-96    ADD_VV      v1.0  (c)  L. Petrov   12-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  N, J1
      DOUBLE PRECISION C1, VEC1(N), C2, VEC2(N), VEC3(N)
!
      DO 410 J1=1,N
         VEC3(J1) = C1*VEC1(J1) + C2*VEC2(J1)
 410  CONTINUE
!
      RETURN
      END
