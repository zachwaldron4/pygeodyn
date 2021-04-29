!
! ------------------------------------------------------------------------
!
      SUBROUTINE MAT_IS ( N, MATI, MATS )
! ************************************************************************
! *                                                                      *
! *   Routine  MAT_IS  transform matrix MATI of dimension L*L in         *
! *   rectangular representation to matrix MATS in upper triangular      *
! *   representation, saving diagonal and up-diagonal terms.             *
! *                                                                      *
! *  ###  02-JAN-97     MAT_IS     V1.0 (c)   L. Petrov  02-JAN-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  N, LA, J1, J2
      DOUBLE PRECISION MATI(N,N), MATS(*)
!
      LA=0
      DO 410 J1=1,N
         DO 420 J2=1,J1
            LA=LA+1
            MATS(LA)=MATI(J2,J1)
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END
