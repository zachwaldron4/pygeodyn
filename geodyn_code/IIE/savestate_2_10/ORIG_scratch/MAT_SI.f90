!
! ------------------------------------------------------------------------
!
      SUBROUTINE MAT_SI ( N, MATS, MATI )
! ************************************************************************
! *                                                                      *
! *   Routine  MAT_SI  transform matrix MATS of dimension L*L in         *
! *   upper triangular representation to matrix MATI in rectangular      *
! *   representation.                                                    *
! *                                                                      *
! *  ###  02-JAN-97     MAT_SI     V1.0 (c)   L. Petrov  02-JAN-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  N, LA, J1, J2
      DOUBLE PRECISION MATS(*), MATI(N,N)
!
      LA=0
      DO 410 J1=1,N
         DO 420 J2=1,J1
            LA=LA+1
            MATI(J1,J2)=MATS(LA)
            MATI(J2,J1)=MATS(LA)
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END
