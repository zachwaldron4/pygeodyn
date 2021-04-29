!
! ------------------------------------------------------------------------
!
      SUBROUTINE TM_II ( N, MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  TM_II  transform QUADRATIC matrix MAT of dimension L*L to *
! *   the matrix to be transponse with respect to the initial one.       *
! *                                                                      *
! *  ###  02-JAN-97      TM_II     V1.0 (c)   L. Petrov  02-JAN-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  N, J1, J2
      DOUBLE PRECISION MAT(N,N), SWAP
!
      DO 410 J1=1,N-1
         DO 420 J2=J1+1,N
            SWAP       = MAT(J1,J2)
            MAT(J1,J2) = MAT(J2,J1)
            MAT(J2,J1) = SWAP
 420     CONTINUE
 410  CONTINUE
      RETURN
      END
