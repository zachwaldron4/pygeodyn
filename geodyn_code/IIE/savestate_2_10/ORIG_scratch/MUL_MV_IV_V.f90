      SUBROUTINE MUL_MV_IV_V ( M1, N1, MAT, M2, VECI, M3, VECO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_IV_V  multiplies matrix by vector:              *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! * ###  10-DEC-96  MUL_MV_IV_V  v3.0  (c)  L. Petrov   22-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
!     INCLUDE   'matvec.i'
      INTEGER  M1, N1, M2, M3, IUER
      DOUBLE PRECISION MAT(M1,N1), VECI(M2), VECO(M3)
      CHARACTER  STR*80
!
      DOUBLE PRECISION S
      INTEGER  J1, J2, J3, J4
      INTEGER  I_LEN, ILEN
!
           DO 430 J3=1,M1
              S=0.0D0
              DO 440 J4=1,N1
                 S = S + MAT(J3,J4)*VECI(J4)
 440          CONTINUE
              VECO(J3) = S
 430       CONTINUE
!
      RETURN
      END
