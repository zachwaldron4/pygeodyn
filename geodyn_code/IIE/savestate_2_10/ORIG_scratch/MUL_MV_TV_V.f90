      SUBROUTINE MUL_MV_TV_V ( M1, N1, MAT, M2, VECI, M3, VECO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_TV_V  multiplies matrix by vector:              *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! *  ###  12-Dec-96   MUL_MV_TV_V  v1.0  (c)  L. Petrov   12-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  M1, N1, M2, M3, IUER
      DOUBLE PRECISION MAT(M1,N1), VECI(M2), VECO(M3)
      CHARACTER  STR*80
!
      DOUBLE PRECISION S
      INTEGER  J1, J2
      INTEGER  I_LEN, ILEN
!
      DO 410 J1=1,N1
         S=0.0D0
         DO 420 J2=1,M1
            S = S + MAT(J2,J1)*VECI(J2)
 420     CONTINUE
         VECO(J1) = S
 410  CONTINUE
      RETURN
      END
