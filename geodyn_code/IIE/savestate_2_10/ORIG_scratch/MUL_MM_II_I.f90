      SUBROUTINE MUL_MM_II_I ( M1, N1, MAT1, M2, N2, MAT2, M3, N3, &
     &                              MATO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MM_II_I  multiplies matrices:                      *
! *   MATO = MAT1 * MAT2                                                 *
! *                                                                      *
! *  ###  10-DEC-96   MUL_MM_II_I  v3.0  (c) L. Petrov  17-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
!     INCLUDE   'matvec.i' ! Definnition of DB__MUL_MM_II_I
      INTEGER  M1, N1, M2, N2, M3, N3, IUER
      DOUBLE PRECISION MAT1(M1,N1), MAT2(M2,N2), MATO(M3, N3)
      CHARACTER  STR*80
      INTEGER  J1, J2, J3
!
      INTEGER  I_LEN, ILEN
!
! --- Genereic version
!
      DO 410 J1=1,N3
         DO 420 J2=1,M3
            MATO(J2,J1) = 0.0D0
            DO 430 J3=1,N1
               MATO(J2,J1) = MATO(J2,J1) + MAT1(J2,J3)*MAT2(J3,J1)
 430        CONTINUE
 420     CONTINUE
 410  CONTINUE
      RETURN
      END
