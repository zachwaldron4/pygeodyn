!$LOW_TRG_UPDATE
!
      SUBROUTINE LOW_TRG_UPDATE ( NPAR, NOR_MAT, NOR_VEC, EQU_CNS,  &
     &RH, SIGMA )
! ************************************************************************
! *                                                                      *
! *   Routine LOW_TRG_UPDATE
! *                                                                      *
! *  ### 11-AUG-2005 LOW_TRG_UPDATE  v1.0 (c) L. Petrov  11-AUG-2005 ### *
! *                                                                      *
! ************************************************************************
      INTEGER    NPAR
      DOUBLE PRECISION NOR_MAT(*), NOR_VEC(NPAR), EQU_CNS(NPAR), RH, SIGMA
      INTEGER    J1, J2, IND
!
      DO 410 J1=1,NPAR
      IND = J1
       DO 420 J2=1,J1
       NOR_MAT(IND) = NOR_MAT(IND) + EQU_CNS(J1)*EQU_CNS(J2)/SIGMA**2
       IND = IND + NPAR - J2
420    CONTINUE
       NOR_VEC(J1) = NOR_VEC(J1) + EQU_CNS(J1)*RH/SIGMA**2
410   CONTINUE
!
      RETURN
      END  SUBROUTINE LOW_TRG_UPDATE
