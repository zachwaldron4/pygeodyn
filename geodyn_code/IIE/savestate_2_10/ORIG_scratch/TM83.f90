!
! ------------------------------------------------------------------------
!
      SUBROUTINE TM83 ( MAT1, MAT2 )
! ************************************************************************
! *                                                                      *
! *   Routinme  TM83  computes MAT2 as a matrix transponed with respect  *
! *   to MAT1. Both matrix are 3*3                                       *
! *                                                                      *
! *  ###  31-AUG-99      TM83      v1.0  (c)  L. Petrov  31-AUG-99  ###  *
! *                                                                      *
! ************************************************************************
      DOUBLE PRECISION MAT1(3,3), MAT2(3,3)
!
      MAT2(1,1) = MAT1(1,1)
      MAT2(2,1) = MAT1(1,2)
      MAT2(3,1) = MAT1(1,3)
!
      MAT2(1,2) = MAT1(2,1)
      MAT2(2,2) = MAT1(2,2)
      MAT2(3,2) = MAT1(2,3)
!
      MAT2(1,3) = MAT1(3,1)
      MAT2(2,3) = MAT1(3,2)
      MAT2(3,3) = MAT1(3,3)
!
      RETURN
      END
