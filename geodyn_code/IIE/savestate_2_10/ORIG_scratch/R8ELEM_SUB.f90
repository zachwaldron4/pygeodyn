!
! ------------------------------------------------------------------------
!
      SUBROUTINE R8ELEM_SUB ( VEC1, IND1, VEC2, IND2 )
! ************************************************************************
! *                                                                      *
! *   Auxillary  function subtract the IND2-th element of the vector     *
! *   VEC2 from the IND1-th element of the vector VEC1 and puts result   *
! *   in the IND1-the element of the vector VEC2.                        *
! *                                                                      *
! *   VEC1(IND1) = VEC1(IND1) - VEC2(IND2)                               *
! *                                                                      *
! *  ###  05-JAN-99   R8ELEM_SUB   v1.0  (c)  L. Petrov  05-JAN-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      DOUBLE PRECISION VEC1(*), VEC2(*)
      INTEGER  IND1, IND2
      VEC1(IND1) = VEC1(IND1) - VEC2(IND2)
      RETURN
      END
