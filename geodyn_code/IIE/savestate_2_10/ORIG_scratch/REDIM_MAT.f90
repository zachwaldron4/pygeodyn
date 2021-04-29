!
! ------------------------------------------------------------------------
!
      FUNCTION REDIM_MAT ( MO1, KO1, KO2, MN1, KN1, KN2 )
! ************************************************************************
! *                                                                      *
! *   Routine  REDIM_MAT  calculates a pair of indeces for the certain   *
! *   element of the rectangular matrix with different keeping scheme.   *
! *   Input:  assumed dimension of the matrix (MO1,*) (KO1,KO2),         *
! *   Output: assumed dimension of the matrix (MN1,*) (KN1,KN2).         *
! *   Thus, REDIM_MAT  can be used when we change declarations of the    *
! *   dimension of the matrix during passing the matrix as an argument   *
! *   into the subroutine. It finds a pair of indeces in "new"           *
! *   declarations of the dimensions which corresponds to the same       *
! *   element on "old" declarations of the dimensions.                   *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *         MO1 ( INTEGER*4  ) -- Old first dimension of the matrix      *
! *                               (second dimension can be arbitrary).   *
! *         KO1 ( INTEGER*4  ) -- Old first  index of the element.       *
! *         KO2 ( INTEGER*4  ) -- Old second index of the element.       *
! *                                                                      *
! * ________________________ OUTPUT PARAMETERS: ________________________ *
! *                                                                      *
! * <REDIM_MAT> ( INTEGER*4  ) -- offset of the matrix element from the  *
! *                               beginning of the matrix (offset        *
! *                               counted from the 1 for the first       *
! *                               element).                              *
! *         MN1 ( INTEGER*4  ) -- New first dimension of the matrix      *
! *                               (second dimension can be arbitrary).   *
! *         KN1 ( INTEGER*4  ) -- New first  index of the element.       *
! *         KN2 ( INTEGER*4  ) -- New second index of the element.       *
! *                                                                      *
! *  ###  24-FEB-97    REDIM_MAT    v1.0 (c)  L. Petrov  25-FEB-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  REDIM_MAT, MO1, KO1, KO2, MN1, KN1, KN2
!
      REDIM_MAT = KO1 + (KO2-1)*MO1
      KN2 = REDIM_MAT/MN1
      KN1 = REDIM_MAT - KN2*MN1
!
      IF ( KN1 .EQ. 0 ) THEN
           KN1 = MN1
         ELSE
           KN2 = KN2 + 1
      END IF
!
      RETURN
      END
