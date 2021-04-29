!
! ------------------------------------------------------------------------
!
      SUBROUTINE COPY_I4 ( N, IVEC1, IVEC2 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  COPY_I4  copies N elements of the INTEGER*4 vector     *
! *   IVEC1 to the vector  IVEC2.                                        *
! *                                                                      *
! *  ###  02-AUG-97     COPY_I4    v1.0  (c)  L. Petrov  02-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      INTEGER  N
      INTEGER*4  IVEC1(N), IVEC2(N)
      INTEGER  J1
      IF ( N .GT. 0 ) THEN
           DO 410 J1=1,N
              IVEC2(J1) = IVEC1(J1)
 410       CONTINUE
      END IF
      RETURN
      END
