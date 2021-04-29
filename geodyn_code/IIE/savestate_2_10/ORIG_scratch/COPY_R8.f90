!
! ------------------------------------------------------------------------
!
      SUBROUTINE COPY_R8 ( N, VEC1, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  COPY_R8  copies N elements of the vector VEC1 to the   *
! *   vector  VEC2. Type of vector: REAL*8.                              *
! *                                                                      *
! *  ###  12-Dec-96    COPY_R8     v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      INTEGER  N
      DOUBLE PRECISION VEC1(N), VEC2(N)
      INTEGER  J1
      IF ( N .GT. 0 ) THEN
           DO 410 J1=1,N
              VEC2(J1) = VEC1(J1)
 410       CONTINUE
      END IF
      RETURN
      END
