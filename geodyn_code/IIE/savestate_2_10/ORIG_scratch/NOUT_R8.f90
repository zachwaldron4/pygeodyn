!
! ------------------------------------------------------------------------
!
      SUBROUTINE NOUT_R8 ( N, VEC )
! ************************************************************************
! *                                                                      *
! *   Subroutine  NOUT_R8  fill N elements of the vector  VEC by zeroes. *
! *                                                                      *
! *  ###  12-Dec-96     NOUT_R8    v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      INTEGER  N
      DOUBLE PRECISION VEC(N)
      INTEGER  J1
      IF ( N .GT. 0 ) THEN
           DO 410 J1=1,N
              VEC(J1) = 0.0D0
 410       CONTINUE
      END IF
      RETURN
      END
