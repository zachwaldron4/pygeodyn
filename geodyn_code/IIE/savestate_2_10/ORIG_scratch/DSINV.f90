!$DSINV
      SUBROUTINE DSINV(A,ND,N,DUMMY)
!********1*********2*********3*********4*********5*********6*********7**
! DSINV            2014-02-06          0000.0    PGMR - Joseph Nicholas
!
!
! FUNCTION:
!
!   Inverts the normal matrix using a Cholesky decomposition.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A       I/O   A    Normal matrix in lower-triangular packed form
!   ND       I    S    Leading dimension
!   N        I    S    Size of array to invert
!   DUMMY         A    Not used (present to match calling arguments of
!                      the original DSINV routine)
!
! COMMENTS:
!
!   This is a faster replacement for the orignal DSINV.  This version
!   must be linked to an implementation of the LAPACK library.
!
!
!********1*********2*********3*********4*********5*********6*********7**

!     USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT32
      IMPLICIT NONE
      INTEGER, PARAMETER :: INT32 = 4
      DOUBLE PRECISION, INTENT(INOUT) :: A(*), DUMMY(*)
      INTEGER, INTENT(IN) :: ND, N

      INTEGER :: I, J
      INTEGER(INT32) :: N_LAPACK, IERR
      DOUBLE PRECISION, ALLOCATABLE :: PACKED(:)

      ALLOCATE(PACKED(N*(N+1)/2))

      DO I = 1, N
          DO J = 1, I
              PACKED(I+(J-1)*(2*N-J)/2) = A(I+(J-1)*(2*ND-J)/2)
          END DO
      END DO

      N_LAPACK = N

      CALL DPPTRF('L', N_LAPACK, PACKED, IERR)
      IF (IERR /= 0) THEN
          WRITE(*,'(A,I0)') 'WARNING: DSINV: CHOLESKY FACTORIZATION&
                  & FAILED WITH ERROR CODE ', IERR
          WRITE(*,'(9X,A)') 'FALLING BACK ON LDL FACTORIZATION'
          CALL LDLINV(A,ND,N,DUMMY)
          DEALLOCATE (PACKED)
          RETURN
      END IF

      CALL DPPTRI('L', N_LAPACK, PACKED, IERR)
      IF (IERR /= 0) THEN
          WRITE(*,'(A,I0)') 'WARNING: DSINV: INVERSION&
                & FAILED WITH ERROR CODE ', IERR
          WRITE(*,'(9X,A)') 'FALLING BACK ON LDL FACTORIZATION'
          CALL LDLINV(A,ND,N,DUMMY)
          DEALLOCATE (PACKED)
          RETURN
      END IF

      DO I = 1, N
          DO J = 1, I
              A(I+(J-1)*(2*ND-J)/2) = PACKED(I+(J-1)*(2*N-J)/2)
          END DO
      END DO

      DEALLOCATE(PACKED)
      END SUBROUTINE
