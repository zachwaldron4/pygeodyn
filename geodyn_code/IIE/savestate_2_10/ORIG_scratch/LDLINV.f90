      SUBROUTINE LDLINV(A,ND,N,DUMMY)
!********1*********2*********3*********4*********5*********6*********7**
! DSINV            2018-07-25          0000.0    PGMR - Joseph Nicholas
!
!
! FUNCTION:
!
!   Inverts the normal matrix 
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
!   This version must be linked to an implementation of the LAPACK
!   library.
!
!
!********1*********2*********3*********4*********5*********6*********7**
      
!     USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT32
      IMPLICIT NONE
      INTEGER, PARAMETER :: INT32 = 4
      DOUBLE PRECISION, INTENT(INOUT) :: A(*), DUMMY(*)
      INTEGER, INTENT(IN) :: ND, N
      
      INTEGER :: I, J
      INTEGER(INT32) :: N_LAPACK, IERR, LWORK, MATRIX_RANK
      DOUBLE PRECISION :: RCOND, WORK1
      INTEGER(INT32), ALLOCATABLE :: IPIV(:)
      DOUBLE PRECISION, ALLOCATABLE :: FULL_MATRIX(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: INVERSE(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: WORK(:)
      
      ALLOCATE(FULL_MATRIX(N,N))
      
      DO J = 1, N
          DO I = J, N
              FULL_MATRIX(I,J) = A(I+(J-1)*(2*ND-J)/2)
              FULL_MATRIX(J,I) = FULL_MATRIX(I,J)
          END DO
      END DO
      
      N_LAPACK = N

      ALLOCATE(INVERSE(N,N))
      INVERSE(:,:) = 0.0D0
      DO I = 1, N
          INVERSE(I,I) = 1.0D0
      END DO

!!!!  ALLOCATE(IPIV(N))
      ALLOCATE(IPIV(N*100))
      
      CALL DSYSV('L', N_LAPACK, N_LAPACK, FULL_MATRIX, &
                N_LAPACK, IPIV, INVERSE, N_LAPACK, &
                WORK1, -1_INT32, IERR)
      IF (IERR /= 0) THEN
          WRITE(*,'(A,I0)') 'ERROR: LDLINV: DSYSV FAILED&
                  & WITH ERROR CODE ', IERR
          STOP 16
      END IF

      LWORK = NINT(WORK1)
!!!!  ALLOCATE(WORK(LWORK))
      ALLOCATE(WORK(100*LWORK))

      CALL DSYSV('L', N_LAPACK, N_LAPACK, FULL_MATRIX, &
                N_LAPACK, IPIV, INVERSE, N_LAPACK, &
                WORK, LWORK, IERR)
      IF (IERR /= 0) THEN
          WRITE(*,'(A,I0)') 'ERROR: LDLINV: DSYSV FAILED&
                  & WITH ERROR CODE ', IERR
          STOP 16
      END IF
      
      DO J = 1, N
          DO I = J, N
              A(I+(J-1)*(2*ND-J)/2) = INVERSE(I,J)
          END DO
      END DO

      DEALLOCATE(WORK)
      DEALLOCATE(IPIV)
      DEALLOCATE(INVERSE)
      DEALLOCATE(FULL_MATRIX)
      END SUBROUTINE
