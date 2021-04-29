!$DOTPRD
      SUBROUTINE DOTPRD(V1MBYI,V2MBYI,VM,NDIM1,NDIM2,NM,NI)
!********1*********2*********3*********4*********5*********6*********7**
! DOTPRD           83/06/15            8306.0    PGMR - TOM MARTIN
!
! FUNCTION:  COMPUTE THE DOT PRODUCT OF TWO Nm x Ni MATRICES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   V1MBYI   I    A    Nm x Ni MATRIX
!   V2MBYI   I    A    Nm x Ni MATRIX
!   VM       O    A    PRODUCT Nm VECTOR
!   NDIM1    I    S
!   NDIM2    I    S
!   NM       I    S    DIMENSION Nm
!   NI       I    S    DIMENSION Ni
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION V1MBYI(NDIM1,NI),V2MBYI(NDIM2,NI),VM(NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO  800 M=1,NM
      VM(M)=      V1MBYI(M,1)*V2MBYI(M,1)
  800 END DO
      DO 2000 I=2,NI
      DO 1800 M=1,NM
      VM(M)=VM(M)+V1MBYI(M,I)*V2MBYI(M,I)
 1800 END DO
 2000 END DO
      RETURN
      END
