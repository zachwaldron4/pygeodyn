!$MATPRD
      SUBROUTINE MATPRD(VMBYJ,VJBYI,VMBYI,NM,NJ,NI)
!********1*********2*********3*********4*********5*********6*********7**
! MATPRD           83/06/15            8306.0    PGMR - TOM MARTIN
!
!
! FUNCTION:  COMPUTE THE PRODUCT OF AN Nm x Nj MATRIX AND
!            A Nj x Ni MATRIX
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   VMBYJ    I    A    Nm x Nj MATRIX
!   VJBYI    I    A    Nj x Ni MATRIX
!   VMBYI    O    A    PRODUCT Nm x Ni MATRIX
!   NM       I    S    DIMENSION Nm
!   NJ       I    S    DIMENSION Nj
!   NI       I    S    DIMENSION Ni
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION VMBYJ(NM,NJ),VJBYI(NJ,NI),VMBYI(NM,NI)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 3000 I=1,NI
      DO  800 M=1,NM
      VMBYI(M,I)=           VMBYJ(M,1)*VJBYI(1,I)
  800 END DO
      DO 2000 J=2,NJ
      DO 1800 M=1,NM
      VMBYI(M,I)=VMBYI(M,I)+VMBYJ(M,J)*VJBYI(J,I)
 1800 END DO
 2000 END DO
 3000 END DO
      RETURN
      END
