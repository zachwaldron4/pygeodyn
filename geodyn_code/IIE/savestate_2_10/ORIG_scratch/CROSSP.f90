!$CROSSP
      SUBROUTINE CROSSP(UX,UY,UZ,NDIMX,NDIMY,NM)
!********1*********2*********3*********4*********5*********6*********7**
! CROSSP           83/07/12            8307.0    PGMR - TOM MARTIN
!
! FUNCTION:  COMPUTE THE CROSS PRODUCT OF TWO Nm x 3 MATRICES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   UX       I    A    Nm x 3 MATRIX
!   UY       I    A    Nm x 3 MATRIX
!   UZ       O    A    PRODUCT Nm x 3 MATRIX
!   NDIMX    I    S
!   NDIMY    I    S
!   NM       I    S    DIMENSION Nm
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION UX(NDIMX,3),UY(NDIMY,3),UZ(NM,3),IX(3),IY(3)
      DATA IX/2,3,1/,IY/3,1,2/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 INDZ=1,3
      INDX=IX(INDZ)
      INDY=IY(INDZ)
      DO  800 M=1,NM
      UZ(M,INDZ)=           UX(M,INDX)*UY(M,INDY)
  800 END DO
 1000 END DO
      DO 2000 INDZ=1,3
      INDX=IY(INDZ)
      INDY=IX(INDZ)
      DO 1800 M=1,NM
      UZ(M,INDZ)=UZ(M,INDZ)-UX(M,INDX)*UY(M,INDY)
 1800 END DO
 2000 END DO
      RETURN
      END
