!$COBDIR
!********1*********2*********3*********4*********5*********6*********7**
! COBDIR           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
       BLOCK DATA COBDR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/COBDIR/DWORD(100),CDIREC,TDIREC
      DATA CDIREC,TDIREC/0.0D0,0.0D0/
      END
