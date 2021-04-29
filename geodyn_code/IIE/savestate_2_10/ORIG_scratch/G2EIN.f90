!$G2EINF
!********1*********2*********3*********4*********5*********6*********7**
! G2EINF           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  BLOCK DATA FOR VERSION AND DATE OF CURRENT LOAD MODULE.
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
       BLOCK DATA G2EIN
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/G2EINF/G2EVER,G2EDAT,G2ERTM
      DATA G2EVER/1810.80D0/
      DATA G2EDAT/100519.D0/
      END
