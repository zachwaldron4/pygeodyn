!$HSCALE
!********1*********2*********3*********4*********5*********6*********7**
! HSCALE           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  BLOCK DATA HSCALB
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
       BLOCK DATA HSCALB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/HSCALE/HTSCAL
      DATA HTSCAL/1.D0/
      END
