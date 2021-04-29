!$COCLRA
!********1*********2*********3*********4*********5*********6*********7**
! COCLRA           08/20/92            9208.0    PGMR - J. McCarthy
!
!
! FUNCTION:  BLOCK DATA ROUTINE FOR COMMON BLOCK/OCLRA/
!
! COMMENTS:  swtiches for OFFSET, CGMASS and LRARC corrections
!
!********1*********2*********3*********4*********5*********6*********7**
       BLOCK DATA COCLRA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
! /OCLRA /  swtiches for OFFSET, CGMASS and LRARC corrections
      COMMON/OCLRA /LOCLRA, LOCLRS, LOCLRC(3), LRAM51
!
      DATA LOCLRA/.FALSE./
      DATA LOCLRS/.FALSE./
      DATA LOCLRC/3*.FALSE./
      DATA LRAM51/.FALSE./
      END
