!$CLIGHT
!********1*********2*********3*********4*********5*********6*********7**
! CLIGHT           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  BLOCK DATA ROUTINE TO PROVIDE VALUE OF ERRLIM IN
!            COMMON BLOCK/CLIGHT/
!
! COMMENTS:  ERRLIM IS ALLOWED DEVIATION OF INPUT SPEED OF LIGHT FROM
!            THE DEFAULT SPEED OF LIGHT.
!
!********1*********2*********3*********4*********5*********6*********7**
       BLOCK DATA CLITEB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      DATA ERRLIM/1.0D-8/
      END
