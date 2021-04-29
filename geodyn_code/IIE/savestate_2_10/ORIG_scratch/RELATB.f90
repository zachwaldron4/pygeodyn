!$CRELAT
!********1*********2*********3*********4*********5*********6*********7**
! CRELAT           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
       BLOCK DATA RELATB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CRELAT/AER(11),GRLFC2(11),GRLFC3(11),SECCOR
! CONSTANTS USED FOR GENERAL RELATIVITY LIGHT TIME CORRECTION
! AER 1/MEAN PLANET RADIUS
!                                   EARTH                   MARS
      DATA AER/   0.00,      0.00,.156937D-6,      0.00,   .295D-6,     &
     &            0.00,      0.00,      0.00,      0.00,      0.00,     &
     &            0.00/
! GRLFC2 IS 2*GM/(C**2), 1/C**2=1.11265-17
      DATA GRLFC2/0.00,      0.00,.8870056D-2,     0.00, .47653D-3,     &
     &            0.00,      0.00,      0.00,      0.00,      0.00,     &
     &            0.00/
! GRLFC3 IS 2*GM/(C**3), 1/C**3=3.71140-26
      DATA GRLFC3/0.00,      0.00,.29587322D-10,   0.00,3.1790696D-12,  &
     &            0.00,      0.00,      0.00,      0.00,      0.00,     &
     &            0.00/
      DATA SECCOR/0.0D0/
      END
