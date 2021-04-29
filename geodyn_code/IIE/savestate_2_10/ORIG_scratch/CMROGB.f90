!$CMROGB
!********1*********2*********3*********4*********5*********6*********7**
! CMROG            00/00/00            0000.0    PGMR -  D ROWLAND
!
!
! FUNCTION:  BLOCK DATA INITIALIZATION OF COMMON BLOCK MROGER.
!            (DEFINITION OF SATELLITE NEIGHBORS BY MEAS TYPE)
!
! COMMENT:
!
!********1*********2*********3*********4*********5*********6*********7**
       BLOCK DATA CMROGB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/MROGER/NEIGHB(2,3,30)
!
!
!                 MEAS T  37 & 38 | MEAS T 39 & 40  | MEAS T 41 & 42
      DATA NEIGHB/0, 0, 0, 0, 0, 0,11, 0, 0, 0, 0, 0,11, 0, 0, 0, 0, 0, &
     &            2, 0, 1, 0, 0, 0,11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     &            2, 0, 1, 0, 0, 0,11, 0, 0, 0, 0, 0,12,11, 0, 0, 0, 0, &
     &            2,12, 1, 0, 0, 0, 3, 2, 1,12,13, 1, 0, 0,11,12,13,11, &
     &           11,12, 0, 0, 0, 0,11, 0,11, 0, 0, 0, 2, 3, 1, 0, 1, 0, &
     &            2, 0, 1,11, 0, 0, 0, 0, 0, 0, 0, 0, 2,11, 1,11, 0, 0, &
     &            2, 3, 1,12, 1,13, 2,12, 1, 0, 1, 0,12,11, 0, 0, 0, 0, &
     &            3, 2, 1,12,13, 1, 0, 0,11,12,13,11, 3, 2, 1,12,13, 1, &
     &            2, 3,11, 1,11, 1,11,12,11,12, 0, 0, 2,12, 1,12, 1,13, &
     &            0, 0,11,12,11,13, 2, 3, 1,12, 1,13, 3, 2, 1,12,13, 1/
!                 MEAS T  43 & 44 | MEAS T 45 & 46  | MEAS T 47 & 48
!                 MEAS T  49 & 50 | MEAS T 51 & 52  | MEAS T 53 & 54
!                 MEAS T  55 & 56 | MEAS T 57 & 58  | MEAS T 59 & 60
!                 MEAS T  61 & 62 | MEAS T 63 & 64  | MEAS T 65 & 66
!                 MEAS T  67 & 68 | MEAS T 69 & 70  | MEAS T 71 & 72
!                 MEAS T  73 & 74 | MEAS T 75 & 76  | MEAS T 77 & 78
!                 MEAS T  79 & 80 | MEAS T 81 & 82  | MEAS T 83 & 84
!                 MEAS T  85 & 86 | MEAS T 87 & 88  | MEAS T 89 & 90
!                 MEAS T  91 & 92 | MEAS T 93 & 94  | MEAS T 95 & 96
      END
