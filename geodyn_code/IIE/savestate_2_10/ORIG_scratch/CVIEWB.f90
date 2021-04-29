!$CVIEW
!********1*********2*********3*********4*********5*********6*********7**
! CVIEW            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  BLOCK DATA ROUTINE FOR COMMON BLOCK/CVIEW/
!
! COMMENTS:  PRINTOUT INFORMATION
!
!********1*********2*********3*********4*********5*********6*********7**
       BLOCK DATA CVIEWB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
! /CVIEW / PRINTER(TERMINAL) OUTPUT UNITS AND PAGE/LINE CONTROL
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
! 6,8,10,15,16 ARE PRINTER OUTPUTS.
!      6 NORMAL GEODYN II-E OUTPUT
!      8 CARTESIAN TRAJECTORY
!     10 KEPLERIAN TRAJECTORY
!     15 IS NORMAL POINT REPORT
!     16 IS PREPROCESSING CORRECTIONS
!  9 IS TERMINAL OUTPUT.
!  7 IS "PUNCHED" OUTPUT.
      DATA IOUT6 ,IOUT8 ,IOUT9 ,IOUT10,IOUT15,IOUT16/ 6, 8, 9,10,15,16/
      DATA ILINE6,ILINE8,ILINE9,ILIN10,ILIN15,ILIN16/ 0, 0, 0, 0, 0, 0/
      DATA IPAGE6,IPAGE8,IPAGE9,IPAG10,IPAG15,IPAG16/ 0, 0, 0, 0, 0, 0/
      DATA MLINE6,MLINE8,MLINE9,MLIN10,MLIN15,MLIN16/60,60,60,60,60,60/
      DATA IOUT7/7/
      END
