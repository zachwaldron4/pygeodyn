!$DUTCET
      SUBROUTINE DUTCET(MJDS,FSEC,D)
!********1*********2*********3*********4*********5*********6*********7**
! DUTCET           00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  TEMP ROUTINE TO GIVE THE ET-UTC TIME DIFFERENCE
!            WORKS ONLY FOR THE FIRST PART OF 1982
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDS     I    S    DUMMY VARIABLE (NOT USED)
!   FSEC     I    S    DUMMY VARIABLE (NOT USED)
!   D        O    S    DIFFERENCE ET-UTC
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DATA A1UTC/20.0343817D0/,ETA1/32.1496183D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      D=A1UTC+ETA1
      RETURN
      END
