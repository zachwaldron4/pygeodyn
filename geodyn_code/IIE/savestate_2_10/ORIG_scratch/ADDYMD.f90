!$ADDYMD
      SUBROUTINE ADDYMD(IYMD,IDAY)
!********1*********2*********3*********4*********5*********6*********7**
! NAME              ADDYMD
!
! FUNCTION          TO ADD OR SUBTRACT DAYS FROM A DATE IN THE FORM
!                   YYMMDD AND TO PROVIDE THE USER WITH THE NEW DATE
!
! INPUT PARAMETERS:
!
!
!                  IYMD    - SIX DIGIT DATE IN THE FORM YYMMDD
!
!                  IDAY    - NUMBER OF DAYS TO BE ADDED OR SUBTRACTED
!                            FROM INPUT DATE
!
! OUTPUT PARAMETERS:
!                  IYMD    - NEW DATE AFTER IDAY ADDED TO INPUT DATE
!
!
! RESTRICTIONS      ....old and new dates must lie within the years:
!                          March 1, 1900 to Feb. 28, 2100
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      izero = 0
      call mjdymd( modjd, iymd, izero, 2 )
      newjd = modjd +IDAY
      call mjdymd( newjd, iymd, izero, 1 )
      if(iymd.lt.491231)iymd=iymd+1000000
!
      RETURN
      END
