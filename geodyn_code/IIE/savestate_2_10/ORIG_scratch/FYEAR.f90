!$FYEAR
      SUBROUTINE FYEAR(MJDSEC,FSEC,YEAR)
!********1*********2*********3*********4*********5*********6*********7**
!
! FUNCTION          CONVERT TIME  FROM MJDSEC,FSEC TO YEAR.FRACTION
!
! INPUT PARAMETERS:
!                   MJDSEC - MODIFIED JULIAN DATE
!                   FSEC   - REMAINING FRACTIONAL SECONDS
!
! OUTPUT PARAMETERS:
!                   YEAR   - YEAR IN THE FORM YYYY.FRAC
!
! RESTRICTIONS      ....all dates must lie within the years:
!                          March 1, 1900 to Feb. 28, 2100
!
!
!**********1*********2*********3*********4*********5*********6*********7
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
!      parameter ( half =  0.5d0 ) , ( one = 1.d0 ),
!     1          ( seven = 7.d0 )  , ( ten = 10.d0 ),
!     2          ( twelve = 12.d0 ), ( d14 = 14.d0 )
      parameter ( half =  0.5d0 )
      parameter ( one = 1.d0 )
      parameter ( seven = 7.d0 )
      parameter ( ten = 10.d0 )
      parameter ( twelve = 12.d0 )
      parameter ( d14 = 14.d0 )
!
      parameter ( xjd0 = 2400000.5d0 )
      parameter ( d1537 =  1537.d0 )
      parameter ( d122  =   122.1d0 )
      parameter ( d36525 =  365.25d0 )
      parameter ( d30600 =  30.6001d0 )
      parameter ( d4715 = 4715.d0 )
      parameter ( ib = -15 )
      parameter ( d17209 = 1720996.5d0 )
      parameter (  d3600 = 3600.d0 )
      parameter (  d60 =  60.d0 )
!
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************

      CALL MJDYMD(MJDSEC,IYMD,IHMS,4)
      DAY=DOY(MJDSEC,FSEC)
      FR=DAY/D36525

      IY=IYMD/10000

      IF(IY.LE.49) THEN
      IY=IY+2000
      ELSE
      IY=IY+1900
      ENDIF
      YEAR=DBLE(IY)+FR

      RETURN
      END
