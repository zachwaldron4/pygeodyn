!$CHKTM1
      SUBROUTINE CHKTM1( TIME , cdname )
!********1*********2*********3*********4*********5*********6*********7**
! CHKTM1           03/21/91            0000.0    PGMR - J. MCCARTHY
!
! FUNCTION          checks for valid input date/time
!                   if time valid, normal return
!                   if time is invalid, stop with error message
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   time     I         date/time in YYMMDDHHMMSS.SSSS format
!   CDNAME
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      parameter ( time1 = 480101000000.0D0 )
      parameter ( time2 = 991231235960.0D0 )
      parameter ( C1D6  = 1.0D6 )
!
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      character*8 cdname
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      if( time .lt. time1 .or. time .gt. time2 ) then
         write(6,*) '    '
         write(6,*) '*************************************************'
         write(6,*) 'Time read from the  ', cdname, ' card is invalid.'
         write(6,*) 'Value read is ', time
         IYMD = time / 1000000
         iy = iymd / 10000
         im = MOD( iymd, 10000 ) / 100
         id = MOD( iymd, 100 )
         IHMS = time - IYMD * C1D6
         ih = ihms / 10000
         mn = MOD( ihms, 10000 ) / 100
         is = MOD( ihms, 100 )
         write(6,*) 'This time corresponds to:'
         write(6,*) 'year   = ', iy
         write(6,*) 'month  = ', im
         write(6,*) 'day    = ', id
         write(6,*) 'hour   = ', ih
         write(6,*) 'minute = ', mn
         write(6,*) 'sec    = ', is
         write(6,*) 'Please correct card and re-submit.'
         write(6,*) 'Stopping program in subroutine CHKTM1.'
         write(6,*) '*************************************************'
         write(6,*) '    '
         stop 16
      endif
      return
!
      END
