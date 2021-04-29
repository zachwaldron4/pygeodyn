!$MJDYMD
      SUBROUTINE MJDYMD(MODJD,IYMD,IHMS,IFLAG)
!********1*********2*********3*********4*********5*********6*********7**
! MJDYMD           CREATION DATE 07/18/83     PGMR - D. SEVITSKI
!                       UPDATED  10/30/84     PGMR - B EDDY
!                       UPDATED  12/13/90     PGMR - J. MCCARTHY
!
! FUNCTION          CONVERT TIMES FROM MJD AND MJDS TO YMD AND
!                   VICE VERSA
!
! INPUT PARAMETERS:
!                   MODJD - MODIFIED JULIAN DATE        (IFLAG =1)
!                   MODJD - MODIFIED JULIAN DAY SECONDS (IFLAG =3 OR 4)
!                   IYMD  - YEAR,MONTH,DAY(YYMMDD)      (IFLAG =2)
!                   IFLAG - FLAG INDICATING INPUT AND OUTPUT
!                           TIME CONVERSION AS FOLLOWS:
!                              1 = MJD    TO YYMMDD
!                              2 = YYMMDD TO MJD
!                              3 = MJDS   TO YYMMDD
!                              4 = MJDS   TO YYMMDD HHMMSS
!
! OUTPUT PARAMETERS:
!                   MODJD - MODIFIED JULIAN DATE        (IFLAG =2)
!                   IYMD  - YEAR,MONTH,DAY(YYMMDD)      (IFLAG =1,3,4)
!                   IHMS  - HOUR,MINUTE, SECOND(HHMMSS) (IFLAG =4)
!
!
! RESTRICTIONS      ....all dates must lie within the years:
!                          March 1, 1900 to Feb. 28, 2100
!
!
! UPDATE HISTORY:
!
! 10/30/84  B EDDY     - OPTION TO OUTPUT HOUR MINUTES SECONDS ADDED
! 12/13/90  J McCarthy - capability to handle dates in 21th century adde
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
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
!      DATA MJD00/15019/,MD1901/15385/,MD1940/29629/
!      DATA IYMD01/010101/,IYMD40/400101/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      IF(IFLAG.LT.1 .OR. IFLAG.GT.4) RETURN
!
      GOTO (100,200,300,300),IFLAG
!
!     MJD TO YMD
!
  100 CONTINUE
!
      xjd = modjd + xjd0
!     write(6,*)'mjdymd: xjd ', xjd
      c = INT( xjd + half ) + 1537
!     write(6,*)'mjdymd: c ',c
      nd = INT( (c - d122) / d36525 )
!     write(6,*)'mjdymd: nd ', nd
      e = INT( d36525 * nd )
!     write(6,*)'mjdymd: e ', e
      nf = INT( ( c - e ) / d30600 )
!
      id = c - e - INT( d30600 * nf ) + frac( xjd + half )
!     write(6,*)'mjdymd: id ',id
      im = nf - 1 - 12 * INT( nf / 14 )
!     write(6,*)'mjdymd: im ', im
      iy = nd - 4715 - INT(  ( 7 + im ) / 10 )
!     write(6,*)'mjdymd: iy ', iy
!
!
!     ....need this mod to get 2-digit output years
!
      iy = MOD( iy, 100 )
!     write(6,*)'mjdymd: iy ', iy
!
      iymd = iy * 10000 + im * 100 + id
!
!     write(6,*) ' iy, im, id ', iy, im, id, '  mjd ', mjd
!
      RETURN
!
!     YMD TO MJD
!
  200 CONTINUE
      IY=IYMD/10000
      IM=(IYMD-IY*10000)/100
      ID=IYMD-IY*10000-IM*100
!
!
      if( iy .lt. 1900 ) then
         if( iy .gt. 50 ) then
            y = 1900 + iy - 1
!     write (6,*)'in 200 block: y ', y
         else
            y = 2000 + iy - 1
!     write (6,*)'in 200 block: y ', y
         endif
      endif
!
      if( im .gt. 2 ) then
         m = im
!   write (6,*)'in 200 block: m ', m
         y = y + 1
      else
         m = im + 12
!   write (6,*)'in 200 block: m ', m
      endif
!
      xjd = INT( d36525 * y ) + INT( d30600 * (m + 1) ) + ib            &
     &    + d17209 + id
!     write(6,*) ' xjd ', xjd
!
      modjd = xjd - xjd0
!     write(6,*) ' modjd ', modjd
!
!     ....below line not needed for iflag=2
!cc      fsec = ih * d3600 + imin * d60 + sec
!
!     write(6,*) ' iy, im, id ', iy, im, id, '  mjd ', mjd
!
      RETURN
!
!     MJDS TO YYMMDD (AND HHMMSS IF IFLAG=4)
!
  300 CONTINUE
!     CONVERT FROM MJDS TO MJD
!     ....mjds = 86400 * ( jd - geodyn reference time in jd )
!     ....     = 86400 * ( mjd - geodyn reference time in mjd )
      IDY=(MODJD/86400)+INT(TMGDN2+0.1)
!
!     CONVERT FROM MJD TO YMD (SAME AS ABOVE)
!
      xjd = IDY + xjd0
      c = INT( xjd + half ) + 1537
      nd = INT( (c - d122) / d36525 )
      e = INT( d36525 * nd )
      nf = INT( ( c - e ) / d30600 )
!
      id = c - e - INT( d30600 * nf ) + frac( xjd + half )
      im = nf - 1 - 12 * INT( nf / 14 )
      iy = nd - 4715 - INT(  ( 7 + im ) / 10 )
!
!     ....need this mod to get 2-digit output years
!
      iy = MOD( iy, 100 )
!
      iymd = iy * 10000 + im * 100 + id
!
!      write(6,*) ' iy, im, id ', iy, im, id, '  mjd ', mjd
!
!
      IF(IFLAG.EQ.3) RETURN
!
      ISEC  =MOD(MODJD,86400)
      IH    =ISEC/3600
      IM    =(ISEC-IH*3600)/60
      IS    =ISEC-IH*3600-IM*60
      IHMS  =IH*10000+IM*100+IS
      RETURN
      END
