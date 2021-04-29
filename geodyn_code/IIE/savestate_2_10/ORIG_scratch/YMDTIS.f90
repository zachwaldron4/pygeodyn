!$YMDTIS
      SUBROUTINE YMDTIS(IYIMID,IHIMIS,ISEC)
!*********************************************************************SA
!   VERSION           DATE 11/30/82     PGMR D. ROWLANDS
!                           7/14/90          J. McCarthy
!
!   FUNCTION         CALCULATE THE INTEGER NUMBER OF UT SECONDS
!                    SINCE GEODYN REF. TIME TO TIME IYIMID,IHIMIS
!
!   INPUT PARAMETER  IYIMID - REFERENCE DATE
!                    IHIMIS - REFERENCE TIME
!
!   OUTPUT PARAMETER ISEC - INTEGER UT SECONDS FROM GEODYN REF.TIME TO
!                           TIME IYIMID IHIMIS
!
!   RESTRICTIONS     the DATE is interpreted as follows:
!                        if IY <  50, the year is 2000-2049
!                        if IY >= 50, the year is 1950-1999
!
!                    only works for years 1950 - 2049
!
!********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
!
      DIMENSION MONTAB(24)
!
      DATA MONTAB/0,31,59,90,120,151,181,212,243,273,304,334,           &
     &            0,31,60,91,121,152,182,213,244,274,305,335/
!
      data jd1900/ 2415019/
      data jd2000/ 2451543/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!      write(6,*) 'ymdtis: IYIMID,IHIMIS ', IYIMID,IHIMIS
!
      IADD=0
      IYR = MOD( IYIMID/10000, 100 )
!
      IF( iyr .GE. 50 ) THEN
         JDCENT = jd1900
         ileap = 1
      ELSE
         JDCENT = jd2000
         ileap = 0
      ENDIF
!
!       write(6,*) 'ymdtis: IYEAR = ', iyr, '  JDCENT = ',jdcent
!
!     ....note:  1900 is not a leap year, but 2000 is a leap year
      IYR4=(IYR+3)/4 - ileap
!
      IJD=365*IYR+IYR4
!      write(6,*) 'ymdtis: years ijd ',ijd
      IMO=MOD(IYIMID,10000)
      IMO=IMO/100
      IF(MOD(IYR,4).EQ.0) IADD=12
      IJD=IJD+MONTAB(IMO+IADD)
!      write(6,*) 'ymdtis: + mon ijd', ijd
      ID=MOD(IYIMID,100)
!
      IJD=IJD+ID+ jdcent
!      write(6,*) 'ymdtis: COMPLETE J.D. ', ijd
      IH=IHIMIS/10000
      IM=IHIMIS-IH*10000
      IM=IM/100
      IS=MOD(IHIMIS,100)
      ISEC=(IJD-INT(TMGDN1-0.1))
!      ISEC=(IJD-2430000)*86400+IH*3600+IM*60+IS
!       write(6,*) 'ymdtis: isec ', isec
      SEC= ISEC*86400.D0
!       write(6,*) 'ymdtis: sec ', sec
      SEC= SEC +IH*3600.D0+IM*60.D0+ DBLE(IS)
!       write(6,*) 'ymdtis: iyimid  ', iyimid, '    J.D.  ', ijd
!       write(6,*) 'ymdtis: sec ', sec
       isec = sec
      RETURN
      END
