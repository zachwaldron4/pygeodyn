      IMPLICIT REAL*8 (A-H,O-Z), LOGICAL (L)                                    
C
C   COMPILE THISPROGRAM SO THAT INTEGERS HAVE 8 BYTES (64 BITS)
C
      PARAMETER(MAXOBS=2000)
      PARAMETER(MAXBLK=100)
C
C
C
      DIMENSION MJDS(MAXBLK),FSEC(MAXBLK),X(MAXBLK,3)
      DIMENSION BFOUT(200,10)                                                  
      DIMENSION BUFW(10)
      DIMENSION FSECX(1),IYMD(1),IHM(1),SEC(1)
      DATA NLOG/0/
      DATA IPTO/0/
      DATA BFOUT/2000*0.D0/
      DATA PRE9/16777214.D0/
      DATA PRE8/16777215.D0/
      DATA VLIGHT/299792458.D0/
      DATA WRD5/0.000003D0/
      DATA WRD8/0.00023D0/
      DATA SATID/1807001.D0/
      DATA SIGMA/1.D0/
C      
      INQUIRE(EXIST=LEXIST,FILE='TRAJ.txt')
      IF(LEXIST) THEN
         OPEN(UNIT=10,FILE='TRAJ.txt',FORM='FORMATTED',STATUS='OLD')
      ELSE
         WRITE(6,6000) 
         STOP
      ENDIF
C      
      INQUIRE(EXIST=LEXIST,FILE='G2B_PCE')
      IF(LEXIST) CALL SYSTEM('rm G2B_PCE')
      OPEN(UNIT=40,FILE='G2B_PCE',FORM='UNFORMATTED',STATUS='NEW')
C
 1000 CONTINUE                                                                  
C
C READ IN A CHUNK OF MAXBLK POINTS FROM SEQUENTIAL FILE
C LOAD THEM INTO HOLFING ARRAYS
C
      NM=0
      LEND=.FALSE.
      DO 1100 I=1,MAXBLK
      READ(10,*,END=1200) MJDS(I),FSEC(I),X(I,1),X(I,2),X(I,3)
      NM=NM+1
 1100 CONTINUE                                                                  
 1200 CONTINUE                                                                  
      IF(NM.LT.MAXBLK) LEND=.TRUE.
      IF(NM.EQ.0) GO TO 3000
C
C  PUT THE CHUNK OF MAXOB POINTS INTO G2B FORMAT
C  DO THIS FOR X, Y AND Z
C
      DO 2000 IMEAS=1,3
      BUFW(1)=DFLOAT(MJDS(1))
      BUFW(2)=FSEC(1)
      BUFW(3)=DFLOAT(MJDS(NM)-MJDS(1))+FSEC(NM)-FSEC(1)
      BUFW(4)=VLIGHT
      BUFW(5)=WRD5+DFLOAT(IMEAS)
      BUFW(7)=DFLOAT(NM)
      BUFW(8)=WRD8
      BUFW(9)=PRE9
      BUFW(10)=-9000000.D0
C
      CALL BUFOUT(IPTO,BUFW,BFOUT)
C
      DO K=1,10
       BUFW(K)=0.D0
      ENDDO
      BUFW(5)=2000.D0
      BUFW(8)=SATID
      BUFW(9)=PRE8
      BUFW(10)=-8000000.D0
      CALL BUFOUT(IPTO,BUFW,BFOUT)
C
C
      DO 1400 I=1,NM
      DO K=1,10
       BUFW(K)=0.D0
      ENDDO
      BUFW(1)=X(I,IMEAS)
      BUFW(6)=DFLOAT(MJDS(I)-MJDS(1))+FSEC(I)-FSEC(1)
      BUFW(7)=SIGMA
      CALL BUFOUT(IPTO,BUFW,BFOUT)
 1400 CONTINUE
C
C
 2000 CONTINUE
      IF(.NOT.LEND) GO TO 1000                                                                
C
C
C
 3000 CONTINUE                                                                  
C
C
C
      IF(IPTO.GT.0) WRITE(40) BFOUT
      CLOSE(40)
      STOP
6000  FORMAT(' EXECITION TERMINATING. FILE TRAJ.txt NOT FOUND')
      END                                                                       
      SUBROUTINE BUFOUT(IPTO,BUFI,BUFO)
      IMPLICIT REAL*8(A-H,O-Z),LOGICAL(L)
      DIMENSION BUFI(10)
      DIMENSION BUFO(200,10)
      DATA NBLKO/0/
C
      DO 100 I=1,1
      IPTO=IPTO+1
      IF(IPTO.EQ.201) THEN
        NBLKO=NBLKO+1
        WRITE(6,78912) NBLKO
78912   FORMAT(' BLOCK # ',I6' WRITTEN')
        WRITE(40) BUFO
        DO J=1,2000
          BUFO(J,1)=0.D0
        ENDDO
        IPTO=1
      ENDIF
      DO J=1,10
        BUFO(IPTO,J)=BUFI(J)
      ENDDO
 100  CONTINUE
      RETURN
      END
C$YMDTIS
      SUBROUTINE YMDTIS(IYIMID,IHIMIS,ISEC)
C********1*********2*********3*********4*********5*********6*********7**
C YMDTIS           82/11/30            0000.0    PGMR - D. ROWLANDS
C                  90/07/90                      PGMR - LUCIA TSAOUSSI
C FUNCTION:
C
C  OLD VERSION        11/30/82          D. ROWLANDS
C                    CALCULATE THE INTEGER NUMBER OF UT SECONDS
C                    SINCE JD 2430000.5D0 TO  TIME IYIMID,IHIMIS
C
C  NEW VERSION        07/31/90          LUCIA TSAOUSSI
C                    EXTEND CALCULATION TO 21ST CENTURY AND EXTEND
C                    INPUT DATE TO YYYY MM DD (INSTEAD OF YY MM DD)
C                    USE DIFFERENT ALGORITHM FOR LEAP YEARS!
C
C I/O PARAMETERS:
C
C   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
C   ------  ---  ---   ------------------------------------------------
C   IYIMID   I         REFERENCE DATE
C   IHIMIS   I         REFERENCE TIME
C   ISEC     O         INTEGER UT SECONDS FROM JD 2430000.5 TO
C                      TIME IYIMID IHIMIS
C
C   RESTRICTIONS:    DATE MUST BE IN 20TH OR 21ST CENTURY
C
C********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT REAL (A-H,O-Z), LOGICAL (L)
      SAVE
      DIMENSION MONTAB(24),MYRAB(2)
      DATA MONTAB/0,31,59,90,120,151,181,212,243,273,304,334,
     .            0,31,60,91,121,152,182,213,244,274,305,335/
      DATA MYRAB/2415019,2451544/
C
C**********************************************************************
C START OF EXECUTABLE CODE ********************************************
C**********************************************************************
C
      IYR=IYIMID/10000
      IF (IYR.GT.99) THEN
        IYRH=MOD(IYR,100)
        IYRT=IYR/100
      ELSE
        IYRH=IYR
        IF (IYRH.GE.50) THEN
          IYRT=19
        ELSE
          IYRT=20
        END IF
      END IF
      IADD=0
      IYR4=(IYRH-1)/4
      IF(IYR.EQ.0) IYR4=-1
      IJD=365*IYRH+IYR4
      IMO=MOD(IYIMID,10000)
      IMO=IMO/100
      IF (IYRH.EQ.0) THEN
         IYEAR=IYRT*100+IYRH
         IF (MOD(IYEAR,400).EQ.0) IADD=12
      ELSE
         IF(MOD(IYRH,4).EQ.0) IADD=12
      END IF
      IJD=IJD+MONTAB(IMO+IADD)
      ID=MOD(IYIMID,100)
      IJD=IJD+ID+MYRAB(IYRT-18)
      IH=IHIMIS/10000
      IM=IHIMIS-IH*10000
      IM=IM/100
      IS=MOD(IHIMIS,100)
      ISEC=(IJD-2430000)*86400+IH*3600+IM*60+IS
      RETURN
      END
C$ADDYMD
      SUBROUTINE ADDYMD(IYMD,IDAY)
*********1*********2*********3*********4*********5*********6*********7**
* NAME              ADDYMD
*
* FUNCTION          TO ADD OR SUBTRACT DAYS FROM A DATE IN THE FORM
*                   YYMMDD AND TO PROVIDE THE USER WITH THE NEW DATE
*
* INPUT PARAMETERS:
*
*
*                  IYMD    - SIX DIGIT DATE IN THE FORM YYMMDD
*
*                  IDAY    - NUMBER OF DAYS TO BE ADDED OR SUBTRACTED
*                            FROM INPUT DATE
*
* OUTPUT PARAMETERS:
*                  IYMD    - NEW DATE AFTER IDAY ADDED TO INPUT DATE
*
*
* RESTRICTIONS      ....old and new dates must lie within the years:
*                          March 1, 1900 to Feb. 28, 2100
*
*********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
*
***********************************************************************
* START OF EXECUTABLE CODE ********************************************
***********************************************************************
*
      izero = 0
      call mjdymd( modjd, iymd, izero, 2 )
      newjd = modjd +IDAY
      call mjdymd( newjd, iymd, izero, 1 )
      if(iymd.lt.491231)iymd=iymd+1000000
*
      RETURN
      END
C$MJDYMD
      SUBROUTINE MJDYMD(MODJD,IYMD,IHMS,IFLAG)
*********1*********2*********3*********4*********5*********6*********7**
* MJDYMD           CREATION DATE 07/18/83     PGMR - D. SEVITSKI
*                       UPDATED  10/30/84     PGMR - B EDDY
*                       UPDATED  12/13/90     PGMR - J. MCCARTHY
*
* FUNCTION          CONVERT TIMES FROM MJD AND MJDS TO YMD AND
*                   VICE VERSA
*
* INPUT PARAMETERS:
*                   MODJD - MODIFIED JULIAN DATE        (IFLAG =1)
*                   MODJD - MODIFIED JULIAN DAY SECONDS (IFLAG =3 OR 4)
*                   IYMD  - YEAR,MONTH,DAY(YYMMDD)      (IFLAG =2)
*                   IFLAG - FLAG INDICATING INPUT AND OUTPUT
*                           TIME CONVERSION AS FOLLOWS:
*                              1 = MJD    TO YYMMDD
*                              2 = YYMMDD TO MJD
*                              3 = MJDS   TO YYMMDD
*                              4 = MJDS   TO YYMMDD HHMMSS
*
* OUTPUT PARAMETERS:
*                   MODJD - MODIFIED JULIAN DATE        (IFLAG =2)
*                   IYMD  - YEAR,MONTH,DAY(YYMMDD)      (IFLAG =1,3,4)
*                   IHMS  - HOUR,MINUTE, SECOND(HHMMSS) (IFLAG =4)
*
*
* RESTRICTIONS      ....all dates must lie within the years:
*                          March 1, 1900 to Feb. 28, 2100
*
*
* UPDATE HISTORY:
*
* 10/30/84  B EDDY     - OPTION TO OUTPUT HOUR MINUTES SECONDS ADDED
* 12/13/90  J McCarthy - capability to handle dates in 21th century added
*
***********1*********2*********3*********4*********5*********6*********7
*
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
*
*
*      parameter ( half =  0.5d0 ) , ( one = 1.d0 ),
*     1          ( seven = 7.d0 )  , ( ten = 10.d0 ),
*     2          ( twelve = 12.d0 ), ( d14 = 14.d0 )
      parameter ( half =  0.5d0 )
      parameter ( one = 1.d0 )
      parameter ( seven = 7.d0 )
      parameter ( ten = 10.d0 )
      parameter ( twelve = 12.d0 )
      parameter ( d14 = 14.d0 )
*
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
*
      DATA TGTYMD/410106.D0/
      DATA TMGDN2/30000.D0/
*
*      DATA MJD00/15019/,MD1901/15385/,MD1940/29629/
*      DATA IYMD01/010101/,IYMD40/400101/
*
***********************************************************************
* START OF EXECUTABLE CODE ********************************************
***********************************************************************
*
      IF(IFLAG.LT.1 .OR. IFLAG.GT.4) RETURN
*
      GOTO (100,200,300,300),IFLAG
*
*     MJD TO YMD
*
100   CONTINUE
*
      xjd = modjd + xjd0
*     write(6,*)'mjdymd: xjd ', xjd
      c = INT( xjd + half ) + 1537
*     write(6,*)'mjdymd: c ',c
      nd = INT( (c - d122) / d36525 )
*     write(6,*)'mjdymd: nd ', nd
      e = INT( d36525 * nd )
*     write(6,*)'mjdymd: e ', e
      nf = INT( ( c - e ) / d30600 )
*
      id = c - e - INT( d30600 * nf ) + frac( xjd + half )
*     write(6,*)'mjdymd: id ',id
      im = nf - 1 - 12 * INT( nf / 14 )
*     write(6,*)'mjdymd: im ', im
      iy = nd - 4715 - INT(  ( 7 + im ) / 10 )
*     write(6,*)'mjdymd: iy ', iy
*
*
*     ....need this mod to get 2-digit output years
*
      iy = MOD( iy, 100 )
*     write(6,*)'mjdymd: iy ', iy
*
      iymd = iy * 10000 + im * 100 + id
*
*     write(6,*) ' iy, im, id ', iy, im, id, '  mjd ', mjd
*
      RETURN
*
*     YMD TO MJD
*
200   CONTINUE
      IY=IYMD/10000
      IM=(IYMD-IY*10000)/100
      ID=IYMD-IY*10000-IM*100
*
*
      if( iy .lt. 1900 ) then
         if( iy .gt. 50 ) then
            y = 1900 + iy - 1
*     write (6,*)'in 200 block: y ', y
         else
            y = 2000 + iy - 1
*     write (6,*)'in 200 block: y ', y
         endif
      endif
*
      if( im .gt. 2 ) then
         m = im
*   write (6,*)'in 200 block: m ', m
         y = y + 1
      else
         m = im + 12
*   write (6,*)'in 200 block: m ', m
      endif
*
      xjd = INT( d36525 * y ) + INT( d30600 * (m + 1) ) + ib
     &    + d17209 + id
*     write(6,*) ' xjd ', xjd
*
      modjd = xjd - xjd0
*     write(6,*) ' modjd ', modjd
*
*     ....below line not needed for iflag=2
ccc      fsec = ih * d3600 + imin * d60 + sec
*
*     write(6,*) ' iy, im, id ', iy, im, id, '  mjd ', mjd
*
      RETURN
*
*     MJDS TO YYMMDD (AND HHMMSS IF IFLAG=4)
*
300   CONTINUE
*     CONVERT FROM MJDS TO MJD
*     ....mjds = 86400 * ( jd - geodyn reference time in jd )
*     ....     = 86400 * ( mjd - geodyn reference time in mjd )
      IDY=(MODJD/86400)+INT(TMGDN2+0.1)
*
*     CONVERT FROM MJD TO YMD (SAME AS ABOVE)
*
      xjd = IDY + xjd0
      c = INT( xjd + half ) + 1537
      nd = INT( (c - d122) / d36525 )
      e = INT( d36525 * nd )
      nf = INT( ( c - e ) / d30600 )
*
      id = c - e - INT( d30600 * nf ) + frac( xjd + half )
      im = nf - 1 - 12 * INT( nf / 14 )
      iy = nd - 4715 - INT(  ( 7 + im ) / 10 )
*
*     ....need this mod to get 2-digit output years
*
      iy = MOD( iy, 100 )
*
      iymd = iy * 10000 + im * 100 + id
*
*      write(6,*) ' iy, im, id ', iy, im, id, '  mjd ', mjd
*
*
      IF(IFLAG.EQ.3) RETURN
*
      ISEC  =MOD(MODJD,86400)
      IH    =ISEC/3600
      IM    =(ISEC-IH*3600)/60
      IS    =ISEC-IH*3600-IM*60
      IHMS  =IH*10000+IM*100+IS
      RETURN
      END
C$YMDHMS
      SUBROUTINE YMDHMS(MJDS,FSEC,IYMD,IHM,SEC,NPTS)
*********1*********2*********3*********4*********5*********6*********7**
* YMDHMS           00/00/00            0000.0    PGMR - ?
*
*
* FUNCTION:
*
* I/O PARAMETERS:
*
*   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
*   ------  ---  ---   ------------------------------------------------
*   MJDS
*   FSEC
*   IYMD
*   IHM
*   SEC
*   NPTS
*
*
* COMMENTS:
*
*
*********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION FSEC(NPTS),IYMD(NPTS),IHM(NPTS),SEC(NPTS)
*     DATA IYMDRF/410106/
      DATA SECMIN/6.0D1/
      DATA TGTYMD/410106.D0/
      DATA TMGDN2/30000.D0/
      DATA SECDAY/86400.D0/
*
************************************************************************
* START OF EXECUTABLE CODE *********************************************
************************************************************************
*
* COMPUTE MODIFIED JULIAN DAY = JULIAN DAY - GEODYN REFERENCE TIME IN JD
      MJD=(DBLE(MJDS)+FSEC(1))/SECDAY
* ADD ELAPSED DAYS SINCE GEODYN TIME TO REFERENCE CALENDAR DATE
      IYMDRF=INT(TGTYMD+0.001)
      IYMD0=IYMDRF
      CALL ADDYMD(IYMD0,MJD)
* COMPUTE START TIME ELAPSED SECONDS WITHIN DAY
      ISEC0=MJDS-MJD*86400
      SEC0=DBLE(ISEC0)
* ADD TO ELAPSED SECONDS SINCE START TIME
      DO 1000 N=1,NPTS
      SEC(N)=SEC0+FSEC(N)
 1000 CONTINUE
* COMPUTE INTEGRAL ELAPSED DAYS FROM START DATE
      DO 2000 N=1,NPTS
      IYMD(N)=SEC(N)/SECDAY
 2000 CONTINUE
* SUBTRACT INTEGRAL DAYS FROM ELAPSED SECONDS
      DO 3000 N=1,NPTS
      SEC(N)=SEC(N)-DBLE(IYMD(N)*86400)
 3000 CONTINUE
* COMPUTE INTEGRAL ELAPSED MINUTES WITHIN DAY
      DO 4000 N=1,NPTS
      IHM(N)=SEC(N)/SECMIN
 4000 CONTINUE
* SUBTRACT INTEGRAL MINUTES FROM ELAPSED SECONDS
      DO 5000 N=1,NPTS
      SEC(N)=SEC(N)-DBLE(IHM(N)*60)
 5000 CONTINUE
* CONVERT INTEGRAL MINUTES TO HOURS AND MINUTES
      DO 6000 N=1,NPTS
*     IH=IHM(N)/60
*     IHM(N)=IHM(N)-IH*60
*     IHM(N)=IHM(N)+IH*100
      IHM(N)=(IHM(N)/60)*40+IHM(N)
 6000 CONTINUE
* REPLACE INTEGRAL DAYS SINCE START DATE WITH CALENDAR DATE
      IDAYS=0
      IYMDAY=IYMD0
      DO 8000 N=1,NPTS
      IF(IYMD(N).EQ.IDAYS) GO TO 7000
      IDAYS=IYMD(N)
      IYMDAY=IYMD0
      CALL ADDYMD(IYMDAY,IDAYS)
 7000 CONTINUE
      IYMD(N)=IYMDAY
 8000 CONTINUE
      RETURN
      END
C$FRAC
      FUNCTION FRAC(x)
      IMPLICIT REAL*8 (A-H,O-Z),LOGICAL(L)
C
C
      ix = int(x)
      frac = x - ix
      return
      end
