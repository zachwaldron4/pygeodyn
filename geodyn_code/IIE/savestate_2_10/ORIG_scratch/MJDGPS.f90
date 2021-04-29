!$MJDGPS
      SUBROUTINE MJDGPS(MJDS,FSECI,NWEEK,TTAG,AA,LET)
!********1*********2*********3*********4*********5*********6*********7**
! MJDGPS                      01/31/12            PGMR - D. Pavlis
!
!   FUNCTION       CONVERTS FROM MJDS TO GPS WEEK NUMBER/SECS OF WEEK
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   MJDS     I      S    INTEGER EPHEMERIS SECONDS SINCE GEODYN REF TIME
!   FSECI    I      S    FRACTIONAL REMAINING SECONDS
!   NWEEK    O      S    GPS WEEK SINCE 1980
!   TTAG     O      S    SECONDS OF GPS WEEK
!   AA       I      A    REAL ARRAY
!   LET      I      S    FLAG =.TRUE. INPUT ET TIME
!                             =.FALSE. INPUT IN UTC
!
! RESTRICTIONS      ....all dates must lie within the years:
!                          January 6, 1980 to December 31, 2099
!
!**********1*********2*********3*********4*********5*********6*********7
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CORA03/KRFMT ,KROTMT,KSRTCH,KCFSC ,KTHG  ,KCOSTG,KSINTG,   &
     &              KDPSI ,KEPST ,KEPSM ,KETA  ,KZTA  ,KTHTA ,KEQN  ,   &
     &              KDPSR ,KSL   ,KH2   ,KL2   ,KXPUT ,KYPUT ,KUT   ,   &
     &              KSPCRD,KSPSIG,KSTAIN,KSXYZ ,KSPWT ,KDXSDP,KDPSDP,   &
     &              KXLOCV,KSTNAM,KA1UT ,KPLTIN,KPLTVL,KPLTDV,KPUTBH,   &
     &              KABCOF,KSPEED,KANGFC,KSCROL,KSTVEL,KSTVDV,KTIMVL,   &
     &              KSTEL2,KSTEH2,KSL2DV,KSH2DV,                        &
     &              KAPRES, KASCAL, KASOFF,KXDOTP,KSAVST,               &
     &              KANGT , KSPDT , KCONAM,KGRDAN,KUANG ,KFAMP ,        &
     &              KOFACT, KOTSGN,KWRKRS,KANFSD,KSPDSD,KPRMFS,KSCRFR,  &
     &              KCOSAS,KSINAS,KDXTID,KALCOF,KSTANF,KNUTIN,KNUTMD,   &
     &              KPDPSI,KPEPST,KDXDNU,KDPSIE,KEPSTE,NXCA03
!
      DIMENSION AA(1)
      DIMENSION MJDL(15)
      DIMENSION MDAYS(12)
      DIMENSION FSEC(1),FSECU(1)
      DATA MJDL/44785,45150,45515,46246,47160,47891,48256,48803,49168, &
     &          49533,50082,50629,51178,53735,54831/
      DATA MDAYS/31, 28, 31, 30, 31, 30,              &
     &           31, 31, 30, 31, 30, 31/
      DATA ETGPS/54.184D0/

!
! CONVERT THE CURRENT DAY TO THE DAY FROM THE START OF GPS TIME
! (January 6, 1980)

      IF(MJDS.LT.1230681600) GOTO 100

      IF(LET) THEN
        FSECG=FSECI-ETGPS
      ELSE
        FSECU(1)=FSECI
        CALL UTCET(.TRUE.,1,MJDS,FSECI,FSEC,AA(KA1UT))
        FSECG=FSEC(1)-ETGPS
      ENDIF

      CALL MJDYMD(MJDS,iymd,ihms,4)
      CALL MJDYMD(MJD,IYMD,0,2)
!DEP YOU DISREGARDED THE LEAP SECONDS THAT UTC DIFFERS FROM GPS THESE
! ARE IADD BELOW.
      IADD=0
      DO J=1,15
      IF(MJD.GT.MJDL(J)) IADD=IADD+1
      ENDDO

      iyr=iymd/10000
      imon=(iymd-iyr*10000)/100
      iday=iymd - (iyr*10000 + imon*100)
      ihr=ihms/10000
      imin=(ihms-ihr*10000)/100
      isec=ihms - (ihr*10000 + imin*100)

       IF(IYR.GE.50) THEN
       IYR=1900+IYR
       ELSE
       IYR=2000+IYR
       ENDIF

! Determine number of complete days since Jan 6, 2008
! Years since 1980
      IGPSY = iyr-1980
! Convert to days
      IGPSD = IGPSY*365
! Add leap year days
      IGPSD = IGPSD + IGPSY/4 + 1
      NLY=MOD(IGPSY,4)
! Subtract day if before Feb 29 during a leap year
      IF(imon.LT.3.AND.NLY.EQ.0) IGPSD=IGPSD-1
      IF(imon.EQ.2.AND.iday.EQ.29) IGPSD=IGPSD+1
!
! Add/subtract days between Jan 6 and date
! 1) Date is February or later
      IF(imon.GT.1) THEN
        DO I=2,imon-1
          IGPSD=IGPSD+MDAYS(I)
        ENDDO
        IGPSD=IGPSD+iday-1
! Add the 26 full days in January
        IGPSD=IGPSD+26
      ELSE
! 2) January 6-31, but not complete iday, so no +1
        IF(iday.GE.6) IGPSD = IGPSD + iday-6
! 3) Between Jan1 and Jan5 - go to day start, add seconds later
        IF(iday.LT.6) IGPSD = IGPSD - (6-iday)
      ENDIF
!
! Have number of full days, calculate week number
      IGPSS=IGPSD*86400.D0
      NWEEK=IGPSS/604800.D0
      SREM=IGPSS/604800.D0 - NWEEK
! Remainder -> day of week
      IDOW=NINT(SREM*7)
      IWKSEC=IDOW*86400.D0 + ihr*3600.D0 + imin*60.D0 + isec
!     TTAG=IWKSEC*FSECG
      TTAG=IWKSEC+FSECG+IADD
!     write(6,*)' dbg TTAG ',IWKSEC,FSECG,IWKSEC+FSECG,FSECG
      GOTO 200
!
 100  CONTINUE
      WRITE(6,*)'ERROR IN MJDGPS'
      WRITE(6,*)'DATES MUST BE BETWEEN JAN 6, 1980 and DEC 31, 2099'
      STOP
!
 200  CONTINUE

      RETURN

      END
