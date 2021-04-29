!$RDTB2
      SUBROUTINE RDTB2(MJDSBL, FSECN, NM, MINTIM, XTN, OBS, OBS2,       &
     &           RAP, DECP, RAE, DECE )
!********1*********2*********3*********4*********5*********6*********7**
! RDTB2            89/04/89            0000.0    PGMR - JJM
!
!  FUNCTION:  COMPUTE SATELLITE POSITIONS AT THE TIMES OF THE
!             PHOTOGRAPHIC MEASUREMENTS OF PHOBOS AND DEIMOS
!             RIGHT ASCENSION AND DECLINATION
!
!  I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSBL   I    S    MODIFIED JULIAN DAY SECONDS OF THE FINAL
!                      RECEIVED TIME OF THE FIRST OBSERVATION IN THE
!                      BLOCK
!   FSECN    I    A    ELAPSED SECONDS FROM MJDSBL OF THE EVENT
!                      TIMES ASSOCIATED WITH EACH MEASUREMENT
!    NM      I    S    NUMBER OF MEASUREMENTS IN BLOCK
!    MINTIM  I    S    MAXIMUM NUMBER OF MEASUREMENTS PER BLOCK
!    XTN     O    A    SATELLITE POSITION AT EACH TIME POINT
!    OBS     O    A    RIGHT ASCENSION OF MOON WRT SATELLITE
!                      AT EACH TIME POINT
!    OBS2    O    A    DECLINATION OF MOON WRT SATELLITE
!                      AT EACH TIME POINT
!    RAP     O    A    RA  OF NORMAL TO SPACECRAFT ORBIT PLANE
!    DECP    O    A    DEC OF NORMAL TO SPACECRAFT ORBIT PLANE
!    RAE     O    A    RA  OF VECTOR FROM SPACECRAFT TO EARTH
!    DECE    O    A    DEC OF VECTOR FROM SPACECRAFT TO EARTH
!
! COMMENTS:   THE RAP, DECP, RAE, DECE VARIABLES ARE  USED ONLY
!             FOR DEBUG PURPOSES AT THIS TIME
!
! REFERENCES:    DUXBURY, T.C. AND J. D. CALLAHAN, "PHOBOS AND DEIMOS
!                MEASUREMENTS FROM VIKING", ASTRON. ASTROPHYS., V.201,
!                PP. 169-176, 1988
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!     ....MVDIM SHOULD BE LARGER THAN THE NUMBER OF MARINER DATA POINTS
!     ....INCREASE SIZE IF NOT BIG ENOUGH
!
      PARAMETER ( MVDIM = 500 )
!
!CC   CHARACTER*6 UNIT65
!
      COMMON/CORE2E/KORE2E(3,7),KSCL2E(3),NGRP2E,MACH2E,KCOM2E,NXCORE
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION FSECN(NM), XTN(MINTIM,3), OBS(NM), OBS2(NM),            &
     &   ET(MVDIM),   SATNO(MVDIM),  RADEG(MVDIM),  DECDEG(MVDIM),      &
     &   ETI(MVDIM),  SATNOI(MVDIM), RADEGI(MVDIM), DECDEI(MVDIM),      &
     &   XNUMI(MVDIM), XNUM(MVDIM), XPOSI(MVDIM,3), XPOS(MVDIM,3),      &
     &   RAPI(MVDIM), DECPI(MVDIM), RAEI(MVDIM), DECEI(MVDIM),          &
     &   RAP(MVDIM),  DECP(MVDIM),  RAE(MVDIM),  DECE(MVDIM),           &
     &   RAPO(MVDIM),  DECPO(MVDIM),  RAEO(MVDIM),  DECEO(MVDIM)
!
      DATA LFIRST/.TRUE./
      DATA ONE/1.D0/
      DATA TEN/1.D1/
!C    DATA UNIT65/'UNIT65'/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!
!     ....READ FILE ON FIRST CALL AND DUMP DATA INTO ARRAYS
!
      IF( LFIRST ) THEN
         K = 0
         LFIRST = .FALSE.
         ILAST  = 0
         IUNTMV = IUNT65
!-----------------------------------------------------------------------
!
!        ....READ IN DATA
!
         OPEN(IUNTMV,STATUS='OLD')
         REWIND IUNTMV
 1000    K = K+1
         IF( K .LE. MVDIM ) THEN
!
            READ(IUNTMV,END=2001) XNUMI(K),SATNOI(K), ETI(K),           &
     &        RADEGI(K),DECDEI(K),SIGMA,(XPOSI(K,J),J=1,3),             &
     &        RAPI(K), DECPI(K), RAEI(K), DECEI(K)
            GO TO 1000
!
         ELSE
!
!           ....ERROR EXIT
            WRITE(6, 10011)
            STOP
10011       FORMAT(///' **** RAN OUT OF ARRAY SPACE IN RDTB2 ****'///)
!
         ENDIF
!        ....END OF (K.LE.MVDIM) IF-THEN
!-----------------------------------------------------------------------
 2001    CONTINUE
!
!        ....KCOM2E IS 1 FOR 32 BIT, 2 FOR 64 BIT
!        ....DETERMINE WHICH COMPUTER IS IN USE AND CONVERT ACCORDINGLY
!
         NDATA = K-1
         IF( KCOM2E .EQ. 2 ) THEN
!
!           ....CONVERT ARRAYS TO 64 BIT VARIABLES
!
            CALL Q9IC64( XNUMI , XNUM , NDATA, ISTAT )
            CALL Q9IC64( SATNOI, SATNO, NDATA, ISTAT )
            CALL Q9IC64( ETI, ET,     NDATA, ISTAT )
            CALL Q9IC64( RADEGI, RADEG, NDATA, ISTAT )
            CALL Q9IC64( DECDEI, DECDEG, NDATA, ISTAT )
            CALL Q9IC64( XPOSI(1,1),XPOS(1,1),NDATA,ISTAT )
            CALL Q9IC64( XPOSI(1,2),XPOS(1,2),NDATA,ISTAT )
            CALL Q9IC64( XPOSI(1,3),XPOS(1,3),NDATA,ISTAT )
            CALL Q9IC64( RAPI, RAPO,  NDATA, ISTAT )
            CALL Q9IC64( DECPI, DECPO, NDATA, ISTAT )
            CALL Q9IC64( RAEI, RAEO,  NDATA, ISTAT )
            CALL Q9IC64( DECEI, DECEO, NDATA, ISTAT )
         ELSE
            DO 100 I=1, NDATA
               XNUM(I)  = XNUMI(I)
               SATNO(I) = SATNOI(I)
               ET(I)    = ETI(I)
               RADEG(I) = RADEGI(I)
               DECDEG(I)= DECDEI(I)
               XPOS(I,1) =XPOSI(I,1)
               XPOS(I,2) =XPOSI(I,2)
               XPOS(I,3) =XPOSI(I,3)
               RAPO(I) = RAPI(I)
               DECPO(I)= DECPI(I)
               RAEO(I) = RAEI(I)
               DECEO(I)= DECEI(I)
  100          CONTINUE
         ENDIF
!
!        ....LIST OUT DATA
!
         WRITE(6, 10111)
10111    FORMAT(//1X,'LIST OF INPUT FILE OF MARINER/VIKING DATA '//)
!
         DO 3001 K=1,NDATA
            WRITE(6, 10001) K, XNUM(K), SATNO(K), ET(K),                &
     &           RADEG(K), DECDEG(K), (XPOS(K,J),J=1,3),                &
     &           RAPO(K), DECPO(K), RAEO(K), DECEO(K)
10001         FORMAT(1X,'K=',I4,                                        &
     &        ' XNUM ',F6.1,'  SATNO ',F5.1,'  EPH TIME ',D24.16/       &
     &        7X,' RA, DEC IN DEG ',2D24.16/                            &
     &        7X,' SAT POS ',3D24.16/                                   &
     &        7X,' RA,DEC ORBIT NORMAL ',2F12.5,5X,'RA,DEC EARTH ',     &
     &           2F12.5)
 3001    CONTINUE
!
      ENDIF
!     ....END OF (LFIRST) IF-THEN
!
!-----------------------------------------------------------------------
!
 2000 CONTINUE
!
!     ....FIND THE FIRST TIME IN THE ARRAY
!
!     ....FIRST, TRY THE NEXT POINT IN THE DATA AND SEE IF IT MATCHES
!     ....THE INCOMING DATA POINT
!     ....IF THIS DOES NOT WORK, SEARCH FROM THIS POINT TO THE END OF
!     ....THE TABLES
!     ....IF THE MATCH STILL HAS NOT BEEN FOUND, TRY ONLY ONCE MORE. GO
!     ....TO THE START OF THE TABLES AND SEARCH TO THE END OF THE TABLES
!     ....IF THIS FAILS, DO AN ERROR EXIT
!
      ITRY = 1
      NSRCH = 0
      TDAT = MJDSBL + FSECN(1)
      IF( ILAST .GE. NDATA ) GO TO 3000
      ISUB1 = ILAST + 1
!
!     ....FIXTIM CONVERTS ET FROM TAPE TO TIME IN GEODYN II FORMAT
!
      CALL FIXTIM( ET(ISUB1), MJDSK, XTIM )
      TSTA = MJDSK + XTIM
      DTSTD = TSTA - TDAT
!
      NSRCH = NSRCH + 1
      IF( ABS( DTSTD ) .LT. ONE ) THEN
         ISUB = ISUB1
         GO TO 2200
      ENDIF
!
!     ....SEARCH FROM ISUB1 TO END
!
22000 CONTINUE
!
      DO 22100 K=ISUB1,NDATA
      ISUB = K
!
!     ....FIXTIM CONVERTS ET FROM TAPE TO TIME IN GEODYN II FORMAT
!
      CALL FIXTIM( ET(K), MJDSK, XTIM )
      TSTA = MJDSK + XTIM
      DTSTD = TSTA - TDAT
!
!
!     ....TEST IF TIMES OF DATA AND STATIONS MATCH
!
      NSRCH = NSRCH + 1
      IF( ABS( DTSTD ) .LT. ONE ) GO TO 2200
!
22100  CONTINUE
!
!     ....DID NOT FIND TIME -- IF ONLY ITRY=1, GO BACK TO BEGINNING
!
      IF( ITRY .LE. 1 ) THEN
         ISUB1 = 1
         GO TO 22000
      ELSE
         GO TO 3000
      ENDIF
!
!-----------------------------------------------------------
!
 2200 CONTINUE
!
!     ....NOW ISUB MATCHES FIRST MEASUREMENT TIME
!
      ILAST = ISUB + NM - 1
!
      DO 2201 K=1,NM
         IIS = ISUB-1+K
         DO 2250 J=1,3
            XTN(K,J) = XPOS(IIS,J)
 2250    CONTINUE
!
         RAP(K)  = RAPO(IIS)
         DECP(K) = DECPO(IIS)
         RAE(K)  = RAEO(IIS)
         DECE(K) = DECEO(IIS)
!
!
 2201 END DO
!
      RETURN
!
!-----------------------------------------------------------
 3000 CONTINUE
!
!      ....ERROR EXIT
!
      WRITE(6, 10002)
10002 FORMAT(///' DID NOT FIND TIME IN RDTB2 IN 2100 LOOP'/             &
     &          ' STOPPING PROGRAM IN SUBROUTINE RDTB2   '///)
      STOP
      END
