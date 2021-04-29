!$GPSBLOCK
      SUBROUTINE GPSBLOCK(ISATID,ISVN,IPRN,IBLK)
!********1*********2*********3*********4*********5*********6*********7**
! GPSBLK                      02/02/12            PGMR - D. Pavlis
!
!   FUNCTION       EXTRACTS SVN, PRN, and BLK FROM GPS SATELLITE ID
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   ISATID   I      S    INTEGER EPHEMERIS SECONDS SINCE GEODYN REF TIME
!   ISVN     O      S    SPACE VEHICLE NUMBER
!   IPRN     O      S    PSEUDO-RANDOM NOISE SEQUENCE
!   IBLK     O      S    SATELLITE BLOCK NUMBER
!                        1 = I
!                        2 = II
!                        3 = IIA
!                        4 = IIR
!                        5 = IIR-M
!                        6 = IIF
!                        7 = IIIA  (assuming =7 for 2014 launch)
!
! GPS SAT ID FORMAT:     YYSSPPB
!                   e.g. 9023322
! LAUNCH AFTER 1999:     05
!
!                  where YY = launch year
!                        SS = SVN #
!                        PP = PRN #
!                         B = BLOCK
!
!**********1*********2*********3*********4*********5*********6*********7
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DATA ONE/1.D0/
      SAVE
!


       IPRN=MOD(ISATID,1000)/10
       IBLK=MOD(ISATID,10)
!      IF(IBLK.EQ.3) THEN
!       IF(ISATID.EQ.6062253) IBLK=4
!       IF(ISATID.EQ.6163013) IBLK=4
!      ENDIF

      RETURN
      END
