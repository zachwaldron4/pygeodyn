!$GCMMOD
      SUBROUTINE GCMCOR(MJDSEC,FSEC,XX,YY,ZZ)
!********1*********2*********3*********4*********5*********6*********7**
! GCMCOR             00/00/00            8805.0    PGMR - JTW
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I         CURRENT INTEGRATION STEP TIME IN MJDS SECONDS
!   FSEC     I         FRACTIONAL PART OF CURRENT STEP TIME
!   XX      I/O        X COMPONENT OF TRACKING STATION
!   YY      I/O        Y COMPONENT OF TRACKING STATION
!   ZZ      I/O        Z COMPONENT OF TRACKING STATION
!
! COMMENTS:
!
!*********1*********2*********3*********4*********5*********6*********7
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/GCMMOD/GCMNPS,GCMT0,GCMPER(10),GCMAMP(30),GCMPHA(30),      &
     &              XXGCM

      DATA TWOPI /6.2831853071795864D0/
!
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!
!     COMPUTE THE YEAR AND  FRACTION OF THE YEAR

!      write(6,*)'jtw start gcmmod, xx',XX

      CALL FYEAR(MJDSEC,FSEC,YEAR)
      TDIFF=(YEAR-GCMT0)
!      write(6,*)'time',YEAR,GCMT0,TDIFF
!      write(6,*)'xparameters',GCMAMP(1),GCMPHA(1)
!      write(6,*)'GCMNPS',GCMNPS,INT(GCMNPS)
      DO I=1,INT(GCMNPS)
       DO J=1,3
        GCM=GCMAMP((I-1)*3+J)*COS(TDIFF*(TWOPI/GCMPER(I))-    &
     &                                           GCMPHA((I-1)*3+J))
!        GCMX=GCMAMP((I-1)*3+1)*DCOS(TDIFF*(TWOPI/GCMPER(I))-    &
!     &                                           GCMPHA((I-1)*3+1))
!        write(6,*)'GCMX',GCMX
!        GCMY=GCMAMP((I-1)*3+2)*DCOS(TDIFF*(TWOPI/GCMPER(I))-    &
!     &                                           GCMPHA((I-1)*3+2))
!        GCMZ=GCMAMP((I-1)*3+3)*DCOS(TDIFF*(TWOPI/GCMPER(I))-    &
!     &                                           GCMPHA((I-1)*3+3))
!
!         XX=XX-GCMX
!         YY=YY-GCMY
!         ZZ=ZZ-GCMZ
         IF (J.EQ.1) XX=XX-GCM
         IF (J.EQ.1) GCMXTMP=GCM
         IF (J.EQ.2) YY=YY-GCM
         IF (J.EQ.2) GCMYTMP=GCM
         IF (J.EQ.3) ZZ=ZZ-GCM
!!!         IF (J.EQ.3) write(52,2222)YEAR,GCMXTMP,GCMYTMP,GCM
       END DO
      END DO
 1100 CONTINUE
 2222 FORMAT(4(F17.6,2X))
      RETURN
      END
