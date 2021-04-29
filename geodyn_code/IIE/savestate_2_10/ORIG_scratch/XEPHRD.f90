!$XEPHRD
      SUBROUTINE XEPHRD(AA,IBUF,PVBUFR,PVBUF,IXPT,IXPT1,IXPT10)
!********1*********2*********3*********4*********5*********6*********7**
! XEPHEM           04/10/92                      PGMR - BILL EDDY
!
! FUNCTION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA       I    A    DYNAMIC ARRAY FOR REAL VALUES. NEEDED FOR CALL
!                      TO UTCET TO PASS IN TIME INFORMATION.
!   IBUF     I    S    TELLS WHICH BUFFER THE REQUESTED DATA POINT IS IN
!                      IBUF=0 MEANS THE POINT WASN'T IN EITHER BUFFER
!                      IBUF=1 MEANS THE POINT WAS IN BUFFER ONE SO
!                             LOAD ADDITIONAL DATA INTO BUFFER 2
!                      IBUF=2 MEANS THE POINT WAS IN BUFFER TWO SO
!                             LOAD ADDITIONAL DATA INTO BUFFER 1
!   PVBUFR        A    BUFFER USED TO READ ONE LOGICAL RECORD FROM THE
!                      EXTERNAL EPHEMERIS FILE. LENGTH OF PVBUFR IS
!                      NXPSAT*6+1 WORDS.
!   PVBUF    O    A    DOUBLE BUFFER USED TO HOLD AS MANNY EPHEMERIS
!                      RECORDS AS INDICATED BY NEPPBK IN COMMON
!                      XEPHMI. (DIMENSION IS 6,NXPSAT,NEPPBK,2)
!   IXPT     I    S    LOGICAL RECORD NUMBER OF THE EXTERNAL EPHEMERIS
!                      POINT CLOSES TO (BUT PRIOR TO) TIME REQUESTED
!   IXPT1    I    S    LOGICAL RECORD NUMBER OF THE EXTERNAL EPHEMERIS
!                      POINT THAT WILL BE USED FOR THE FIRST OF THE
!                      TEN POINTS FOR THE HERMITIAN INTERPOLATION
!   IXPT10   I    S    LOGICAL RECORD NUMBER OF THE EXTERNAL EPHEMERIS
!                      POINT THAT WILL BE USED FOR THE LAST  OF THE
!                      TEN POINTS FOR THE HERMITIAN INTERPOLATION
!
! COMMENTS
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      CHARACTER*7 FILNAM
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
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/XEPHMI/IBLKXE,IBLKXL,IBLKXP(2),IXEPRW,IXEPRD,              &
     &              IXEPB1,IXEPB2,IXBFS(2),IXBFE(2),                    &
     &              MJXEPS,IXPSAT(200),NXPSAT,MXPBFR,                   &
     &              NEPPBK,IXEPEF,IUNTXE,NXXEPH
      COMMON/XEPHMR/XEPDEL,XEPSFR
!
      DIMENSION AA(1)
      DIMENSION PVBUFR(MXPBFR),PVBUF(6,NXPSAT,NEPPBK,2)
      DIMENSION TIN(1),TOUT(1)
!
      DATA C1/1.0D0/,EPS2/1.0D-2/
      DATA C1D6/1.0D6/,C0/0.0D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
      IF(IBUF.GT.0 .AND. IBUF.LE.2) THEN
!
! Determine which block of additional data is needed, given that the
! requested point was in the block pointed to by IBUF
!
         IBNEXT=3-IBUF
         IBLKXE=IBLKXP(IBUF)
         IBLKXL=IBLKXP(IBUF)
         IF(IXPT1 .LT. IXBFS(IBUF)) THEN
            IBLKXE=IBLKXE-1
            ENDIF
         IF(IXPT10 .GT. IXBFE(IBUF)) THEN
            IBLKXL=IBLKXL+1
            IF(IXEPEF.GT.0 .AND. IXPT10.GT.IXEPEF) GO TO 9000
            ENDIF
         GO TO 1000
         ENDIF
!
      IF(IBUF.EQ.0) THEN
!
! requested data point was not in either block. Determine whether one or
! two blocks will be required (since the 10 points may span more than on
! block)
         IBNEXT=IXEPB1
         IBLKXE=(IXPT1 -1)/NEPPBK+1
         IBLKXL=(IXPT10-1)/NEPPBK+1
         IBLKXP(IBNEXT)=IBLKXE
         IBLKXP(3-IBNEXT)=IBLKXL
         IF(IXEPEF.GT.0 .AND. IXPT10.GT.IXEPEF) GO TO 9000
         GO TO 1000
         ENDIF
!
      IF(IBUF.LT.0) THEN
!       this is an initialization call, read the header information
         IXEPB1=1
         IXEPB2=2
         IBLKXE=1
         IBLKXL=2
         IXEPRD=0
         IBNEXT=1
         IREC=1
         IXEPEF=0
         IU1=IUNTXE/10
         IU2=IUNTXE-IU1*10
         WRITE(FILNAM,FMT='(A3,I1,I1)')'ftn',IU1,IU2
         OPEN(IUNTXE,FILE=FILNAM,STATUS='OLD',FORM='UNFORMATTED')
         READ(IUNTXE,END=9000) PVBUFR
!         write(6,*) 'xephrd: pvbufr ', pvbufr
         ISATS=PVBUFR(1)
!         write(6,*) 'xephrd: isats ', isats
         IF(ISATS.NE.NXPSAT) THEN
            WRITE(IOUT6,90100)
            WRITE(IOUT6,90140) NXPSAT,ISATS
            STOP
            ENDIF
!  CONVERT TIME TO INTERNAL MJDS SYSTEM FROM YYMMDDHHMMSS FORMAT
!  AND THEN CONVERT TIME TO EPHEMERIS TIME
         TIME =PVBUFR(2)+EPS2
!         write(6,*) 'xephrd: time ', time
         IYMD=TIME/C1D6
         IHMS=TIME-IYMD*C1D6
!         write(6,*) 'xephrd: iymd, ihms ', iymd, ihms
         CALL YMDTIS(IYMD,IHMS,MJXEPS)
!         write(6,*) 'xephrd:  mjxeps ',  mjxeps
         TIN(1)=C0
         TOUT(1)=C0
         CALL UTCET(.TRUE.,1,MJXEPS,TIN,TOUT,AA(KA1UT))
         XEPSFR=TOUT(1)
         XEPDEL=PVBUFR(3)
         DO 200 J=1,ISATS
         IXPSAT(J)=PVBUFR(3+J)
  200    CONTINUE
         GO TO 1000
         ENDIF
!
! Terminate run since IBUF must be greater than 2 to get here
!
      WRITE(IOUT6,90100)
      WRITE(IOUT6,90120) IBUF
      STOP
!
! READ THE EXTERNAL EPHEMERIS FILE.
!
 1000 CONTINUE
!
! rewind the ephemeris file if the requested block(s) are prior to the l
! block read and read over the header record again
      IF(IBLKXE.LT.IXEPRD) THEN
         REWIND (IUNTXE)
         IXEPRW=IXEPRW+1
         IXEPRD=0
         IREC=1
         READ(IUNTXE,END=9000) PVBUFR
         ENDIF
!
! determine the number of blocks between the last block read and the
! earliest block required
      NBLKS=IBLKXE-IXEPRD
! if they are the same block check if a second block is required
      IF (NBLKS.EQ.0) GO TO 1800
! skip (read) up to the requested block
      NBSKIP=NBLKS-1
      IF(NBSKIP.GT.0) THEN
         IRD=0
         IREC=IXEPRD*NEPPBK
         DO 1510 I=1,NBSKIP
         IXEPRD=IXEPRD+1
         DO 1500 J=1,NEPPBK
         IREC=IREC+1
         READ(IUNTXE,END= 9000) PVBUFR
         TIME=PVBUFR(1)
!         write(6,*) 'xephrd: time ', time
!         IYMD=TIME/C1D6
!         IHMS=TIME-IYMD*C1D6
!         write(6,*) 'xephrd: iymd, ihms ', iymd, ihms
!         CALL YMDTIS(IYMD,IHMS,MJXEPS)
!         write(6,*) 'xephrd:  mjxeps ',  mjxeps
 1500    CONTINUE
!        test last point to make sure that no points were lost
         TEST=(IXEPRD*NEPPBK-1)*XEPDEL
         IF(ABS(TIME-TEST).GT.C1) THEN
            WRITE(IOUT6,90100)
            IREC=IXEPRD*NEPPBK
            WRITE(IOUT6,90110) IREC,TEST,TIME
            STOP
            ENDIF
 1510    CONTINUE
         ENDIF
!
! read a new block
!
      IREC=IXEPRD*NEPPBK
      IXEPRD=IXEPRD+1
      IBLKXP(IBNEXT)=IXEPRD
      IXBFS(IBNEXT)=(IXEPRD-1)*NEPPBK+1
      IXBFE(IBNEXT)=IXBFS(IBNEXT)+NEPPBK-1
      DO 1650 JBK=1,NEPPBK
      IREC=IREC+1
      READ(IUNTXE,END=2000) PVBUFR
      J=1
      DO 1610 JSAT=1,NXPSAT
      DO 1610 J6=1,6
      J=J+1
      PVBUF(J6,JSAT,JBK,IBNEXT)=PVBUFR(J)
 1610 CONTINUE
 1650 END DO
      IBNEXT=3-IBNEXT
      GO TO 1800
!
!
! read a second block if necessary
!
 1800 IF(IBLKXL.LE.IBLKXE) GO TO 3000
      IREC=IXEPRD*NEPPBK
      IXEPRD=IXEPRD+1
      IXBFS(IBNEXT)=(IXEPRD-1)*NEPPBK+1
      IXBFE(IBNEXT)=IXBFS(IBNEXT)+NEPPBK-1
      IBLKXP(IBNEXT)=IXEPRD
      DO 1950 JBK=1,NEPPBK
      IREC=IREC+1
      READ(IUNTXE,END=2000) PVBUFR
      J=1
      DO 1910 JSAT=1,NXPSAT
      DO 1910 J6=1,6
      J=J+1
      PVBUF(J6,JSAT,JBK,IBNEXT)=PVBUFR(J)
 1910 CONTINUE
 1950 END DO
      IBNEXT=3-IBNEXT
      GO TO 3000
 2000 CONTINUE
!
! End of file reached. Check if sufficient points for interpolation are
! available, if so set end of buffer pointer correctly
      IREC=IREC-1
      IXEPEF=IREC
      IF(IXPT10.GT.IREC) GO TO 9000
      IXBFS(IBNEXT)=(IXEPRD-1)*NEPPBK+1
      IXBFE(IBNEXT)=IREC
      IBNEXT=3-IBNEXT
!
!
! reset pointers and return
!
 3000 CONTINUE
      IXEPB1=1
      IF(IXBFS(2) .LT. IXBFS(1)) IXEPB1=2
      IXEPB2=3-IXEPB1
!      write(6,*) 'xephrd: pvbufr ', pvbufr
      RETURN
 9000 CONTINUE
      WRITE(IOUT6,90100)
      WRITE(IOUT6,90150) IXPT10,IXEPEF
      STOP
90100 FORMAT(1X,'EXECUTION TERMINATING IN SUBROUTINE XEPHRD')
90110 FORMAT(1X,'TIME ON EXTERNAL EPHEMERIS RECORD NUMBER ', I5,        &
     &          'IS OUT OF ORDER'/                                      &
     &       1X,'EXPECTED TIME =',E15.8, 'ACTUAL TIME ',E15.8)
90120 FORMAT(1X,'FATAL BUFFER ERROR IN SUBROUTINE XEPHRD'/              &
     & 'IBUF= ',I3)
90140 FORMAT(1X,'INCONSISTENT NUMBER OF SATELLITES ON EPHEMERIS FILE'/  &
     & 1X,'NUMBER EXPECTED FROM IIS INFORMATION = ',I3/                 &
     & 1X,'NUMBER FOUND IN HEADER RECORD OF EPHEMERIS FILE= ',I3)
90150 FORMAT(1X,'END OF FILE REACHED WHILE ATTEMPTING TO OBTAIN 10'/    &
     &       1X,'POINTS FOR THE INTERPOLATOR. THE LAST RECORD NEEDED'/  &
     &       1X,'WAS ',I5, 'LAST RECORD AVAILABLE WAS ',I5)
      END
