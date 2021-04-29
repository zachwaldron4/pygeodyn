!$XEPHEM
      SUBROUTINE XEPHEM(AA,ISATID,MJDSM,XI,PVBUF,NM,FSECM,PVBUFR,NDIM2,&
     &                       L_error,LFIND )
!********1*********2*********3*********4*********5*********6*********7**
! XEPHEM           04/10/92                      PGMR - BILL EDDY
!
! FUNCTION   CONTROLS READING OF EXTERNAL EPHEMERIS AND INTERPOLATION
!            OF TO GET EPHEMERIS POINTS AT THE REQUESTED TIMES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA       I    A    DYNAMIC ARRAY FOR REAL VARIABLES
!   ISATID   I    S    SATELLITE ID FOR REQUESTED SATELLITE EPHEMERIS
!   MJDSM    I    S    TIME FOR WHICH EPHEMERIS HAS BEEN REQUESTED.
!                      THIS IS THE INTEGER PART OF THE TIME, SEE
!                      FSECM FOR THE FRACTIONAL PART.
!   XI       O    A    OUTPUT ARRAY CONTAINING INTERPOLATED EPHEMERIS
!
!   PVBUF         A    DOUBLE BUFFER USED TO HOLD AS MANY EPHEMERIS
!                      RECORDS AS INDICATED BY NEPPBK IN COMMON
!                      XEPHMI. (DIMENSION IS 6,NXPSAT,NEPPBK,2)
!   NM       I    S    NUMBER OF MEASUREMENTS IN THE BLOCK
!
!   FSECM    I    A    FRACTIONAL PART OF TIME FOR WHICH THE EPHEMERIS
!                      HAS BEEN REQUESTED. TOTAL TIME IS GIVEN BY
!                      ADDING MJDSM AND FSECM. FSECM MAY BE GREATER
!                      ONE.
!   PVBUFR        A    BUFFER USED TO READ ONE LOGICAL RECORD FROM THE
!                      EXTERNAL EPHEMERIS FILE. LENGTH OF PVBUFR IS
!                      NXPSAT*6+1 WORDS.
!   NDIM2    I    S    SECOND DIMENSION OF XI ARRAY
! COMMENTS
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE

      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CORA02/KFSCTB,KFSCTC,KFSCTE,KDSECT,                        &
     &              KFSECT,KS    ,KCIP  ,KCIV  ,                        &
     &       KXTN  ,KXSM  ,KXTK  ,KXSJ  ,KXTI  ,KXTKP ,                 &
     &       KVTN  ,KVSM  ,KVTK  ,KVSJ  ,KVTI  ,KVTKP ,                 &
     &       KSATLT,KSATLN,KSATH ,KCOSTH,KSINTH,                        &
     &       KPXPFM,KPXPFK,KPXPFJ,KTRBF ,KFSTRC,                        &
     &       KFSTRE,KDSCTR,KFSCVS,KXSMBF,KXSKBF,KXSJBF,                 &
     &       KRSSV1,KRSSV2,KRSSV3,KTPMES,KACOEF,KACTIM,                 &
     &       KXTNPC,KXSMPC,KXTKPC,KXSJPC,KXTIPC,KXTKPP,                 &
     &       KRLRNG,KASTO,KASTP,NXCA02
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/LDUAL/LASTSN,LSTSNX,LSTSNY
      COMMON/XEPHL2/  L_call_sumcmp   ! jjm 20150413
      COMMON/XEPHMI/IBLKXE,IBLKXL,IBLKXP(2),IXEPRW,IXEPRD,              &
     &              IXEPB1,IXEPB2,IXBFS(2),IXBFE(2),                    &
     &              MJXEPS,IXPSAT(200),NXPSAT,MXPBFR,                   &
     &              NEPPBK,IXEPEF,IUNTXE,NXXEPH
      COMMON/XEPHMR/XEPDEL,XEPSFR
!
!         XEPSFR - FRACTION OF SECONDS
!
      DIMENSION AA(1)
      DIMENSION FSECM(NM)
      DIMENSION PVBUF(6,NXPSAT,NEPPBK,2),PVBUFR(MXPBFR)
      DIMENSION XI(MINTIM,NDIM2,2)
      DIMENSION PV10(10,6),PVINT(6)
!
      DATA EPS/1.0D-6/
      DATA LTOR/.FALSE./

      LOGICAL :: L_error

!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
       L_error = .FALSE.
       LFIND=.FALSE.

       KTRJX=KVTKP+3*MINTIM
       KTRJV=KTRJX+3*MINTIM
!
! ON THE FIRST CALL, READ EPHEMERIS HEADER RECORD
!
      IF(MJXEPS.LE.0) THEN
      IBUF=-1
      IXPT=5
      IXPT1=1
      IXPT10=10
      CALL XEPHRD(AA,IBUF,PVBUFR,PVBUF,IXPT,IXPT1,IXPT10)
      ENDIF
!
! Search sat id list to determine if requested sat is on the ephemeris
!
      ISAT=0
      DO 400 I=1,NXPSAT
      IF(ISATID.EQ.IXPSAT(I)) THEN
         ISAT=I
         LFIND=.TRUE.
         GO TO 500
         ENDIF
  400 END DO
!
! Requested satellite not found RETURN
         RETURN
! Requested satellite not found. Terminate run
!        WRITE(IOUT6,90100)
!        WRITE(IOUT6,90200) ISATID,(IXPSAT(J),J=1,NXPSAT)
!        STOP
!
! LOOP OVER THE NM MEASUREMENTS IN A BLOCK
!
  500 DO 5000 INM=1,NM
      TIME=MJDSM+FSECM(INM)
!  Determine the nearest ephemeris point that is earlier than the
!  required time.
      TIMDIF=TIME-MJXEPS-XEPSFR
      IXPT  = INT( TIMDIF/XEPDEL + EPS )  + 1
      IXPT1 = IXPT-4
      IXPT10= IXPT+5
         IF(IXPT1.LT.1 ) THEN
!        First point needed for interpolation is prior to start of
!        ephemeris. Allow interpolation as long as IXPT is at least
!        two points from the start of the 10 points.

             IF( IXPT .LT. 2 ) THEN

              !write(iout6,'(/A,1x, I12, 2( 1x, E24.16)  )') &
              !     'xephem:2 mjdsm, fsecm(inm), time     ', &
              !               mjdsm, fsecm(inm), time
              !write(iout6,'(A,1x, I12, 3( 1x, E24.16)  )') &
              !     'xephem:2 mjxeps, time, xepsfr, timdif', &
              !               mjxeps, time, xepsfr, timdif
              !write(iout6,'(A,4(1x, I10)  )') &
              !     'xephem:2  ixpt, ixpt1, ixpt10 ', &
              !                ixpt, ixpt1, ixpt10
              !flush(iout6)


              ! if the error occurs on the call from sumcmp, ignore it an
              ! the error is likely due to not having any data for this s
              ! for other calls, stop if the error occurs  jjm 20150413

              !original STOP

              L_error = .TRUE.


              if( L_call_sumcmp ) then
                  RETURN
              else
                  WRITE(IOUT6,90100)
                  WRITE(IOUT6,90300) MJDSM,FSECM(INM),MJXEPS
                  stop
              endif !  L_call_sumcmp

          ENDIF ! IXPT < 2

          IXPT1=1
          IXPT10=10

      ENDIF  ! IXPT1 < 1

  600 CONTINUE
      IBUF=0


!
      DO 2000 IB=1,2
! determine if the requested point is in an existing buffer
      IF(IXPT.LT.IXBFS(IB) .OR. IXPT.GT.IXBFE(IB)) GO TO 2000
! requested point is in buffer IB
      IBUF=IB
! check if all 10 points needed for interpolation are in IBUF
      IF(IXPT1.GE.IXBFS(IBUF) .AND. IXPT10.LE.IXBFE(IBUF)) THEN
!        the 10 points are available.
         ISTART= IXPT1-IXBFS(IBUF)
         NPTS=10
         DO 1300 I=1,NPTS
         ISTART=ISTART+1
         DO 1300 J=1,6
         PV10(I,J)=PVBUF(J,ISAT,ISTART,IBUF)
 1300    CONTINUE
         GO TO 4500
         ENDIF
!  check if the two available buffers are contiguous and if they are
!  check if the 10 points are in the combined two buffers
      IF(IXBFE(1)+1 .EQ. IXBFS(2) .OR. IXBFE(2)+1 .EQ. IXBFS(1) ) THEN
        IF(IXPT1  .GE. IXBFS(IXEPB1) .AND.                              &
     &     IXPT10 .LE. IXBFE(IXEPB2)) GOTO 1400
        ENDIF
!  the requested point is in buffer IBUF, however, the 10 points needed
!  for the interpolation are not all available so load another buffer
!
      CALL XEPHRD(AA,IBUF,PVBUFR,PVBUF,IXPT,IXPT1,IXPT10)
 1400 CONTINUE
!
! load the appropriate points from the two buffers starting with the
! buffer containing the earlier data
      ISTART= IXPT1-IXBFS(IXEPB1)
      NPTS=IXBFE(IXEPB1)-IXPT1+1
      NPTS=MIN(NPTS,10)
      NPTS=MAX(NPTS,1 )
      DO 1510 I=1,NPTS
      ISTART=ISTART+1
      DO 1500 J=1,6
      PV10(I,J)=PVBUF(J,ISAT,ISTART,IXEPB1)
 1500 END DO
 1510 END DO
! load remainder of points from the later buffer
      NPTS2=10-NPTS
      IF (NPTS2.EQ.0) GO TO 4500
      ISTART=0
      NPTS1=NPTS+1
      DO 1560 I=NPTS1,10
      ISTART=ISTART+1
      DO 1550 J=1,6
      PV10(I,J)=PVBUF(J,ISAT,ISTART,IXEPB2)
 1550 END DO
 1560 END DO
      GO TO 4500
 2000 END DO
!
!  Requested point was not in either buffer so load more data
      IBUF=0
      CALL XEPHRD(AA,IBUF,PVBUFR,PVBUF,IXPT,IXPT1,IXPT10)
      GO TO 600
 4500 CONTINUE
!
!  Perform interpolation. First compute the time from the first point
!  in the 10 point interpolation buffer
      TIMDIF=(MJDSM-MJXEPS)+(FSECM(INM)-XEPSFR)-(XEPDEL*(IXPT1-1))
      CALL XEPINT(XEPDEL,TIMDIF,PV10,PVINT)
      INMK=INM+KTRJX-1
      AA(INMK         )=PVINT(1)
      AA(INMK+  MINTIM)=PVINT(2)
      AA(INMK+2*MINTIM)=PVINT(3)
      AA(INMK+3*MINTIM)=PVINT(4)
      AA(INMK+4*MINTIM)=PVINT(5)
      AA(INMK+5*MINTIM)=PVINT(6)
 5000 END DO
! Transform true of date vector to true of reference
      CALL SATUPD(MJDSM,FSECM,AA(KTRJX),XI,AA,NM,MINTIM,.TRUE.,.TRUE.   &
     &,II,0,1)
      CALL SATUPD(MJDSM,FSECM,AA(KTRJV),XI(1,1,2),AA,                   &
     &             NM,MINTIM,.FALSE.,.TRUE.,II,0,1)
!
      IF(.NOT.LTOR) RETURN
      CALL SATUPD(MJDSM,FSECM,XI,XI,AA,NM,MINTIM,.FALSE.,.FALSE.        &
     &,II,0,1)
      CALL SATUPD(MJDSM,FSECM,XI(1,1,2),XI(1,1,2),AA,                   &
     &             NM,MINTIM,.FALSE.,.FALSE.,II,0,1)
!!!!  CALL TRNTRJ(NM,MINTIM,AA(KTRJX),XI)
!!!!  CALL TRNTRJ(NM,MINTIM,AA(KTRJV),XI(1,1,2))
      RETURN
90100 FORMAT(1X,'EXECUTION TERMINATING IN SUBROUTINE XEPHEM')
90200 FORMAT(1X,'EPHEMERIS FOR REQUESTED SATELLITE ( ',I7,')',          &
     & ' NOT AVAILABLE ON EXTERNAL EPHEMERIS.'/1X,(10I8))
90300 FORMAT(1X,'REQUESTED TIME FOR INTERPOLATION (',I10,E15.8,         &
     & ') IS TO EARLY.'/1X,'UNABLE TO GET THE NECESSARY 10 POINTS',     &
     & ' TO PERFORM THE INTERPOLATION'/1X,'START TIME FOR EPHEMERIS IS',&
     & I10)
      END
