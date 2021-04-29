!$PARWRT
      SUBROUTINE PARWRT(PMPP)
!********1*********2*********3*********4*********5*********6*********7**
! PARWRT           00/00/00            0000.0    PGMR - SBL
!
!
! FUNCTION:  WRITES A SET OF PARTIALS, FOR A PARTICULAR MEASUREMENT, FOR
!            PARTITIONED NORMAL EQUATION RUNS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPP    I/O   A    FULL ARRAY OF PARTIALS FOR A PARTICULAR
!                      MEASUREMENT
!
! COMMENTS:  THE SET OF PARTIALS ONLY INCLUDES THOSE NOT SUMMED INTO THE
!            FIRST PARTITION OF THE PARTITIONED NORMAL EQUATIONS
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      CHARACTER*5 FILNAM
!
      COMMON/CPARTI/MAXLP ,KORSIZ,MAXWRK,                               &
     &              IUNTP1,IUNTP2,IUNTPF,ISIZPF,IRECPF,NRECPF,          &
     &              NPDONE,NP1   ,NP2   ,NRECND,NPDIM ,NXPARI
      COMMON/CPARTL/LPART ,LPARTI,NXPARL
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!
      DIMENSION PMPP(NPDIM )
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! INCREMENT THE RECORD COUNTER.
      IRECPF=IRECPF+1
      IF(IRECPF.LE.NRECPF) GO TO 5000
! CLOSE THE FILE PRESENTLY OPEN.
      END FILE IUNTPF
      REWIND IUNTPF
! MAXIMUM RECORD COUNT WAS EXCEEDED. INCREMENT THE FILE COUNTER.
      IUNTPF=IUNTPF+1
      IF(IUNTPF.GT.IUNTP2) GO TO 9000
! REINITIALIZE RECORD COUNTER
      IRECPF=1
      IU1=IUNTPF/10
      IU2=IUNTPF-IU1*10
      WRITE(FILNAM,FMT='(A3,I1,I1)')'ftn',IU1,IU2
      OPEN(IUNTPF,FILE=FILNAM,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',   &
     &     status='new')
      WRITE(IOUT6,40000) IUNTPF
! OUTPUT THE PARTIAL DERIVATIVE RECORD.
 5000 CONTINUE
      WRITE(IUNTPF) PMPP
      RETURN
 9000 CONTINUE
! DISK SPACE IS FULL. PRINT MESSAGE AND STOP WITH CONDITION 16.
      WRITE(IOUT6,90000) IUNTP1,IUNTP2,NRECPF,NPDIM
      STOP 16
40000 FORMAT('UNIT ',I3,' OPENED FOR PARTITIONING PARTIAL FILE')
90000 FORMAT('0** PARWRT **  LOGICAL DISK UNITS',I3,' THRU',I3/         &
     &   ' HAVE EACH BEEN FILLED WITH',I8,' PARTIAL DERIVATIVE',        &
     &   ' RECORDS OF LENGTH',I5,' WORDS.'/' ADDITIONAL DISK SPACE',    &
     &   ' IS UNAVAILABLE.'/'0EXECUTION TERMINATING ABNORMALLY.'/1X )
      END
