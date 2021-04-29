!$PARTRD
      SUBROUTINE PARTRD(PMPP,LEOF)
!********1*********2*********3*********4*********5*********6*********7**
! PARTRD           00/00/00            0000.0    PGMR - SBL
!
!
! FUNCTION:  READS A SET OF PARTIALS FOR A PARTICULAR MEASUREMENT, FOR
!            PARTITIONED NORMAL EQUATION RUNS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPP    I/O   A    FULL ARRAY OF PARTIALS FOR A PARTICULAR
!                      MEASUREMENT.
!   LEOF    I/O   S    FLAG TO INDICATE IF ALL PARTIAL DERIVITIVE FILES
!                      HAVE BEEN READ, .TRUE. = ALL FILES HAVE BEEN READ
!
! COMMENTS:  THE SET OF PARTIALS ONLY INCLUDES THOSE NOT SUMMED INTO THE
!            FIRST PARTITION OF THE PARTITIONED NORMAL EQUATIONS.
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CPARTI/MAXLP ,KORSIZ,MAXWRK,                               &
     &              IUNTP1,IUNTP2,IUNTPF,ISIZPF,IRECPF,NRECPF,          &
     &              NPDONE,NP1   ,NP2   ,NRECND,NPDIM ,NXPARI
      COMMON/CPARTL/LPART ,LPARTI,NXPARL
      DIMENSION PMPP(NPDIM )
!
!***********************************************************************
! SPART OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      LEOF=.FALSE.
      IRECPF=IRECPF+1
 1000 CONTINUE
! READ A PARTIAL DERIVATIVE RECORD
      READ(IUNTPF,END=3000) PMPP
      RETURN
 3000 CONTINUE
! CLOSE THE FILE PRESENTLY OPEN.
      REWIND IUNTPF
! E.O.F. ENCOUNTERED. INCREMENT THE FILE COUNTER.
      IUNTPF=IUNTPF+1
      IF(IUNTPF.GT.IUNTP2) GO TO 9000
      IRECPF=1
      IF(IUNTPF.EQ.IUNTP2) NRECPF=NRECND
      GO TO 1000
! PARTIAL DERIVATIVE FILES EXHAUSTED.
 9000 CONTINUE
      LEOF=.TRUE.
      RETURN
      END
