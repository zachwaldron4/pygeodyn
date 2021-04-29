!$CHTIME
      SUBROUTINE CHTIME(PARMV,CN,SN,FMJD,ITPGRC,ITPGRS,ITPUC,ITPUS,     &
     &ITPC,ITPS,ITPGPC,ITPGPS,ICNTP,ISNTP,ICNT,ISNT,ICNTT,ISNTT,        &
     &LC,LS,ISEQC,                                                      &
     &ISEQS,IBEG,IEND,IC,NAJCG,NAJSG,NAJCT,NAJST)
!********1*********2*********3*********4*********5*********6*********7**
! CHTIME            00/00/00            0000.0    PGMR - D.PAVLIS
!
! FUNCTION:      CHECKS FOR APPLICABLE TIME PERIOD FOR GRAVITY
!                THEN UPDATES THE C AND S COEFFICIENT ARRAYS.
!                ALSO IF TIME PERIOD GRAVITY IS APPLICABLE DETERMINE
!                IF ADJUSTMENT OF THE PARTICULAR COEFFICIENTS IS
!                REQUESTED. THEN UPDATE THE ICPNTT ARRAY NEEDED FOR
!                THE EXPLICIT PARTIALS IN SUBROUTINE RESPAR
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PARMV    I    A    PARAMETER VALUES
!   CN      I/O   A    GRAVITY C COEFFICIENTS
!   SN      I/O   A    GRAVITY S COEFFICIENTS
!   FMJD     I    S    TIME OF PRESENT OBSERVATION
!   ITPGRC   I    A    ARRAY WITH TIME PERIODS FOR C COEFFICIENT
!   ITPGRS   I    A    ARRAY WITH TIME PERIODS FOR S COEFFICIENT
!   LC       O    S    .TRUE. IF C COEFFICIENT APPLIES AT THIS TIME
!   LS       O    S    .TRUE. IF S COEFFICIENT APPLIES AT THIS TIME
!   ISEQC    O    S    SEQUENCE WITHIN THE TIMING ARRAY OF C COEFF
!   ISEQS    O    S    SEQUENCE WITHIN THE TIMING ARRAY OF S COEFF
!   IBEG     I    S    BEGINING POINTEER FOR PARMV ARRAY
!   IEND     I    S    END POINTEER FOR PARMV ARRAY
!   ICNTP   I/O   A    ARRAY WITH POINTERS OF ADJ C COEFF WITHIN CERTAIN
!                      TIME PERIODS TO THE TOTAL C ARRAY
!   ISNTP   I/O   A    ARRAY WITH POINTERS OF ADJ S COEFF WITHIN CERTAIN
!                      TIME PERIODS TO THE TOTAL S ARRAY
!   ICNT     I    A    POINTERS TO ADJUSTED C COEFFICIENTS
!   ISNT     I    A    POINTERS TO ADJUSTED S COEFFICIENTS
!   IC       I    S    TOTAL NUMBER OF C COEFFICIENTS
!   NAJCG    I    S    NUMBER OF ADJUSTED C COEFFICIENTS
!   NAJSG    I    S    NUMBER OF ADJUSTED S COEFFICIENTS
!   NAJCT    I    S    NUMBER OF ADJUSTED C COEFFICIENTS WITHIN TIME PER
!   NAJSG    I    S    NUMBER OF ADJUSTED S COEFFICIENTS WITHIN TIME PER
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      COMMON/TPGRAV/NTPGCU,NTPGCA,NTPGSU,NTPGSA,ITPGRV,NTIMEC,NTIMES,   &
     &              NXTPGR
      DIMENSION PARMV(1)
      DIMENSION CN(1),SN(1)
      DIMENSION ITPGRC(2,NTIMEC)
      DIMENSION ITPGRS(2,NTIMES)
      DIMENSION ITPUC(2,NTIMEC)
      DIMENSION ITPUS(2,NTIMES)
      DIMENSION ITPGPC(2,NTPGCA)
      DIMENSION ITPGPS(2,NTPGSA)
      DIMENSION ITPC(NTPGCA)
      DIMENSION ITPS(NTPGSA)
      DIMENSION ICNTP(1),ISNTP(1)
      DIMENSION ICNTT(1),ISNTT(1)
      DIMENSION ICNT(1),ISNT(1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! Initialize the pointer arrays
      DO 10 I=1,NAJCG
      ICNTP(I)=ICNT(I)
   10 END DO
      DO 20 I=1,NAJSG
      ISNTP(I)=ISNT(I)
   20 END DO
! initializations
      LC=.FALSE.
      LS=.FALSE.
      ISEQC=0
      ISEQS=0
      IF(NTIMEC.EQ.0)GOTO 300
!  first check C coefficients
      DO 100 I=1,NTIMEC
      FMC1=DBLE(ITPGRC(1,I))
      FMC2=DBLE(ITPGRC(2,I))
      IF(FMJD.GE.FMC1.AND.FMJD.LE.FMC2) THEN
! set logical flag to .true. and save the sequence number
! within the array of times.
      LC=.TRUE.
      ISEQC=I
      N=ITPUC(1,ISEQC)
      M=ITPUC(2,ISEQC)
      NOFFC=(N-1)*N/2+3*(N-1)+M+1
! find index for parmv array
      JINDC=IBEG+ISEQC-1
      CN(NOFFC)=PARMV(JINDC)
! Are ther any adjusted C coeffisients for this time period?
      DO 80 IJ=1,NTPGCA
      IF(ITPC(IJ).EQ.ISEQC) THEN
      IDEGC=ITPGPC(1,IJ)
      IORDC=ITPGPC(2,IJ)
      NOFFC=(IDEGC-1)*IDEGC/2+3*(IDEGC-1)+IORDC+1
      ICNTT(IJ)=NOFFC
      ENDIF
   80 END DO
      ENDIF
  100 END DO
  300 CONTINUE
      IF(NTIMES.EQ.0) GO TO 600
!  check S coefficients
      DO 400 I=1,NTIMES
      FMC1=DBLE(ITPGRS(1,I))
      FMC2=DBLE(ITPGRS(2,I))
      IF(FMJD.GE.FMC1.AND.FMJD.LE.FMC2) THEN
! set logical flag to .true. and save the sequence number
! within the array of times.
      LS=.TRUE.
      ISEQS=I
      N=ITPUS(1,ISEQS)
      M=ITPUS(2,ISEQS)
      NOFFS=(N-1)*N/2+3*(N-1)+M+1
! find index for parmv array
      JINDS=IBEG+IC+ISEQS-1
      SN(NOFFS)=PARMV(JINDS)
      DO 180 IJ=1,NTPGSA
      IF(ITPS(IJ).EQ.ISEQS) THEN
      IDEGS=ITPGPS(1,IJ)
      IORDS=ITPGPS(2,IJ)
      NOFFS=(IDEGS-1)*IDEGS/2+3*(IDEGS-1)+IORDS+1
      ISNTT(IJ)=NOFFS
      ENDIF
  180 END DO
      ENDIF
  400 END DO
  600 CONTINUE
      RETURN
      END
