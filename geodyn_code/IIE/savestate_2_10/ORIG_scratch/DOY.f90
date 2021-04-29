!$DOY
      FUNCTION DOY(MJDS,FSEC)
!********1*********2*********3*********4*********5*********6*********7**
! DOY              00/00/00            8803.0    PGMR - WEI XIA
!
! FUNCTION:  CALCULATE THE DAY OF THE CURRENT YEAR
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDS     I    S    TIME IN GEODYN INTERNAL TIME SYSTEM(MODIFIED
!                      JULIAN DAY SECONDS
!   FSEC     I    S    FRACTIONS OF SECONDS
!   DOY      O    S    DAY OF THE YEAR
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
!     DATA ID0/14616/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      ID0 = TMGDN1-2415384.5
!
! COMPUTE THE YYMMDD FROM MJDS AND FSE
      MJD=(DBLE(MJDS)+FSEC)/SECDAY
      FDAY=(DBLE(MJDS-MJD*86400)+FSEC)/SECDAY
      ID=MJD+ID0
      IY=((ID-1)*100)/36525+1
      ID=ID-(36525*(IY-1))/100
      ILEAPY=MOD(IY,4)
      ILEAPY=MIN(ILEAPY,1)+1
      IF(ILEAPY.EQ.1.OR.ID.LT.366) GO TO 500
      IY=IY+1
      ID=ID-365
  500 CONTINUE
      DOY=DBLE(ID)+FDAY
      RETURN
      END
