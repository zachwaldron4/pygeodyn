!$ADJSTM
      SUBROUTINE ADJSTM(NM,FSECS,FSECM,FSECNL,FSECNU,FSECN,LCONS)
!********1*********2*********3*********4*********5*********6*********7**
!  ADJSTM          00/00/00         0000.0      PGMR - ?
!
! FUNCTION: ADJUST ELAPSED SECONDS OF TRACKING STATION (VLBI)
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NM       I    S    NUMBER OF MEASUREMENTS
!   FSECS    I    A    ELAPSED SECONDS FROM MJDSN OF THE EVENT
!                      TIMES ASSOCIATED WITH TRACKING STATION Tn
!   FSECM    I    A    ELAPSED SECONDS FROM MJDSN OF THE EVENT
!                      TIMES ASSOCIATED WITH SATELLITE Sm
!   FSECNL   I    A    LOWER LIMIT (SECONDS)
!   FSECNU   I    A    UPPER LIMIT (SECONDS)
!   FSECN   I/O   A    ELAPSED SECONDS FROM MJDSN OF THE EVENT
!                      TIMES ASSOCIATED WITH TRACKING STATION Tn
!   LCONS
!
! COMMENTS: MJDSN IS MODIFIED JULIAN DAY SECONDS OF THE FINAL
!           RECEIVED TIME OF THE FIRST OBSERVATION IN THE BLOCK
!
!
!********1*********2*********3*********4*********5*********6*********7**

!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
!
      DIMENSION FSECS(NM),FSECM(NM),FSECNL(NM),FSECNU(NM),FSECN(NM)
!
      DATA TOL/1.D-9/
!
      LCONS=.TRUE.
!
      DO 1000 I=1,NM
!
         DIF=FSECS(I)-FSECM(I)
         DIFA=ABS(DIF)
         IF(DIFA.LT.TOL) GO TO 1000
         LCONS=.FALSE.
!
!  FIRST HANDLE CASE WHERE FSECN PRODUCES AN  ESTIMATE OF FSECM
!  WHICH IS TOO SMALL
!
         IF(DIF.GT.0.D0) THEN
!
            IF(FSECN(I).GT.FSECNL(I)) FSECNL(I)=FSECN(I)
            IF(FSECNU(I).LT.8.D9) THEN
               FSECN(I)=(FSECNL(I)+FSECNU(I))/2.D0
               GO TO 1000
            ELSE
               FSECN(I)=FSECN(I)+DIFA
               GO TO 1000
            ENDIF
!
!  NOW HANDLE CASE WHERE FSECN PRODUCES AN  ESTIMATE OF FSECM
!  WHICH IS TOO LARGE
!
         ELSE
!
            IF(FSECN(I).LT.FSECNU(I)) FSECNU(I)=FSECN(I)
            IF(FSECNL(I).GT.-8.D9) THEN
               FSECN(I)=(FSECNL(I)+FSECNU(I))/2.D0
               GO TO 1000
            ELSE
               FSECN(I)=FSECN(I)-DIFA
               GO TO 1000
            ENDIF
!
         ENDIF
!
1000  CONTINUE
!
      RETURN
      END
