!$SHDDEL
      SUBROUTINE SHDDEL(NM,LEDIT,IYMD,IHM,SEC)
!********1*********2*********3*********4*********5*********6*********7**
! SHDDEL           88/07/26            8805.0    PGMR - D. ROWLANDS
!
! FUNCTION:  IN SIMDAT RUNS, WRITE DELETE CARDS AT MESUREMENT TIMES
!            FOR WHICH THE SATELLITE IS SHADOWED.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NM       I    S    NUMBER OF MESUREMENTS TO BE CHECKED
!                      FOR POSSIBLE DELETETION.
!   LEDIT    I    A    ARRAY SAYING WHETHER MEAS HAS BEEN EDITED FOR
!                      SOME OTHER REASON.
!   IYMD     I    A    ARRAY CONTAINING YYMMDD OF TIMES TO BE CHECKED
!   IHM      I    A    ARRAY CONTAINING HHMM   OF TIMES TO BE CHECKED
!   SEC      I    A    ARRAY CONTAINING SECONDS OF TIMES TO BE CHECKED
!
! COMMENTS:
!
!          COMMON/OCCLTL/ IS USED TO CHECK ON SHADOWING
!
!          OUTPUT  TO UNIT 35  IS A DELETE CARD FOR EVERY MEASUREMENT
!          WHICH IS SHADOWED AND NOT OCCULTED
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/OCCLTL/LOCCLT(200),LMSHD(200)
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      DIMENSION LEDIT(NM),IYMD(NM),IHM(NM),SEC(NM)
      CHARACTER(8)      :: DELETE
      DATA DELETE/'DELETE'/,ONE/1.D0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      DO 1000 I=1,NM
      IF(LACC) GOTO 1001
      IF(LEDIT(I)) GO TO 1000
 1001 CONTINUE
!     IF(LOCCLT(I)) GO TO 1000
      IF(.NOT.LMSHD(I).AND..NOT.LOCCLT(I)) GO TO 1000
      S1=SEC(I)-ONE
      IF(S1.LT.0.D0) THEN
      S1=0.D0
      ENDIF
      S2=SEC(I)+ONE
      WRITE(35,7000) DELETE,IYMD(I),IHM(I),S1,IYMD(I),IHM(I),S2
 1000 END DO
      RETURN
 7000 FORMAT(A6,34X,I6,I4,D10.2,I6,I4,D10.2)
      END
