!$CRAGAG
      SUBROUTINE CRAGAG(SUM1,NDIM,NSTART,NPARAM,IARCNO)
!********1*********2*********3*********4*********5*********6*********7**
! CRAGAG           00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  TO COMPUTE AND PRINT CORRELATION COEFFICIENTS FROM
!            THE OFF DIAGONAL PORTION OF A NORMAL
!            MATRIX IN VECTOR FORM
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   SUM1     I    A    INVERTED LEAST SQUARES MATRIX IN VECTOR
!   (1)                FORM
!   NDIM     I    S    FIRST DIMENSION OF SQUARE ARRAY EQUIVALENT
!   NSTART   I    S    STARTING LOCATION OF GLOBAL PORTION OF
!                      'SUM1' VECTOR
!   NPARAM   I    S    NUMBER OF ARC PARAMETERS
!   IARCNO   I    S    ARC NUMBER
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION ITL(18),SUM1(1)
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      INDXNO(I)=NDIM*(I-1)-(I*(I-1))/2
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! PRINT CROSS CORRELATIONS
      WRITE(IOUT6,44450) IARCNO
      DO 10 JJ=NSTART,NDIM,18
      JJJ=JJ+17
      JJJ=MIN(JJJ,NDIM)
      JJJJ=JJJ-JJ+1
      DO 5 J=1,JJJJ
      ITL(J)=JJ+J-1
    5 END DO
      WRITE(IOUT6,10214) (ITL(J),J=1,JJJJ)
   10 END DO
      IST=NSTART
      ISTP=NDIM
      DO 800 I=1,NPARAM
      INDEXI=INDXNO(I)
      II=INDEXI+I
      DO 700 J=NSTART,NDIM
      JJ=INDXNO(J)+J
      IJ=INDEXI+J
  700 SUM1(IJ)=SUM1(IJ)/(SUM1(II)*SQRT(SUM1(JJ)))
      WRITE(IOUT6,10215) I,(SUM1(J),J=IST,ISTP)
      IST=IST+NDIM-I
  800 ISTP=ISTP+NDIM-I
      RETURN
10214 FORMAT('0',6X,18I6)
10215 FORMAT('0',I4,2X,18F6.3/(7X,18F6.3))
44450 FORMAT('0'/7X,'CROSS CORRELATION COEFFICIENTS BETWEEN COMMON ',   &
     &  'PARAMETERS AND ARC',I3,' ADJUSTED PARAMETERS')
      END
