!$DPSINV
      SUBROUTINE DPSINV(A,ND,N,SCRCH)
!********1*********2*********3*********4*********5*********6*********7**
! DPSINV           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A
!   ND
!   N
!   SCRCH
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
! DOUBLE PRECISION VERSION OF DSINV
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
       DOUBLE PRECISION ZERO,ONE,EPS
      DIMENSION A(1),SCRCH(1)
!     DIMENSION TEMP(1)
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/INVRSL/LSYMSW,LMATP,NXINVR
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
      DATA ZERO/0.0D0/,ONE/1.0D0/,EPS/1.0D-8/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
       ZEROD = ZERO
       ONED  = ONE
       EPSD  = EPS
!     NSIZE=N*(N+1)/2
! CALCULATE STORAGE AVAILABLE FOR SAVING MATRIX
!     MXSTOR=(IPSIZE-10)/2
      IF(.NOT.LMATP) GO TO 130
      WRITE(IOUT6,1001)
      IST=1
      IEND=N
      DO 125 I=1,N
      WRITE(IOUT6,1000) (A(J),J=IST,IEND)
      IST=ND-I+IST+1
      IEND=IST+N-I-1
  125 END DO
  130 CONTINUE
      IF(N.LT.1)GO TO 12
      IER=0
      IF(N.GT.1)GO TO 15
      A(1)=ONED/A(1)
      RETURN
   15 CONTINUE
!
!        FACTOR A INTO TRN(R)*R WHERE R IS UPPER TRIANGULAR
!        (CHOLESKY REDUCTION)
      NDIF=ND-N
      J=1
      DO 14 K=1,N
      ILEND=K-1
       TOL=ABS(EPSD*A(J))
      DO 11 I=K,N
      DSUM=ZEROD
      IF(K.EQ.1)GO TO 4
      DO 3 IL=1,ILEND
      KL=K-IL
      IL1=(KL-1)*ND-(KL-1)*KL/2
    3 DSUM=DSUM+A(IL1+K)*A(IL1+I)
    4 DSUM=A(J)-DSUM
      IF(I.GT.K)GO TO 10
      IF(DSUM.GT.TOL)GO TO 9
      IF(DSUM.LE.ZERO)GO TO 12
      IF(IER.GT.0)GO TO 9
      IER=K-1
    9 DPIV=SQRT(DSUM)
      A(J)=DPIV
      DPIV=ONED/DPIV
      GO TO 11
   10 A(J)=DSUM*DPIV
   11 J=J+1
   14 J=J+NDIF
!
!        INVERT R
      J=(N-1)*ND+(3-N)*N/2
      A(J)=ONED/A(J)
      IPIV=J
!
      DO 6 I=2,N
      J=IPIV-NDIF
      IPIV=J-I
      DIN=ONED/A(IPIV)
      A(IPIV)=DIN
      I1=N+2-I
      I2=I-1
      I3=I1-1
      IL1=(I3-1)*ND-(I3-1)*I3/2
      DO 5 K1=1,I2
      K=N+1-K1
      J=J-1
      WORK=ZERO
      DO 2 IL=I1,K
      IL2=(IL-1)*ND-(IL-1)*IL/2+K
    2 WORK=WORK+A(IL1+IL)*A(IL2)
    5 A(J)=-DIN*WORK
    6 END DO
!
!        INVERSE(A) = INV(R)*TRN(INV(R))
      IL=1
      DO 13 I=1,N
      IL1=(I-1)*ND-(I-1)*I/2
      DO 8 J=I,N
      IL2=(J-1)*ND-(J-1)*J/2
      WORK=ZEROD
      DO 7 K=J,N
    7 WORK=WORK+A(IL1+K)*A(IL2+K)
      A(IL)=WORK
    8 IL=IL+1
   13 IL=IL+NDIF
!  LOSS OF SIGNIFIGANCE IF IER.GT.0
      IF(IER.GT.0) WRITE(IOUT6,10000) EPSD
      IF(IER.GT.0) WRITE(IUNT88,10000) EPSD
!     IF(IER.GT.0) CALL ERROR(19,EPS)
!     IF(LTRYSM.AND.NSIZE.LE.MXSTOR) CALL CLEAR(TEMP,NSIZE,2)
      GO TO 300
   12 CONTINUE
      WRITE(IOUT6,10100) EPSD
      STOP
  300 CONTINUE
      IF(.NOT.LMATP) GO TO 400
!     IF(.NOT.LPRINT) GO TO 400
      WRITE(IOUT6,1002)
      IST=1
      IEND=N
      DO 325 I=1,N
      WRITE(IOUT6,1000) (A(J),J=IST,IEND)
      IST=ND-I+IST+1
      IEND=IST+N-I-1
  325 END DO
  400 CONTINUE
      RETURN
!     ENTRY DSINV1(TEMP)
!     RETURN
 1000 FORMAT(1X,6G23.16)
 1001 FORMAT('0','  MATRIX TO BE INVERTED'/1X )
 1002 FORMAT('0','  MATRIX AFTER INVERSION'/1X )
10000 FORMAT(' ** DSINV **  LOSS OF SIGNIFICANCE. EPS=',G24.16)
10100 FORMAT(' ** DSINV **  INVERSION UNSUCCESSFUL. EPS=',G24.16)
      END
