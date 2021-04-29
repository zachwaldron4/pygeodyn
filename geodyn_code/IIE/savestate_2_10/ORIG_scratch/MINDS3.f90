!$MINDS3
      SUBROUTINE MINDS3(ND1,NU1,T1EST,T2EST,ACOEF,T1,T2,D,XPRN,LA1,LA2)
!********1*********2*********3*********4*********5*********6*********7**
! PFTPAR             00/00/00            0000.0    PGMR -DAVE ROWLANDS
!
!
! FUNCTION: PROCESS DYNAMIC CROSSOVERS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER(MAXND1=21)
      PARAMETER(MAXITER=7)
      DIMENSION POLY(3,2,3),TM(2),PDIF(3)
      INCLUDE 'COMMON_DECL.inc'
      COMMON/EDPAR/MD3C,MD3C2
      DIMENSION ACOEF(ND1,NU1,3,2)
      DIMENSION XPRN(3,2)
      DIMENSION CHB(MAXND1),CHBV(MAXND1),CHBA(MAXND1)
!
!
      IF(ND1.GT.MAXND1) THEN
       ND=ND1-1
       MD=MAXND1-1
       WRITE(6,6000)
       WRITE(6,6000)
       WRITE(6,6001)
       WRITE(6,6002) ND
       WRITE(6,6003) MD
       STOP
      ENDIF
!
!
      IT=0
      TM(1)=T1EST
      TM(2)=T2EST
  100 CONTINUE
      DO 120 J=1,2
      CALL CHEBPL2(CHB,CHBV,CHBA,TM(J),ND1)
      DO 110 I=1,3
      CALL PVALC(ND1,CHB,CHBV,CHBA,ACOEF(1,1,I,J),                      &
     &           POLY(I,J,1),POLY(I,J,2),POLY(I,J,3))
  110 END DO
  120 END DO
      LA1=.TRUE.
      LA2=.TRUE.
      IF(POLY(3,1,2).LT.0.D0) LA1=.FALSE.
      IF(POLY(3,2,2).LT.0.D0) LA2=.FALSE.
      IT=IT+1
      PDIF(1)=POLY(1,1,1)-POLY(1,2,1)
      PDIF(2)=POLY(2,1,1)-POLY(2,2,1)
      PDIF(3)=POLY(3,1,1)-POLY(3,2,1)
!
      RHS1=-PDIF(1)*POLY(1,1,2)                                         &
     &     -PDIF(2)*POLY(2,1,2)                                         &
     &     -PDIF(3)*POLY(3,1,2)
      RHS2=+PDIF(1)*POLY(1,2,2)                                         &
     &     +PDIF(2)*POLY(2,2,2)                                         &
     &     +PDIF(3)*POLY(3,2,2)
      XMIN=ABS(RHS1)+ABS(RHS2)
      IF(XMIN.LT.5.D-4) GO TO 200
      IF(IT.GT.MAXITER) THEN
         MD3C=MD3C+1
         MD3C2=MD3C2+1
         WRITE(6,6004) MAXITER,MD3C,MD3C2
        GO TO 200
      ENDIF
      A11= POLY(1,1,2)*POLY(1,1,2)+PDIF(1)*POLY(1,1,3)                  &
     &    +POLY(2,1,2)*POLY(2,1,2)+PDIF(2)*POLY(2,1,3)                  &
     &    +POLY(3,1,2)*POLY(3,1,2)+PDIF(3)*POLY(3,1,3)
      A22= POLY(1,2,2)*POLY(1,2,2)-PDIF(1)*POLY(1,2,3)                  &
     &    +POLY(2,2,2)*POLY(2,2,2)-PDIF(2)*POLY(2,2,3)                  &
     &    +POLY(3,2,2)*POLY(3,2,2)-PDIF(3)*POLY(3,2,3)
      A12=-POLY(1,2,2)*POLY(1,1,2)                                      &
     &    -POLY(2,2,2)*POLY(2,1,2)                                      &
     &    -POLY(3,2,2)*POLY(3,1,2)
      A21=A12
      DET=A11*A22-A21*A12
      IF(DET.EQ.0.D0) THEN
         WRITE(6,6000)
         WRITE(6,6001)
         WRITE(6,6005)
         STOP
      ENDIF
      DELT1=( A22*RHS1-A12*RHS2)/DET
      DELT2=(-A21*RHS1+A11*RHS2)/DET
      TM(1)=TM(1)+DELT1
      TM(2)=TM(2)+DELT2
      GO TO 100
  200 CONTINUE
      T1=TM(1)
      T2=TM(2)
      D=SQRT(PDIF(1)*PDIF(1)+PDIF(2)*PDIF(2)+PDIF(3)*PDIF(3))
      XPRN(1,1)=POLY(1,1,1)
      XPRN(2,1)=POLY(2,1,1)
      XPRN(3,1)=POLY(3,1,1)
      XPRN(1,2)=POLY(1,2,1)
      XPRN(2,2)=POLY(2,2,1)
      XPRN(3,2)=POLY(3,2,1)
      RETURN
 6000 FORMAT(' ')
 6001 FORMAT(' EXECUTION TERMINATING IN MINDS3')
 6002 FORMAT(' DEGREE OF INPUT POLYNOMIAL: ',I3)
 6003 FORMAT(' EXCEEDS MAX ALLOWABLE: ',I3)
 6004 FORMAT(' WARNING MAX ITERATIONS ',I3,' EXCEEEDED IN MINDS3',2I10)
 6005 FORMAT(' DETERMINANT FOR MIN DIST IS ZERO')
      END
