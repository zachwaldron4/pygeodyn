!$FACBIN
      SUBROUTINE FACBIN
!********1*********2*********3*********4*********5*********6*********7**
! FACBIN           83/08/16            8308.0    PGMR - D.ROWLANDS
!                                                       (MOD FOR GII)
!
! FUNCTION:  INITIALIZE COMMON BLOCK CFACT FOR (ETIDE)
!            ROUTINES FINCL&GEXC
!
! COMMENTS: INPUT - NONE
!           OUTPUT THROUGH COMMON BLOCK CFACT
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CFACT/F21(21),B33(3,3),BN(5,18)
      DATA IBX,INUX,INLX,INX/3,5,18,21/
      DATA ZERO/0.D0/,ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! COMPUTE 0: TO (INX-1):
      F21(1)=ONE
      DO 100 J=2,INX
      F21(J)=DBLE(J-1)*F21(J-1)
  100 END DO
! COMPUTE (J-1):/((K-1):(J-K):)
      DO 400 J=1,IBX
      DO 200 K=1,J
      KPT=J-K+1
      B33(J,K)=(F21(J)/F21(K))/F21(KPT)
  200 END DO
      IF(J.GE.IBX) GO TO 400
      JP1=J+1
      DO 300 K=JP1,IBX
      B33(J,K)=ZERO
  300 END DO
  400 END DO
      DO 500 K=1,INLX
      BN(1,K)=ZERO
      DO 500 J=2,INUX
      KPT=J+K-2
      BN(J,K)=(F21(KPT)/F21(K))/F21(J-1)
      IF(MOD(K,2).EQ.0) BN(J,K)=-BN(J,K)
  500 CONTINUE
      BN(1,1)=ONE
      RETURN
      END
