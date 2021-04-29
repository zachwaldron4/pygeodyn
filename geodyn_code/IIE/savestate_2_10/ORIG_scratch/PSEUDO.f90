!$PSEUDO
      SUBROUTINE PSEUDO(N,D,ZL,WK,NN,IZ,JOBN,NEL,AN,DI,ANI)
!********1*********2*********3*********4*********5*********6*********7**
! PSEUDO           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   N
!   D
!   ZL
!   WK
!   NN
!   IZ
!   JOBN
!   NEL
!   AN
!   DI
!   ANI
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DOUBLE PRECISION N(1),D(1),ZL(IZ,1),WK(1),AN(IZ,1),DI(1),ANI(IZ,1)
      INCLUDE 'COMMON_DECL.inc'
      COMMON/INVRSA/EPSIN1,EPSIN2,TOLRNC,XINVRS
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!      CALL E2CSF(NN,N,IZ,D,ZL,IZ,WK,IER)
      CALL EVCSF(NN,N,IZ,D,ZL,IZ,IER)
!
!     WRITE(6,100) IER, WK(1)
! 100 FORMAT(1H1/'        IER = ' , I5,'     PERFORMANCE INDEX = ',
!    .  F10.4)
      IF(IER.NE.0) STOP 99
!     WRITE(6,200) (D(I),I=1,NN)
! 200 FORMAT(/1X,10E13.6)
!      CONDN=D(1)/D(IZ)
      CONDN=D(IZ)/D(1)
!     WRITE(6,300) CONDN
! 300 FORMAT(//10X,'INVERSE CONDITION NUMBER : ',E16.8//)
!
      K=0
      DO 10 I=1,NN
!      IF(DABS(D(I)/D(IZ)).LE.TOLRNC) GO TO 10
      IF(ABS(D(IZ)/D(I)).LE.TOLRNC) GO TO 10
      K=K+1
      DI(K)=1.D0/D(I)
      DO 9 J=1,IZ
      AN(J,K)=ZL(J,I)
    9 END DO
!
   10 END DO
!     WRITE(6,200) (ZL(I,J),J=1,NN)
!
      NDEF=K
      CALL BSBT(AN,DI,ANI,NN,NDEF)
!     DO 5 I=1,NN
!   5 WRITE(6,200) (ANI(I,J),J=1,NN)
      CALL MULTI(ANI,AN,ZL,NN,NN,NN)
!     DO 7 I=1,NN
!   7 WRITE(6,200) (ZL(I,J),J=1,NN)
      RETURN
      END
