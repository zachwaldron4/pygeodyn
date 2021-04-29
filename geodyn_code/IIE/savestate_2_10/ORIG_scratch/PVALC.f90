!$PVALC
      SUBROUTINE PVALC(ND1,CHB,CHBV,CHBA,ACOEF,P,PD,PDD)
!********1*********2*********3*********4*********5*********6*********7**
! PVAL               00/00/00            0000.0    PGMR -DAVE ROWLANDS
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
!
      DIMENSION ACOEF(ND1)
      DIMENSION CHB(ND1),CHBV(ND1),CHBA(ND1)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!
!
      P=ACOEF(1)*CHB(1)
      PD=ACOEF(1)*CHBV(1)
      PDD=ACOEF(1)*CHBA(1)
      DO I=2,ND1
        P=P+ACOEF(I)*CHB(I)
        PD=PD+ACOEF(I)*CHBV(I)
        PDD=PDD+ACOEF(I)*CHBA(I)
      ENDDO
!
!
!
!
      RETURN
      END
