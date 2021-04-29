!$STARTE
      SUBROUTINE STARTE(LSUMER,SUMX,N6,KOUNT)
!********1*********2*********3*********4*********5*********6*********7**
! STARTE           82/08/27            8208.0    PGMR - TOM MARTIN
!
! FUNCTION:  COMPUTE THE LOCAL STARTER ERROR AND CHECK FOR
!            CONVERGENCE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   LSUMER
!   SUMX     I         COWELL POS./VEL. SUMS
!   N6       I         6 TIMES THE NUMBER OF SATELLITES IN THE SET
!   KOUNT
!
! COMMENTS:
!
! OUTPUT PARAMETERS - LOGICAL SWITCH INDICATING NON-CONVERGENCE OF THE
!                     COWELL SUMS WHEN .TRUE.
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      DIMENSION SUMX(N6,2)
      DATA EPS/0.5D-13/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 N=1,N6
      DIFF=ABS(SUMX(N,1)-SUMX(N,2))
      SUM=ABS(SUMX(N,1))+ABS(SUMX(N,2))
      ERRLIM=EPS*SUM
      LSUMER=LSUMER.OR.DIFF.GT.ERRLIM
      IF(KOUNT.LT.20) GO TO 1000
      IF(DIFF.GT.ERRLIM) WRITE(IOUT6,20000) N,DIFF,SUM
 1000 END DO
      RETURN
20000 FORMAT(' **STARTE**  N,DIFF,SUM=',I10,5X,2D25.16)
      END
