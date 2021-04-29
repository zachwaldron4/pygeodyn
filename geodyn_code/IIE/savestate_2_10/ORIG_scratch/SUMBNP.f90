!$SUMBNP
      SUBROUTINE SUMBNP(PMPA,RESID,WT,NP,NDIM,ATWA,ATWB,SCRTCH)
!********1*********2*********3*********4*********5*********6*********7**
! SUMBNP           85/03/13            8503.0    PGMR - TOM MARTIN
!                  84/02/27            8402.0    PGMR - D.ROWLAND
!
! FUNCTION:  ACCUMULATE PARTIALS & RESIDUALS INTO
!            NORMAL MATRIX AND RIGHT HAND SIDE OF NORMAL EQ.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA     I         FULL ARRAY OF PARTIALS(INCLUDING ZEROS WHERE
!                      NECCESSARY)
!   RESID    I         RESIDUAL
!   WT       I         MEASUREMENT WEIGHT
!   NP       I         NUMPER OF PARAMETERS
!   NDIM     I         MAXIMUM DIMENSION OF NORMAL MATRIX
!   ATWA     O         NORMAL MATRIX
!   ATWB     O         RIGHT HAND SIDE OF NORMAL EQUATIONS
!   SCRTCH        A    SCRATCH ARRAY
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION PMPA(NP),ATWA(1),ATWB(NP),SCRTCH(NP)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      NL=NDIM-NP
!     SCRATCH IS ATW
      DO 100 I=1,NP
      SCRTCH(I)=WT*PMPA(I)
  100 END DO
      IL=NP
      ISROW=0
      ISP=0
      DO 500 I=1,NP
      DO 200 II=1,IL
      ATWA(ISROW+II)=ATWA(ISROW+II)+SCRTCH(I)*PMPA(ISP+II)
  200 END DO
      ISROW=ISROW+NL+IL
      ISP=ISP+1
      IL=IL-1
  500 END DO
      DO 600 I=1,NP
      ATWB(I)=ATWB(I)+RESID*SCRTCH(I)
  600 END DO
      RETURN
      END
