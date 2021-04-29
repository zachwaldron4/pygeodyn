!$SUMGTE
      SUBROUTINE SUMGTE(PMPP,RESID,WT,NP,NRMTOT,NM,ATPA,ATPL,SCRTCH,NA, &
     &                  NAMBB)
!********1*********2*********3*********4*********5*********6*********7**
! SUMNP            85/10/08            0000.0    PGMR - TOM MARTI
!                  84/02/27            0000.0    PGMR - D.ROWLAND
!
! FUNCTION:  ACCUMULATE PARTIALS & RESIDUALS INTO THE
!            NORMAL MATRIX AND RIGHT HAND SIDE OF THE NORMAL EQ.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPP     I         FULL ARRAY OF PARTIALS(INCLUDING ZEROS WHERE
!                      NECCESSARY) FOR EACH MEASUREMENT.SOMETIMES A
!                      INCLUDES ONLY ARC,SOMTIMES ARC&COMMON.
!   RESID    I         RESIDUAL ARRAY
!   WT       I         MEASUREMENT WEIGHT ARRAY
!   NP       I         NUMPER OF PARAMETERS
!   NRMTOT   I         MAXIMUM DIMENSION OF NORMAL MATRIX
!   NM       I         NUMBER OF MEASUREMENTS
!   ATPA     O         NORMAL MATRIX
!   ATPL     O         RIGHT HAND SIDE OF NORMAL EQUATIONS
!   SCRTCH       A     SCRATCH ARRAY
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
      DIMENSION PMPP(NP,NM),RESID(NM),WT(NM),ATPA(1),ATPL(NP),SCRTCH(NM)
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      ISROW1=(NA-1)*(NRMTOT-NAMBB+1)
      IL1=NP-NAMBB+1
      IP1=NAMBB-1
      DO 2000 IOBS=1,NM
      SCRTCH(IOBS)=WT(IOBS)*PMPP(NA,IOBS)
      ATPL(NA)=ATPL(NA)+RESID(IOBS)*SCRTCH(IOBS)
      ATPA(ISROW1+1)=ATPA(ISROW1+1)+SCRTCH(IOBS)*PMPP(NA,IOBS)
 2000 END DO
      DO 3000 IOBS=1,NM
      DO 2800 II=2,IL1
      ATPA(ISROW1+II)=ATPA(ISROW1+II)+SCRTCH(IOBS)*PMPP(IP1+II,IOBS)
 2800 END DO
 3000 END DO
      RETURN
      END
