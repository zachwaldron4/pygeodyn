!$EBSUMM
      SUBROUTINE EBSUMM(PMPA,PMPE,RESID,WT,NM,NP,NE,                    &
     &   BTWB,TBTWA,BTWD,SCRTCH)
!********1*********2*********3*********4*********5*********6*********7**
! EBSUMM           85/03/25            8503.0    PGMR - TOM MARTIN
!
! FUNCTION:   ACCUMULATE PARTIALS & RESIDUALS INTO THE NORMAL
!             MATRIX AND RIGHT HAND SIDE OF THE E-BIAS EQUATIONS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA     I         FULL ARRAY OF PARTIALS(INCLUDING ZEROS WHERE
!                      NECCESSARY) FOR EACH MEASUREMENT.SOMETIMES A
!                      INCLUDES ONLY ARC,SOMTIMES ARC&COMMON.
!   PMPE     I         PARTIALS OF MEAS. W.R.T. E-BIAS PARAMETERS
!   RESID    I         RESIDUAL ARRAY
!   WT       I         MEASUREMENT WEIGHT ARRAY
!   NM       I         NUMBER OF MEASUREMENTS
!   NP       I         NUMBER OF PARAMETERS
!   NE       I         NUMBER OF E-BIAS PARAMETERS FOR THIS PASS
!   BTWB     O         NORMAL MATRIX FOR E-BIAS PARAMETERS
!   TBTWA    O         TRANSPOSE OF CROSS TERMS BETWEEN E-BIAS
!                      PARAMETERS AND OTHER PARAMETERS.
!   BTWD     O         RIGHT HAND SIDE OF E-BIAS NORMAL EQUATIONS
!   SCRATCH       A
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER( ZERO = 0.0D0 )
!
      COMMON/CEBGLB/MEPARM,MEDIM ,MEBIAS,MBIASE,NEBGRP,IEBGRP(4),NXCEBG
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
!
      DIMENSION PMPA(NM,NP),PMPE(MINTIM,MEPARM),RESID(NM),WT(NM),       &
     &   BTWB(MEDIM),TBTWA(MAPARM,MEPARM),BTWD(MEPARM),SCRTCH(NM)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      IP=1
      IL=MEPARM-NE
      DO 4000 IE=1,NE
      DO  800 N=1,NM
      SCRTCH(N)=WT(N)*PMPE(N,IE)
  800 END DO
!
      DO 2000 JE=IE,NE
      SUM=ZERO
      DO 1800 N=1,NM
      SUM=SUM+SCRTCH(N)*PMPE(N,JE)
 1800 END DO
      BTWB(IP)=BTWB(IP)+SUM
      IP=IP+1
 2000 END DO
!
      DO 3000 JP=1,NP
      SUM=ZERO
      DO 2800 N=1,NM
      SUM=SUM+SCRTCH(N)*PMPA(N,JP)
 2800 END DO
      TBTWA(JP,IE)=TBTWA(JP,IE)+SUM
 3000 END DO
!
      SUM=ZERO
      DO 3800 N=1,NM
      SUM=SUM+RESID(N)*SCRTCH(N)
 3800 END DO
      BTWD(IE)=BTWD(IE)+SUM
      IP=IP+IL
 4000 END DO
      RETURN
      END
