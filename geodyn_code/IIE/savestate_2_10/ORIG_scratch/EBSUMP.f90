!$EBSUMP
      SUBROUTINE EBSUMP(PMPA,PMPE,RESID,WT,NM,NP,NE,                    &
     &   BTWB,TBTWA,BTWD,SCRTCH)
!********1*********2*********3*********4*********5*********6*********7**
! EBSUMP           85/03/25            8503.0    PGMR - TOM MARTIN
!
! FUNCTION:  ACCUMULATE PARTIALS & RESIDUALS INTO THE NORMAL
!            MATRIX AND RIGHT HAND SIDE OF THE E-BIAS EQUATIONS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA     I         FULL ARRAY OF PARTIALS(INCLUDING ZEROS WHERE
!                      NECCESSARY) FOR EACH MEASUREMENT.SOMETIMES ARRAY
!                      INCLUDES ONLY ARC,SOMTIMES ARC&COMMON.
!   PMPE     I         PARTIALS OF MEAS. W.R.T. E-BIAS PARAMETERS
!   RESID    I         RESIDUAL ARRAY
!   WT       I         MEASUREMENT WEIGHT ARRAY
!   NM       I         NUMBER OF MEASUREMENTS
!   NP       I         NUMBER OF PARAMETERS
!   NE       I         NUUMBER OF E-BIAS PARAMETERS FOR THIS PASS
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
      PARAMETER ( ZERO = 0.0D0 )
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
      DIMENSION PMPA(NP,NM),PMPE(MINTIM,MEPARM),RESID(NM),WT(NM),       &
     &   BTWB(MEDIM),TBTWA(MAPARM,MEPARM),BTWD(MEPARM),SCRTCH(MEPARM)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      IL=MEPARM-NE
      DO 5000 N=1,NM
      IF(WT(N).LE.ZERO) GO TO 5000
      DO  800 IE=1,NE
      SCRTCH(IE)=WT(N)*PMPE(N,IE)
  800 END DO
      IP=1
      DO 2000 IE=1,NE
      DO 1000 JE=IE,NE
      BTWB(IP)=BTWB(IP)+SCRTCH(IE)*PMPE(N,JE)
      IP=IP+1
 1000 END DO
      IP=IP+IL
 2000 END DO
      DO 4000 IE=1,NE
      DO 3000 JP=1,NP
      TBTWA(JP,IE)=TBTWA(JP,IE)+SCRTCH(IE)*PMPA(JP,N)
 3000 END DO
      BTWD(IE)=BTWD(IE)+RESID(N)*SCRTCH(IE)
 4000 END DO
 5000 END DO
      RETURN
      END
