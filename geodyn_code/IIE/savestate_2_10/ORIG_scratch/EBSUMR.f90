!$EBSUMR
      SUBROUTINE EBSUMR(RESIDU,PMPE  ,NM    ,INDEXX,NINDEX,STATEB)
!********1*********2*********3*********4*********5*********6*********7**
! EBSUMR           90/03/00            90??.0    PGMR - W. EDDY
!                  90/03/00            90??.0    PGMR - J. MCCARTHY
!
! FUNCTION:
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA     I    A    MATRIX OF PARTIALS OF MEASUREMENTS WRT PARAMETERS
!   RESIDU        A    MEASUREMENT RESIDUALS SCALED FOR PRINTOUT
!   PMPE          A    Matrix of partials of measurements wrt E-biases
!   NM       I    S    NUMBER OF MEASUREMENTS IN A DATA BLOCK
!   INDEXX
!   NINDEX
!   STATEB
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
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
!
      DIMENSION RESIDU(NM),PMPE(MINTIM,MEPARM),STATEB(MEPARM),          &
     &   INDEXX(NINDEX)
!
!**********************************************************************
! START OF EXECUPABLE CODE ********************************************
!**********************************************************************
!
      DO 2000 I=1,NINDEX
      J=INDEXX(I)
      SUM=ZERO
      DO 1000 N=1,NM
      SUM=SUM+RESIDU(N)*PMPE(N,I)
 1000 END DO
      STATEB(J)=STATEB(J)+SUM
 2000 END DO
      RETURN
      END
