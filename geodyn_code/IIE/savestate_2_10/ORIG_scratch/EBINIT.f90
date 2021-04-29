!$EBINIT
      SUBROUTINE EBINIT(BTWB  ,TBTWA ,BTWD  ,PMPE  ,EBSTAT,NEBOBS)
!********1*********2*********3*********4*********5*********6*********7**
! EBINIT           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   BTWB
!   TBTWA
!   BTWD
!   PMPE
!   EBSTAT
!   NEBOBS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!********1*********2*********3*********4*********5*********6*********7**
! EBINIT           00/00/00            0000.0    PGMR - ?
!                      3/90            90??.0    PGMR - W. EDDY
!                      3/90            90??.0    PGMR - J. MCCARTHY
!
! FUNCTION:
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   SAMPLE   I    A    MATRIX OF PARTIALS OF MEASUREMENTS WRT PARAMETERS
!   BTWB          A    NORMAL MATRIX OF E-BIAS PARAMETERS
!   TBTWA         A    TRANSPOSE MATRIX OF CROSS TERMS BETWEEN
!                        E-BIAS AND OTHER PARAMETERS
!   BTWD          A    Right Hand Side of E-bias solution
!   PMPE          A    Matrix of partials of measurements wrt E-biases
!   EBSTAT
!   NEBOBS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      COMMON/CEBARC/NEBIAS,NBIASE,NXCEBA
      COMMON/CEBGLB/MEPARM,MEDIM ,MEBIAS,MBIASE,NEBGRP,IEBGRP(4),NXCEBG
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
!
      DIMENSION BTWB(MEDIM,3,MEBIAS),TBTWA(MAPARM,MEPARM,MEBIAS),       &
     &   BTWD(MEPARM,3,MEBIAS),PMPE(MINTIM,MEPARM),                     &
     &   EBSTAT(MEPARM,2,MEBIAS),NEBOBS(2,MEBIAS)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      NEBIAS=0
      NCLEAR=MEDIM*MEBIAS*3
      CALL CLEARA(BTWB,NCLEAR)
      NSUB=MEPARM*MEBIAS
      NCLEAR=NSUB*MAPARM
      CALL CLEARA(TBTWA,NCLEAR)
      NCLEAR=NSUB*3
      CALL CLEARA(BTWD,NCLEAR)
      NCLEAR=MINTIM*MEPARM
      CALL CLEARA(PMPE,NCLEAR)
      NCLEAR=2*NSUB
      CALL CLEARA(EBSTAT,NCLEAR)
      NCLEAR=2*MEBIAS
      CALL CLEARI(NEBOBS,NCLEAR)
      RETURN
      END
