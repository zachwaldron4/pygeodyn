!$STABHD
      SUBROUTINE STABHD(ISTANO,STNAME,SXYZ,STAINF)
!********1*********2*********3*********4*********5*********6*********7**
! STABHD           85/03/25            8504.0    PGMR - D. ROWLANDS
!
! FUNCTION:  PRINT OUT STATION HEADER INFO FOR BINRES FILE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ISTANO   I    A    EXTERNAL STATION NUMBER
!   STNAME   I    A    STATION NAME
!   SXYZ     I    A    EARTH CENTERD XYZ COORDINATES
!   STAINF(1)I    A    LATITUDE OF STATION (RADIANS)
!   STAINF(2)I    A    COSINE OF STATION LATITUDE
!   STAINF(3)I    A    SINE OF STATION LATITUDE
!   STAINF(4)I    A    LONGITUDE OF STATION (RADIANS)
!   STAINF(5)I    A    COSINE OF STATION LONGITUDE
!   STAINF(6)I    A    SINE OF STATION LONGITUDE
!   STAINF(7)I    A    SPHEROID HT
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION SXYZ(3,1),STAINF(NSTAIN,1),ISTANO(1),STNAME(1)
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
      DATA ZERO /0.D0/,ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLA CODE *********************************************
!***********************************************************************
!
      ISTACT=0
   10 CONTINUE
      ISTACT=ISTACT+1
      IF(ISTACT.GT.NSTA) RETURN
      SAD=SXYZ(1,ISTACT)*SXYZ(1,ISTACT)+SXYZ(2,ISTACT)*SXYZ(2,ISTACT)
      SAD=SQRT(SAD)
      XSTANO=DBLE(ISTANO(ISTACT))
      WRITE(IUNT19) STNAME(ISTACT),XSTANO,SXYZ(1,ISTACT),               &
     &              SXYZ(2,ISTACT),SXYZ(3,ISTACT),STAINF(1,ISTACT),     &
     &              STAINF(4,ISTACT),STAINF(7,ISTACT),SAD
      GO TO 10
      END
