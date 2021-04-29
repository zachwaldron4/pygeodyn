!$VMATOW
      SUBROUTINE VMATOW(IYMD,IHM,SEC,MJDSTC,FSECTI,XS,VS,IDSAT,NEQN,NM, &
     &            LNPNM,PXPFNP,PXPFNM)
!********1*********2*********3*********4*********5*********6*********7**
! VMATOW           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IYMD
!   IHM
!   SEC
!   MJDSTC
!   FSECTI
!   XS
!   VS
!   IDSAT
!   NEQN
!   NM
!   LNPNM
!   PXPFNP
!   PXPFNM
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      DIMENSION FSECTI(NM),XS(MINTIM,3),VS(MINTIM,3)
      DIMENSION SEC(NM),IHM(NM),IYMD(NM)
!     ....change pxpfnp dimension order  from ddr -- jjm 10/6/92
!CC      DIMENSION PXPFNP(NEQN,NM,3,2),PXPFNM(NM,NEQN,3,2)
      DIMENSION PXPFNP(NEQN,3,NM,2),PXPFNM(NM,NEQN,3,2)
      DIMENSION DWORD(12)
      DATA D6/1.D6/,D2/1.D2/
      DATA IUNT80/80/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!     mjds from default reference time
      DWORD(3)= DBLE(MJDSTC) + REPDIF
      DWORD(11)=IDSAT
      DWORD(12)=6*NEQN
      IF(LNPNM) GO TO 1000
      DO 100 I=1,NM
      ISEC=SEC(I)
      XSEC=DBLE(ISEC)
      DWORD(1)=D6*IYMD(I)+D2*IHM(I)+XSEC
      DWORD(2)=SEC(I)-XSEC
      DWORD(4)=FSECTI(I)
      DWORD(5)=XS(I,1)
      DWORD(6)=XS(I,2)
      DWORD(7)=XS(I,3)
      DWORD(8)=VS(I,1)
      DWORD(9)=VS(I,2)
      DWORD(10)=VS(I,3)
      WRITE(IUNT80) DWORD,(PXPFNM(I,N,1,1),N=1,NEQN),                   &
     &  (PXPFNM(I,N,2,1),N=1,NEQN),(PXPFNM(I,N,3,1),N=1,NEQN),          &
     &  (PXPFNM(I,N,1,2),N=1,NEQN),(PXPFNM(I,N,2,2),N=1,NEQN),          &
     &  (PXPFNM(I,N,3,2),N=1,NEQN)
  100 END DO
      RETURN
 1000 CONTINUE
      DO 1100 I=1,NM
      ISEC=SEC(I)
      XSEC=DBLE(ISEC)
      DWORD(1)=D6*IYMD(I)+D2*IHM(I)+XSEC
      DWORD(2)=SEC(I)-XSEC
      DWORD(4)=FSECTI(I)
      DWORD(5)=XS(I,1)
      DWORD(6)=XS(I,2)
      DWORD(7)=XS(I,3)
      DWORD(8)=VS(I,1)
      DWORD(9)=VS(I,2)
      DWORD(10)=VS(I,3)
!     ....change pxpfnp dimension order  from ddr -- jjm 10/6/92
!c     CALL VM7WR(DWORD,PXPFNP(1,I,1,1),PXPFNP(1,I,2,1),PXPFNP(1,I,3,1),
!c    .           PXPFNP(1,I,1,2),PXPFNP(1,I,2,2),PXPFNP(1,I,3,2),NEQN)
      CALL VM7WR(DWORD,PXPFNP(1,1,I,1),PXPFNP(1,2,I,1),PXPFNP(1,3,I,1), &
     &           PXPFNP(1,1,I,2),PXPFNP(1,2,I,2),PXPFNP(1,3,I,2),NEQN)
 1100 END DO
      RETURN
      END
