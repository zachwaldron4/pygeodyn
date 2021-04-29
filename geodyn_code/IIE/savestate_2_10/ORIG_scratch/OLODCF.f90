!$OLODCF
      SUBROUTINE OLODCF(ABCOF,XLOCV,NMD,II,KIOLM,IPVOL,STAINF,          &
     &                  PARMV,ANGFAC,PNAME)
!********1*********2*********3*********4*********5*********6*********7**
! OLODCF           00/00/00            8604.0    PGMR - ?
!
!
! FUNCTION:  CALCULATES X Y Z AMPLITUDES FOR OCEAN LOADING ONCE PER
!            GLOBAL ITERATION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ABCOF    O    A    FREQUENCIES FOR OCEAN LOADING
!   XLOCV    I    A    OCEAN LOADING PARAMETERS COVARIANCE MATRIX
!   NMD      I    A
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   KIOLM    I    A
!   IPVOL    I    A
!   STAINF   I    A    ARRAY CONTAINING STATION INFORMATION
!   PARMV    I    A    PARAMETER VALUE ARRAY
!   ANGFAC   I    A    FACTORS TO MULTIPLY EACH OF THE 5 ARGUMENTS
!                      BY PLUS A +/- PHASE PHASE FOR CERTAIN DOODSON
!                      NUMBERS
!   PNAME    I    A    PARAMETER LABEL ARRAY
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      CHARACTER*8 ALABT
      CHARACTER*4 CSITE1,CSITE2
      SAVE
      DIMENSION XLOCV(3,3,1)
      DIMENSION ABCOF(NTOLFR,2,3,1),STAINF(NSTAIN,1)
! %%%%%%% NMD(3,1) -> NMD(3,*)
      DIMENSION NMD(3,*),II(1),KIOLM(3,1),IPVOL(3,1),PARMV(1)
      DIMENSION ANGFAC(NTOLFR,6)
      DIMENSION PNAME(2,1)
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CORI03/KICRD ,KICON ,KISTNO,KINDPI,KMJDPL,KIPOLC,          &
     &              KNDOLA,KIOLPA,KKIOLA,KIP0OL,KNDOLM,KIOLPM,          &
     &              KKIOLM,KIPVOL,KICNL2,KICNH2,                        &
     &              KSTMJD, KNUMST, KITAST, KSTNRD, KICNV,              &
     &              KTIDES,KFODEG,KFOORD,KRDEGR,KRORDR,                 &
     &              KPHINC,KDOODS,KOTFLG,KJDN  ,KPTDN ,                 &
     &              KATIND,KPRESP,KMP2RS,KSITE,KPTOLS,NXCI03
      COMMON/LOLOAD/LOLMD,LOLAJ,L2FOLT,LAPLOD
      COMMON/OLDMOD/NTOLFR,NTOLF2,NTOLMD,NSTAOL,NSITOL,NTOLAJ,MXOLSA,   &
     &              NPOLSH,                                             &
     &              NXOLMD
      COMMON/OLOADA/LXYZCO,LEOPTO
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      DATA ZERO/0.D0/,EPS/.000001D0/
      DATA CSITE1/'   1'/,CSITE2/'   2'/
      EQUIVALENCE (ALAB,ALABT)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
! ZERO OUT THE ABCOF ARRAY
      NZERO=NTOLFR*6*NSTAOL
      DO 10 I=1,NZERO
      ABCOF(I,1,1,1)=ZERO
   10 END DO
      ISTAOL=0
!
! FIND OUT IF XYZ CENTER OF MASS IS BEING MODELLED
! ON OLOAD CARDS (LXYZCO) AND IF TIDAL EFFECTS ON
! EARTH ORIENTATION ARE BEING MODELLED ON OLOAD
! CARDS (LEOPTO)
!
      LXYZCO=.FALSE.
      LEOPTO=.FALSE.
      ISITE=1
      DO 20 I=1,3
      IF(NMD(I,1).LE.0) GO TO 20
      IPV=IPVOL(I,ISITE)
      ALAB=PNAME(1,IPV)
      IF(ALABT(4:7).EQ.CSITE1) THEN
         ISTAOL=ISTAOL+1
         LXYZCO=.TRUE.
         GO TO 25
      ENDIF
      IF(ALABT(4:7).EQ.CSITE2) THEN
         ISTAOL=ISTAOL+1
         LEOPTO=.TRUE.
         GO TO 25
      ENDIF
   20 END DO
   25 CONTINUE
      IF(LEOPTO.OR.NSITOL.LT.2) GO TO 35
      ISITE=2
      DO 30 I=1,3
      IF(NMD(I,2).LE.0) GO TO 30
      IPV=IPVOL(I,ISITE)
      ALAB=PNAME(1,IPV)
      IF(ALABT(4:7).EQ.CSITE2) THEN
         ISTAOL=ISTAOL+1
         LEOPTO=.TRUE.
         GO TO 35
      ENDIF
   30 END DO
   35 CONTINUE
!
!
      NXTRA=ISTAOL
      NTOT=NSTA+NXTRA
      JSTA=0
      ISTAOL=0
!
!
   50 CONTINUE
      JSTA=JSTA+1
      IF(JSTA.GT.NTOT) GO TO 1000
      ISTA=JSTA-NXTRA
      LXTRA=.FALSE.
      IF(ISTA.LT.1) LXTRA=.TRUE.
!CCCCCDO 1000 ISTA=1,NSTA
! DETERMINE OCEAN LOADING SITE FOR THE STATION
      IF(.NOT.LXTRA) THEN
         ISITE=STAINF(14,ISTA)+EPS
         IF(ISITE.LE.0) GO TO 50
      ELSE
         ISITE=JSTA
      ENDIF
      ISTAOL=ISTAOL+1
!   PROCESS EAST FREQUENCIES AT ISTA STATION
      NEMD=NMD(1,ISITE)
      IF(NEMD.LE.0) GO TO 150
      IPV=IPVOL(1,ISITE)-1
      IPTE=KIOLM(1,ISITE)-1+KIOLPM-1
      IF(.NOT.LXTRA) THEN
         DO 100 I=1,NEMD
         JFREQ=II(IPTE+I)
         AEAST=PARMV(IPV+I)
         BEAST=PARMV(IPV+I+NEMD)
         ABCOF(JFREQ,1,1,ISTAOL)=ABCOF(JFREQ,1,1,ISTAOL)+AEAST          &
     &                          *XLOCV(1,1,ISTA)
         ABCOF(JFREQ,2,1,ISTAOL)=ABCOF(JFREQ,2,1,ISTAOL)+BEAST          &
     &                          *XLOCV(1,1,ISTA)
         ABCOF(JFREQ,1,2,ISTAOL)=ABCOF(JFREQ,1,2,ISTAOL)+AEAST          &
     &                          *XLOCV(2,1,ISTA)
         ABCOF(JFREQ,2,2,ISTAOL)=ABCOF(JFREQ,2,2,ISTAOL)+BEAST          &
     &                          *XLOCV(2,1,ISTA)
         ABCOF(JFREQ,1,3,ISTAOL)=ABCOF(JFREQ,1,3,ISTAOL)+AEAST          &
     &                          *XLOCV(3,1,ISTA)
         ABCOF(JFREQ,2,3,ISTAOL)=ABCOF(JFREQ,2,3,ISTAOL)+BEAST          &
     &                          *XLOCV(3,1,ISTA)
  100    CONTINUE
      ELSE
         DO 110 I=1,NEMD
         JFREQ=II(IPTE+I)
         AEAST=PARMV(IPV+I)
         BEAST=PARMV(IPV+I+NEMD)
         IF(LEOPTO.AND.ISTA.EQ.0) THEN
            ABCOF(JFREQ,1,1,ISTAOL)=ABCOF(JFREQ,1,1,ISTAOL)+AEAST
            ABCOF(JFREQ,2,1,ISTAOL)=ABCOF(JFREQ,2,1,ISTAOL)-BEAST
            ABCOF(JFREQ,1,2,ISTAOL)=ABCOF(JFREQ,1,2,ISTAOL)-BEAST
            ABCOF(JFREQ,2,2,ISTAOL)=ABCOF(JFREQ,2,2,ISTAOL)-AEAST
         ELSE
            ABCOF(JFREQ,1,1,ISTAOL)=ABCOF(JFREQ,1,1,ISTAOL)+AEAST
            ABCOF(JFREQ,2,1,ISTAOL)=ABCOF(JFREQ,2,1,ISTAOL)+BEAST
         ENDIF
  110    CONTINUE
      ENDIF
  150 CONTINUE
!   PROCESS NORTH FREQUENCIES AT ISTA STATION
      NNMD=NMD(2,ISITE)
      IF(NNMD.LE.0) GO TO 250
      IPV=IPVOL(2,ISITE)-1
      IPTN=KIOLM(2,ISITE)-1+KIOLPM-1
      IF(.NOT.LXTRA) THEN
         DO 200 I=1,NNMD
         JFREQ=II(IPTN+I)
         ANORTH=PARMV(IPV+I)
         BNORTH=PARMV(IPV+I+NNMD)
         ABCOF(JFREQ,1,1,ISTAOL)=ABCOF(JFREQ,1,1,ISTAOL)+ANORTH         &
     &                          *XLOCV(1,2,ISTA)
         ABCOF(JFREQ,2,1,ISTAOL)=ABCOF(JFREQ,2,1,ISTAOL)+BNORTH         &
     &                          *XLOCV(1,2,ISTA)
         ABCOF(JFREQ,1,2,ISTAOL)=ABCOF(JFREQ,1,2,ISTAOL)+ANORTH         &
     &                          *XLOCV(2,2,ISTA)
         ABCOF(JFREQ,2,2,ISTAOL)=ABCOF(JFREQ,2,2,ISTAOL)+BNORTH         &
     &                          *XLOCV(2,2,ISTA)
         ABCOF(JFREQ,1,3,ISTAOL)=ABCOF(JFREQ,1,3,ISTAOL)+ANORTH         &
     &                          *XLOCV(3,2,ISTA)
         ABCOF(JFREQ,2,3,ISTAOL)=ABCOF(JFREQ,2,3,ISTAOL)+BNORTH         &
     &                          *XLOCV(3,2,ISTA)
  200    CONTINUE
      ELSE
         IF(.NOT.LEOPTO.OR.ISTA.NE.0) THEN
            DO 210 I=1,NNMD
            JFREQ=II(IPTN+I)
            ANORTH=PARMV(IPV+I)
            BNORTH=PARMV(IPV+I+NNMD)
            ABCOF(JFREQ,1,2,ISTAOL)=ABCOF(JFREQ,1,2,ISTAOL)+ANORTH
            ABCOF(JFREQ,2,2,ISTAOL)=ABCOF(JFREQ,2,2,ISTAOL)+BNORTH
  210       CONTINUE
         ENDIF
      ENDIF
  250 CONTINUE
!   PROCESS VERT FREQUENCIES AT ISTA STATION
      NVMD=NMD(3,ISITE)
      IF(NVMD.LE.0) GO TO 350
      IPV=IPVOL(3,ISITE)-1
      IPTV=KIOLM(3,ISITE)-1+KIOLPM-1
      IF(.NOT.LXTRA) THEN
         DO 300 I=1,NVMD
         JFREQ=II(IPTV+I)
         AVERT=PARMV(IPV+I)
         BVERT=PARMV(IPV+I+NVMD)
         ABCOF(JFREQ,1,1,ISTAOL)=ABCOF(JFREQ,1,1,ISTAOL)+AVERT          &
     &                          *XLOCV(1,3,ISTA)
         ABCOF(JFREQ,2,1,ISTAOL)=ABCOF(JFREQ,2,1,ISTAOL)+BVERT          &
     &                          *XLOCV(1,3,ISTA)
         ABCOF(JFREQ,1,2,ISTAOL)=ABCOF(JFREQ,1,2,ISTAOL)+AVERT          &
     &                          *XLOCV(2,3,ISTA)
         ABCOF(JFREQ,2,2,ISTAOL)=ABCOF(JFREQ,2,2,ISTAOL)+BVERT          &
     &                          *XLOCV(2,3,ISTA)
         ABCOF(JFREQ,1,3,ISTAOL)=ABCOF(JFREQ,1,3,ISTAOL)+AVERT          &
     &                          *XLOCV(3,3,ISTA)
         ABCOF(JFREQ,2,3,ISTAOL)=ABCOF(JFREQ,2,3,ISTAOL)+BVERT          &
     &                           *XLOCV(3,3,ISTA)
  300    CONTINUE
      ELSE
         DO 310 I=1,NVMD
         JFREQ=II(IPTV+I)
         AVERT=PARMV(IPV+I)
         BVERT=PARMV(IPV+I+NVMD)
         ABCOF(JFREQ,1,3,ISTAOL)=ABCOF(JFREQ,1,3,ISTAOL)+AVERT
         ABCOF(JFREQ,2,3,ISTAOL)=ABCOF(JFREQ,2,3,ISTAOL)+BVERT
  310    CONTINUE
      ENDIF
  350 CONTINUE
      GO TO 50
 1000 CONTINUE
!
! CHECK TO SEE IF ANY OF THE FREQUENCIES WILL USE THE LAST TWO
! ARGS IN ANGFAC
      L2FOLT=.FALSE.
      DO 2000 I=1,NTOLFR
      IF(ANGFAC(I,4).GE.0.D0) THEN
         IFACT1=ANGFAC(I,4)+EPS
      ELSE
         IFACT1=ANGFAC(I,4)-EPS
      ENDIF
      IF(ANGFAC(I,5).GE.0.D0) THEN
         IFACT2=ANGFAC(I,5)+EPS
      ELSE
         IFACT2=ANGFAC(I,5)-EPS
      ENDIF
      IF(IFACT1.NE.0.OR.IFACT2.NE.0) L2FOLT=.TRUE.
 2000 END DO
      RETURN
      END
