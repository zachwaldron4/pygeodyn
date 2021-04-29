!$ELCUT
      SUBROUTINE ELVCUT (LEDIT ,ELEVSC,NM ,STAINF,ISTAEL,NELEVS,LANTCT)
!********1*********2*********3*********4*********5*********6*********7**
! ELCUT            85/05/06            8505.0    PGMR - TOM MARTIN
!
! FUNCTION:  EDIT OBSERVATIONS BASED UPON ELEVATION CUTOFF.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   LEDIT   I/O   A    LOGICAL EDIT SWITCHES AS INPUT
!                      UPDATED LOGICAL EDIT SWITCHES AS OUTPUT
!   ELEVSC   I    A    ELEVATION ANGLES FOR EACH STATION
!   NM       I    S    NUMBER OF MEASUREMENTS
!   STAINF   I    A    STATION INFORMATION ARRAY
!   ISTAEL   I    A    INDICES INTO STAINF
!   NELEVS   I    S    NUMBER OF STATION ELEVATIONS
!   LANTCT   I    A    ANTENNA CUTOFF LOGICAL ARRAY
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION LEDIT(NM),ELEVSC(NM,12),STAINF(NSTAIN,1),ISTAEL(12)
      DIMENSION LANTCT(1)
      INCLUDE 'COMMON_DECL.inc'
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CSTATI/NOBTOT,NOBWTD,NAPTOT,NCUTOF
      COMMON/SATCUT/CUTSAT,XSATCU
! scott
!      DATA NANTCT/0/
! scott
      DATA DEFEL/-10.D0/
!gps     DATA DEFEL/10.D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!      print *,'elcut: inside elcut'
! COUNT THE NUMBER OF EDITED MEASUREMENTS BEFORE ELEVATION TEST
      NEDITB=0
      DO 500 I=1,NM
      IF (LEDIT(I)) NEDITB=NEDITB+1
  500 END DO
!     write(6,*) 'ELCUT : NEDITB ', NEDITB
!     write(6,*) 'ELCUT : nelevs ', nelevs
!
      DO 2000 I=1,NELEVS
!      CUTOFF=DEFEL
         CUTOFF=CUTSAT
         IF(ISTAEL(I).GT.0) THEN
            J=ISTAEL(I)
            CUTOFF=STAINF(12,J)
!          write(6,*) ' ELCUT : J, CUTOFF  ',J,CUTOFF
         ENDIF
         DO 1000 N=1,NM
!          write(6,*) ' ELCUT : ELEVSC, LEDIT  ',ELEVSC(N,I),LEDIT(N)
            LEDIT(N)=LEDIT(N).OR.ELEVSC(N,I).LT.CUTOFF
! scott
            ! lantct is the gps satellite antenna elev edit switch
            LEDIT(N)=LEDIT(N).OR.LANTCT(N)
!            if(lantct(n)) then
!            print *,'ELCUT: n ledit lantct: ',n,ledit(n),lantct(n)
!            endif
! scott
!          write(6,*) ' ELCUT a: i,cutoff, LEDIT  ',i,cutoff,LEDIT(N)
 1000    CONTINUE
 2000 END DO
!
! COUNT THE NUMBER OF EDITED MEASUREMENTS AFTER ELEVATION TEST
      NEDITA=0
      DO 2500 I=1,NM
! scott debug
!       if(lantct(i)) nantct=nantct+1
! scott degug
      IF (LEDIT(I)) NEDITA=NEDITA+1
 2500 END DO

!      write(6,*)'ELCUT: number edited NEDITA ', NEDITA

!  COMPUTE THE NUMBER OF MEASUREMENTS EDITED BY CUTOFF TEST
      NCUTOF=NCUTOF + (NEDITA-NEDITB)

!      write(6,*)  'ELCUT: number edited sta elev NCUTOF ',NCUTOF
! scott debug
!       write(6,*) 'ELCUT: number edited gps ant elev NANTCT ',nantct
! scott debug

      RETURN
      END
