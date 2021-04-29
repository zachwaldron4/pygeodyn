!$PDFCHK
      SUBROUTINE PDFCHK(NLEN)
!********1*********2*********3*********4*********5*********6*********7**
! PDFCHK           86/03/28            8603.0    PGMR - TOM MARTIN
!
! FUNCTION:  INCREMENT WORD AND BLOCK COUNT OUTPUT TO PARTIAL
!            DERIVATIVE FILE. INCREMENT UNIT NUMBER IF NECESSARY.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NLEN     I    S    LENGTH OF RECORD ABOUT TO BE WRITTEN.
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CPARFI/IUNPF1,IUNPF2,IUNPFI,ISZPFI,NBLKPF,NWRDPF,MTCALL(4)
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! INCREMENT WORD AND BLOCK COUNTS
      NWRDPF=NWRDPF+NLEN
      NBLOCK=NWRDPF/512
      NWRDPF=NWRDPF-NBLOCK*512
      NBLKPF=NBLKPF+NBLOCK
! RETURN IF 'OK'
      IF(NBLKPF.LE.ISZPFI) RETURN
! CLOSE CURRENT FILE AND NOTIFY USER
      END FILE IUNPFI
      REWIND IUNPFI
      NBLKPF=NBLKPF-NBLOCK
      WRITE(IOUT6,10000) IUNPFI,NBLKPF
! INCREMENT FILE NUMBER AND ATTEMPT TO OPEN NEW FILE.
      IUNPFI=IUNPFI+1
      IF(IUNPFI.GT.IUNPF2) GO TO 9000
! NOTIFY USER OF NEW FILE OPENED.
      WRITE(IOUT6,20000) IUNPFI
      NWRDPF=NLEN
      NBLOCK=NWRDPF/512
      NWRDPF=NWRDPF-NBLOCK*512
      NBLKPF=NBLOCK
      RETURN
! FILE OVERFLOW CONDITION. NOTIFY USER AND TERMINATE.
 9000 CONTINUE
      WRITE(IOUT6,90000) IUNPFI,IUNPF1,IUNPF2
      STOP 16
10000 FORMAT('0','UNIT =',I3,' CLOSED FOR PARTIAL DERIVATIVE FILE ',    &
     &   'OUTPUT WITH ',I10,' BLOCKS OF LENGTH 512 WORDS WRITTEN.')
20000 FORMAT('0','UNIT =',I3,' OPENED FOR PARTIAL DERIVATIVE FILE ',    &
     &   'OUTPUT.')
90000 FORMAT(' ** PDFCHK **  PARTIAL DERIVATIVE FILE UNIT NUMBER ',     &
     &   'OUT OF RANGE.'/'0',14X,'UNIT NUMBER =',I12/                   &
     &   1X ,14X,'UNIT RANGE  =',I3,' TO ',I3)
      END
