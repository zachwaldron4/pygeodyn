!$VMTOE
      SUBROUTINE VMTOE(PXPFM,MEQN)
!********1*********2*********3*********4*********5*********6*********7**
! VMTOE            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PXPFM
!   MEQN
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION DWORD(12),PXPFM(MEQN,6)
      DATA DWORD/12*-999999.D0/,IUNT80/80/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      WRITE(IUNT80) DWORD,PXPFM
      RETURN
      END
