
!$VM7WR
      SUBROUTINE VM7WR(DWORD,PXP,PYP,PZP,PXV,PYV,PZV,NEQN)
!********1*********2*********3*********4*********5*********6*********7**
! VM7WR            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   DWORD
!   PXP
!   PYP
!   PZP
!   PXV
!   PYV
!   PZV
!   NEQN
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION DWORD(12),PXP(NEQN),PYP(NEQN),PZP(NEQN)
      DIMENSION PXV(NEQN),PYV(NEQN),PZV(NEQN)
      DATA IUNT80/80/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      WRITE(IUNT80) DWORD,PXP,PYP,PZP,PXV,PYV,PZV
      RETURN
      END
