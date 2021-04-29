!$PDFDAT
      SUBROUTINE PDFDAT(WORD50,PARTL,NADJST)
!********1*********2*********3*********4*********5*********6*********7**
! PDFDAT           86/03/28            8603.0    PGMR - TOM MARTIN
!
! FUNCTION:  OUTPUT ONE PARTIAL DERIVATIVE RECORD.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   WORD50   I    A    FIRST 50 WORDS OF RECORD
!   PARTL    I    A    PARTIAL DERIVATIVES
!   NADJST   I    S    NUMBER OF PARTIAL DERIVATIVES
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CPARFI/IUNPF1,IUNPF2,IUNPFI,ISZPFI,NBLKPF,NWRDPF,MTCALL(4)
      DIMENSION WORD50(50),PARTL(NADJST)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! VERIFY THAT FILE WILL NOT OVERFLOW
      NLEN=NADJST+50
      CALL PDFCHK(NLEN)
! OUTPUT DATA RECORD
      WRITE(IUNPFI) WORD50,PARTL
      RETURN
      END
