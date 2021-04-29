!$BLKDMP
      SUBROUTINE BLKDMP(OBFOUT,DUMP,MDIMEN,IUNT20)
!********1*********2*********3*********4*********5*********6*********7**
! BLKSMP           86/06/04            8606.0    PGMR - TOM MARTIN
!
! FUNCTION:  OUTPUT PHYSICAL BUFFER OF OBSERVATION DATA.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   OBFOUT   I    A    DATA BUFFER TO BE OUTPUT
!   DUMP     I    A    DATA BUFFER TO BE OUTPUT ON THE CYBER
!   MDIMEN   I    S    DIMENSION OF OUTPUT BUFFER.
!   IUNT20   I    S    UNIT NUMBER FOR OUTPUT.
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CORE2E/KORE2E(3,7),KSCL2E(3),NGRP2E,MACH2E,KCOM2E,NXCORE
      COMMON/CORE2S/KORE2S(3,2),KSCL2S(3),NGRP2S,MACH2S,KCOM2S,MAXC2E,  &
     &              NXCORS
      DIMENSION OBFOUT(MDIMEN)
      DIMENSION DUMP(MDIMEN)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      WRITE(IUNT20) OBFOUT
      RETURN
      END
