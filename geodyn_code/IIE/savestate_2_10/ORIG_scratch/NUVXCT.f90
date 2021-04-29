!$NUVXCT
       SUBROUTINE NUVXCT(MJDSC,FSEC,DPSI,EPST,EPSM,NM,SCRTCH)
!********1*********2*********3*********4*********5*********6*********7**
! NUVXCT           86/12/24            8701.0    PGMR - D. ROWLAND
!
! FUNCTION:  DUMMY ROUTINE FOR PLANETES OTHER THAN EARTH.
!            USED TO PARALLEL SUBROUTINE NUVECT.
!
!
! COMMENTS:  THIS ROUTINE DOES NOT DO ANY CALCULATION.  ARGUMENTS ARE
!            COPIED FROM S/R NUVECT
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION FSEC(NM),DPSI(NM),EPST(NM),EPSM(NM),SCRTCH(NM,5)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      RETURN
      END
