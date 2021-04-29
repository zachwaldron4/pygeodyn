!$TSELEV
      SUBROUTINE TSELEV(SV)
!
!     ....called by VTS3 for Venus Atmosphere Model of Hedin
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
      DIMENSION SV(1)
!
      COMMON/VCSW/SWV(15),ISWV
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      DO 100 I=1,15
  100 SWV(I)=SV(I)
      ISWV=64999
      RETURN
      END
