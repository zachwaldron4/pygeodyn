!$TSELEC86
      SUBROUTINE TSELEC86(SV)
!***********************************************************************
! ROUTINE NAME: TSELEC86  DATE:              PGMMR:  A.HEDIN
!
! FUNCTION -
!
! I/O PARAMETERS:
!
! NAME   I/O  DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST ORDER
! ----   ---  -----------------------------------------------------
! SV
!
! COMMENTS:
!
! REFERENCES:
!
!*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CSW/SW(25),SWC(25),ISW
      DIMENSION SV(1),SAV(25),SVV(1)
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      DO 100 I = 1,25
        SAV(I)=SV(I)
        SW(I) = MOD(SV(I),2.D0)
        IF(ABS(SV(I)).GT.0.D0) THEN
          SWC(I)=1.D0
        ELSE
          SWC(I)=0.D0
        ENDIF
  100 END DO
      ISW=64999
      RETURN
      ENTRY TRETRV86(SVV)
      DO 200 I=1,25
        SVV(I)=SAV(I)
  200 END DO
      END
