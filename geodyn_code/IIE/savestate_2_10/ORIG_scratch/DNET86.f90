!$DNET86
      FUNCTION DNET86(DD,DM,ZHM,XMM,XM)
!********1*********2*********3*********4*********5*********6*********7**
! DNET86             00/00/00            0000.0    PGMR - J. RIDGEWAY
!
! FUNCTION -  TURBOPAUSE CORRECTION FOR MSIS MODELS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   DD
!   DM
!   ZHM
!   XMN
!   XM
!
! COMMENTS:
!
! REFERENCES:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER( DM10 = -10.D0 )
      PARAMETER( D10  =  10.D0 )
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!       Eq. A12b
      A=ZHM/(XMM-XM)
!       Eq. A12a
      YLOG= A * LOG(DM/DD)
!
      IF(YLOG.LT. DM10 ) THEN
!
        DNET86=DD
        RETURN
!
      ELSE IF(YLOG .GT. D10 ) THEN
!
        DNET86=DM
        RETURN
!
      ELSE
!
        DNET86=DD*(1.D0+EXP(YLOG))**(1/A)
!
      ENDIF
      RETURN
      END
