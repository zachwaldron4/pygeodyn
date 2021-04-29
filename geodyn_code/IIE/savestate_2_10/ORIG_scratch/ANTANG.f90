!$ANTANG
      SUBROUTINE ANTANG(ISEQ,ROT,U1,U2,U3,LEDIT)
!********1*********2*********3*********4*********5*********6*********7**
! ANTANG                                         PGMR - DESPINA PAVLIS
!
!
! FUNCTION:  COMPUTE ANGLE BETWEEN ANTENNA NORMAL AND
!            THE REVERSE LIGHT PATH GOING FROM SATELLITE TO GPS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ISEQ     I    S    SEQUENCE NUMBER FROM ROBS (WHICH SEQ OF DIFF 1-4
!   ROT      I    A    TOTAL ROTATION MATRIX FROM SBF TO TOD
!   U1-3     I    A    UNIT INERTIAL TOPOCENTRIC S/C POSITION
!   LEDIT    O    S    EDITING FLAG FOR THIS OBSERVATION
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE


      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)
      DIMENSION ROT(9),DUM(3)
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************

      A2NRM1 = VLOW(1,3,ISEQ)
      A2NRM2 = VLOW(2,3,ISEQ)
      A2NRM3 = VLOW(3,3,ISEQ)
      CUTOFF = XCUTSL(ISEQ)
!      write(6,*)'antang:  ISEQ   ',ISEQ
!      write(6,*)'antang:  A2NRM1 ',A2NRM1
!      write(6,*)'antang:  A2NRM2 ',A2NRM2
!      write(6,*)'antang:  A2NRM3 ',A2NRM3
!      write(6,*)'antang:  CUTOFF ',CUTOFF

! ROTATE SATELLITE GPS ANTENNA 2 NORMAL FROM SBF TO INERTIAL

      DUM(1) = ROT(1)*A2NRM1+ROT(4)*A2NRM2+ROT(7)*A2NRM3
      DUM(2) = ROT(2)*A2NRM1+ROT(5)*A2NRM2+ROT(8)*A2NRM3
      DUM(3) = ROT(3)*A2NRM1+ROT(6)*A2NRM2+ROT(9)*A2NRM3

! COMPUTE ANGLE BETWEEN ANTENNA NORMAL AND THE REVERSE LIGHT PATH

! .... GOING UP FROM SATELLITE TO GPS

      TMP = DUM(1)*(-U1)+DUM(2)*(-U2)+DUM(3)*(-U3)
      IF(TMP.GT. 1.D0) TMP= 0.99999999999D0
      IF(TMP.LT.-1.D0) TMP=-0.99999999999D0
      ANG = 90.0D0-(ACOS(TMP)/DEGRAD)

      IF( ANG .LT. CUTOFF ) THEN
          LEDIT = .TRUE.
      ENDIF

      !write(6,*)'antang: ISEQ, ANG, CUTOFF, LEDIT ',&
      !                   ISEQ, ANG, CUTOFF, LEDIT

      RETURN
      END
