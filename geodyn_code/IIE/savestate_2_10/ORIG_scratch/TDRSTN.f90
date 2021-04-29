!$TDRSTN
      SUBROUTINE TDRSTN(I,CTHETA,OMTRNS)
!*******************************************************************
!  ROUTINE NAME:   TDRSTN   DATE: 08/11/94      PGMR: S.B. LUTHCKE
!
!   FUNCTION - COMPUTE ONE MINUS THE TRANSMISSIVITY FOR THE TDRSS
!              TDRSS SPACECRAFT SINGLE ACCESS ANTENNAE.  THE TRANS.
!              IS A FUNCTION OF INCIDENCE ANGLE.
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   I        S     I/O   PLATE NUMBER
!   CTHETA   S     I/O   COS OF INCIDENCE ANGLE
!   OMTRNS   S      O    ONE MINUS THE TRANSMISSIVITY FOR THIS PLATE
!
! NOTE:  THIS ROUTINE IS ONLY INTENDED FOR THE TDRSS SA ANTENNAE
!
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DATA DEGRAD/1.7453292519943296D-2/
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,THREE/3.0D0/
      DATA R0/0.0973083D0/,R1/0.00359112D0/,R2/-0.00131963D0/
      DATA R3/0.000206535D0/,R4/-1.62848D-05/,R5/7.35966D-07/
      DATA R6/-2.02088D-08/,R7/3.42249D-10/,R8/-3.48396D-12/
      DATA R9/1.94935D-14/,R10/-4.59178D-17/
!
      data kentry/0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!      kentry=kentry + 1
!      write(6,*) 'tdrstn: kentry test',kentry
!      if(kentry .gt. 2000) stop 69
!
!      print *,'i,ctheta,degrad: ',i,ctheta,degrad
      IF(CTHETA.GT.1.1D0 .OR. CTHETA.LT.-0.1D0) GOTO 20000
      IF(CTHETA.GT.ONE) CTHETA=ONE
      IF(CTHETA.LT.ZERO) CTHETA=ZERO
      ANG = ACOS(CTHETA)/DEGRAD
!      print *,'ang: ',ang
      OMTRNS = R0 + (R1*ANG) + (R2*ANG**2) + (R3*ANG**3) + (R4*ANG**4) +&
     &         (R5*ANG**5) + (R6*ANG**6) + (R7*ANG**7) + (R8*ANG**8) +  &
     &         (R9*ANG**9) + (R10*ANG**10)
!      print *,'tdrstn: i,ang,omtrns: ',i,ang,omtrns
      IF(OMTRNS.GT.1.4D0 .OR. OMTRNS.LT.ZERO) GOTO 10000
      IF(OMTRNS.GT.ONE) OMTRNS=ONE
      RETURN
10000 WRITE(6,10001) OMTRNS
10001 FORMAT(1X,'*** STOPPED IN TDRSTN: OMTRNS: ',D24.16)
      print *,'i,ctheta,degrad: ',i,ctheta,degrad
      print *,'tdrstn: i,ang,omtrns: ',i,ang,omtrns
      STOP 69
20000 WRITE(6,20001) CTHETA
20001 FORMAT(1X,'*** STOPPED IN TDRSTN: CTHETA: ',D24.16)
      STOP 69
      END
