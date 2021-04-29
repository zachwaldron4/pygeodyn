!$Q9IC64
      SUBROUTINE Q9IC64(XIN,XOUT,ILEN,ISTAT)
!********1*********2*********3*********4*********5*********6********
!  Q9IC64          83/10/31            0000.0    PGMR - DIANE SEVITS
!
! FUNCTION:  REPLACEMENT ON IBM MACHINES FOR CYBER ROUTINE TH
!            CONVERTS FLOATING POINT NUMBERS FROM IBM 64 BIT
!            TO CYBER 64 BIT REPRESENTATION. THIS ROUTINE SIM
!            COPIES DATA FROM XIN TO XOUT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   --------------------------------------------
!   XIN      I    A    INPUT ARRAY OF 64 BIT FLOATING POINT NUM
!   XOUT     O    A    OUTPUT ARRAY OF 64 BIT FLOATING POINT NUM
!   ILEN     I    S    LENGTH OF IN ARRAY TO BE CONVERTED
!   ISTAT    O    S    CONDITION CODE
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6********
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION XIN(1),XOUT(1)
!
!*******************************************************************
! START OF EXECUTABLE CODE *****************************************
!*******************************************************************
!
! ON IBM COMPUTER LOAD FROM XIN TO XOUT
!
      DO 100 I=1,ILEN
      XOUT(I)=XIN(I)
  100 END DO
      ISTAT=0
      RETURN
      END
