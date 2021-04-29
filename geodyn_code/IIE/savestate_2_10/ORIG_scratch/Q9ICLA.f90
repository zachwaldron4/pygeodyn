!$Q9ICLA
      SUBROUTINE Q9ICLA(XIN,XOUT,ILEN,ISTAT)
!********1*********2*********3*********4*********5*********6********
! Q9ICLA           89/12/27            0000.0    PGMR - SBL
!
! FUNCTION:  IF A FORMATTED INTERFACE FILE THEN SIMPLY EXCHANGE
!            XIN TO XOUT. IF BINARY INTERFACE FILE THEN CONVERT
!            TO PROPER MACHINE NATIVE BINARY
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XIN      I    A    INPUT ARRAY OF 64 BIT FLOATING POINT NUM
!   XOUT     O    A    OUTPUT ARRAY OF 64 BIT FLOATING POINT NUM
!   ILEN     I    S    LENGTH OF IN ARRAY TO BE CONVERTED
!   ISTAT    O    S    CONDITION CODE
!
! COMMENTS:  This routine was originally written to accomodate running
!             2E on the CYBER with the Q9IC64 conversion routine.
!
!********1*********2*********3*********4*********5*********6********
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CLASCI/LASCII,LIFCYB,LARECL,NXLASC
      COMMON/CORE2S/KORE2S(3,2),KSCL2S(3),NGRP2S,MACH2S,KCOM2S,MAXC2E,  &
     &              NXCORS
!
      DIMENSION XIN(1),XOUT(1)
      DATA ICMP2E/3/
!
!*******************************************************************
! START OF EXECUTABLE CODE *****************************************
!*******************************************************************
!
!     IF(LASCII.OR.(KCOM2S.EQ.ICMP2E)) THEN
      IF(LASCII.OR..TRUE.) THEN
      DO 100 I=1,ILEN
      XOUT(I)=XIN(I)
  100 END DO
      ISTAT=0
      ELSE
      ISTAT=0
! IF THE 2E IS THE CRAY THEN USE APPROPRIATE CONVERSION ROUTINE
! IF ANY OTHER MACHINE TERMINATE SINCE NO OTHER 2E MACHINE WILL ACCEPT A
! DIFFERENT MACHINES BINARY FILES (AT THE MOMENT ONLY THE CRAY WILL)
      WRITE(6,10000)
      STOP 69
      ENDIF
      RETURN
10000 FORMAT(1X,'EXECUTION TERMINATING IN Q9ICLA.'/                     &
     &     1x, ' CURRENTLY GEODYN DOES NOT ACCEPT THIS MACHINE '/       &
     &     1x, '2S-2E COMBINATION FOR BINARY INTERFACE AND DATA FILES')
      END
