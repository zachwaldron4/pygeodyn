!$FNDCHR
      SUBROUTINE FNDCHR(INUM,IARRAY,JDIM,IRET)
!********1*********2*********3*********4*********5*********6*********7**
!
! FUNCTION:  TO LOACTE A GIVEN INTEGER NUMBER IN AN ARRAY OF
!            INTEGER NUMBERS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   INUM     I    S    NUMBER SEARCHED FOR
!   IARRAY   I    A    ARRAY TO BE SEARCHED FOR INUM
!   JDIM     I    S    DIMENSION OF IARRAY. SEARCH WILL BE FROM
!                      1 TO JDIM.
!   IRET     O    S    ELEMENT NUMBER OF IARRAY THAT MATCHED
!                      INUM. IRET=0 MEANS NO MATCH WAS FOUND.
!                      IRET MUST BE LESS THAN OR = TO JDIM.
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      CHARACTER*8 INUM
      CHARACTER*8 IARRAY
      DIMENSION IARRAY(1)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      IRET=0
!
! SEARCH IARRAY FOR INUM
!
      DO 10 I=1,JDIM
      IX=I
      IF(INUM.EQ.IARRAY(I)) GO TO 20
   10 END DO
      RETURN
   20 IRET=IX
      RETURN
      END
