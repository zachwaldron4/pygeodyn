!$FNDRN
      SUBROUTINE FNDRN(RNUM,ARRAY,JDIM,IRET)
!********1*********2*********3*********4*********5*********6*********7**
! FNDRN           00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  POINT TO THE CLOSEST REAL NUMBER OF ARRAY GIVEN RNUM
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   RNUM     I    S    NUMBER SEARCHED FOR
!   ARRAY    I    A    ARRAY TO BE SEARCHED FOR INUM
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
      DIMENSION ARRAY(1)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      IRET=0
!
! SEARCH IARRAY FOR INUM
!
      DIFFP=10000000.D0
      IX=0
      DO 10 I=1,JDIM
      DIFF=ABS(RNUM-ARRAY(I))
      IF(DIFF.LE.DIFFP) THEN
      DIFFP=DIFF
      IX=I
      ENDIF
   10 END DO
      IRET=IX
      RETURN
      END
