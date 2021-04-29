!$NSTCHk
      SUBROUTINE NSTCHK(INUM,IARRAY,JMAX,JDIM,IRET)
!********1*********2*********3*********4*********5*********6*********7**
! NSTCHK           08/02/94            9505.0    PGMR - J. McCarthy
!
! FUNCTION:  CHECK IF A GIVEN INTEGER NUMBER IS IN AN ARRAY OF
!            INTEGER NUMBERS.
!            IF (yes)  return 0
!            IF (no) return jmax+1 for new location
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   INUM     I    S    NUMBER SEARCHED FOR
!   IARRAY   I    A    ARRAY TO BE SEARCHED FOR INUM
!   JMAX     I    S    SEARCH WILL BE FROM 1 TO JMAX.
!   JDIM     I    S    DIMENSION OF IARRAY.
!   IRET     O    S    IF (yes)  return IRET=0
!                      IF (no)   return IRET=jmax+1 for new location
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION IARRAY(JDIM)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      IRET=0
!
! SEARCH IARRAY FOR INUM
!
      DO 10 I=1,JMAX
         IX=I
         IF(INUM.EQ.IARRAY(I)) GO TO 20
   10 END DO
      IRET=JMAX+1
      if( IRET .le. JDIM ) then
         RETURN
      else
         write(6,*) 'TOO MANY STATIONS REQUESTED. LIMIT IS ', JDIM
         write(6,*) 'STOPPING PROGRAM IN NSTCHK '
         write(6,*) 'NSTCHK CALLED FROM RDOFFL OR RDOFFP '
         stop 999
      endif
!
   20 CONTINUE
      IRET=0
      RETURN
      END
