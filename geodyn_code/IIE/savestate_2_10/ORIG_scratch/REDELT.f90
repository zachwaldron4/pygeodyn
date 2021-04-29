!$REDELT
      SUBROUTINE REDELT(DELTA,NTOT,ILINK,ICLINK)
!********1*********2*********3*********4*********5*********6*********7**
! REDELT
!
!
! FUNCTION:  EXPANDS THE PDELTA ARRAY IF LINKS EXIST AND IT WAS
!            PREVIOUISLY REDUCED
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   DELTA    I    A    THE CORRECTIONS ARRAY
!   NTOT     I/O  S    DIMENSION OF THE DELTA ARRAY
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION DELTA(1),ILINK(ICLINK,2)
      DIMENSION V(500)
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO I=1,500
      V(I)=0.D0
      ENDDO

      JJ=NTOT
! DEBUG DELTA BEFORE
!     DO I=1,NTOT
!     WRITE(6,*)' DELTA BEFORE ', DELTA(I),I
!     ENDDO
! DEBUG DELTA BEFORE

! ICLINK= NUMBER OF LINKS
      DO 1000 ITIMES=1,ICLINK
! COMPUTE THE TOTAL SIZE OF ATPA
      IONE=ILINK(ITIMES,1)
      ITWO=ILINK(ITIMES,2)
! MOVE ELEMENTS UP
      DO I= ITWO,JJ
      V(I)=DELTA(I)
      ENDDO
      DO I= ITWO,JJ
      DELTA(I+1)=V(I)
      ENDDO
! FILL UP THE ITWO ELEMENT WITH IONE
      DELTA(ITWO)=DELTA(IONE)

 1000 END DO

! DEBUG DELTA AFTER
!     DO I=1,JJ+1
!     WRITE(6,*)' DELTA AFTER ', DELTA(I),I
!     ENDDO
! DEBUG DELTA AFTER



      RETURN
      END
