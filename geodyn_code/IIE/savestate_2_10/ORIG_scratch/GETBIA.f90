!$GETBIA
      SUBROUTINE GETBIA(F,BIAS,ACCDOT)
!********1*********2*********3*********4*********5*********6*********7**
! GETBIA           00/00/00            0000.0    PGMR - D. E. Pavlis
!
! FUNCTION:  GET BIASES FOR ACCELEROMETRY
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION F(3),BIAS(3,3)
      DIMENSION ACCDOT(1)
      DATA kentry/0/

!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      kentry = kentry + 1

! ***  PRINT INCOMING INFORMATION  ************************************

      IF(KENTRY.EQ.1)  THEN


      ENDIF
      DO I=1,3
      F(I)=F(I)*(1.D0+BIAS(I,2))+ACCDOT(1)*BIAS(I,3)+BIAS(I,1)
      ENDDO
      RETURN
      END
