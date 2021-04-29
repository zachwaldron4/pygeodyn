!$GETNM
      SUBROUTINE GETNM(NM,LEDIT,NSEL,MJDS,FSEC,HS,TINT)
!********1*********2*********3*********4*********5*********6*********7**
! GETNM            00/00/00            0000.0    PGMR - D. E. Pavlis
!
! FUNCTION:  GET POINTERS FROM NSTEPB TO NM
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!    NM      I    S    NUMBER OF OBSERVATIONS IN THIS BLOCK
!    LEDIT   I/O  A    OBSERVATION EDIUTING ARRAY
!    NSEL    I/O  A    POINTER ARRAY
!    MJDS    I    S    MJD FOR FIRST OBSERVATION
!    FSEC    I    A    FRACTIONAL SECONDS
!    HS     I    S    STEPSIZE
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!

      DIMENSION LEDIT(NM),NSEL(NM),FSEC(NM)

      DATA kentry/0/

!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      kentry = kentry + 1

      T=DBLE(MJDS)
      T1=T+FSEC(1)
      NSEL(1)=1
      TI=T1
!     TI=TINT
      IJ=1


! INITIALIZE LEDIT TO FALSE FOR ALL OBSERVATIONS
      DO I=1,NM
      LEDIT(I)=.FALSE.
      ENDDO

      DO 100 J=1,NM-1
! NEXT OBSERVATION TIME
      TNO=T+FSEC(J+1)
   50 CONTINUE
! NEXT INTEGRATION TIME
      TI=TI+HS
      IJ=IJ+1

! THE OBS TIME NOT EQUAL TO INT TIME
      IF(TNO.NE.TI) THEN

                           !  EDIT OBSERVATION
       IF(TNO.LT.TI) THEN
       LEDIT(J+1)=.TRUE.
       NSEL(J+1)=0
       TI=TI-HS
       GOTO 100

       ELSE

! NOT THERE YET CHECK NEXT INTEGRATION TIME
       GOTO 50

       ENDIF


      ELSE
! THE OBS TIME EQUAL TO INT TIME
      NSEL(J+1)=IJ

      ENDIF
  100 END DO
!     do i=1,nm
!     write(6,*)' dbg nsel ',nsel(i),ledit(i),i
!     enddo
      RETURN
      END
