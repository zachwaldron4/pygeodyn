!$FMROT
      SUBROUTINE FMROT(EF,ZF,TF,PR50TD,NT)
!**************************************************************
!
! VERSION 8606.0 DATE 06/12/86      PRGMMR D.PAVLIS
!
! FUNCTION              FORMS THE ROTATION MATRIX FROM THE
!                       MEAN REFERENCE FRAME OF 1950.0 TO
!                       THE MEAN OF DATE REFERENCE FRAME
!
!                       EF
! INPUT PARAMETERS      ZF= ELEMENTS OF PRECESSION SINCE 1950.0
!                       TF  IN RADIANS
!
! OUTPUT                THE ROTATION MATRIX MEAN OF 1950.0
!                       TO MEAN OF DATE
!
! NOTE                  COORDINATES REF TO THE MEAN REFERENCE
!                       FRAME OF 1950.0 MULTIPLIED BY PR50TD
!                       GIVE COORDINATES IN THE MEAN OF DATE
!                       REFERENCE FRAME
!
!**************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION PR50TD(3,3,NT),EF(NT),ZF(NT),TF(NT)
!**********************************************************************
!* START OF EXECUTABLE CODE
!**********************************************************************
!
      DO 10 I=1,NT
!
         SINF=SIN(EF(I))
         COSF=COS(EF(I))
         SINZ=SIN(ZF(I))
         COSZ=COS(ZF(I))
         SINT=SIN(TF(I))
         COST=COS(TF(I))
!
         A=COSZ*COSF
         B=SINZ*COSF
         C=COST*SINF
!
         PR50TD(1,1,I)= A*COST-SINZ*SINF
         PR50TD(1,2,I)=-COSZ*C-B
         PR50TD(1,3,I)=-COSZ*SINT
         PR50TD(2,1,I)= B*COST+COSZ*SINF
         PR50TD(2,2,I)=-SINZ*C+A
         PR50TD(2,3,I)=-SINZ*SINT
         PR50TD(3,1,I)= SINT*COSF
         PR50TD(3,2,I)=-SINT*SINF
         PR50TD(3,3,I)= COST
!
!      if( i.eq.1) then
!       write(6,*) 'fmrot: pr50td ',
!     &            ((pr50td(iii,jjj,i),iii=1,3),jjj=1,3)
!      endif
   10 END DO
!
      RETURN
      END
