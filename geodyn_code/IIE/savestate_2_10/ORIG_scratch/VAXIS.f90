!$VAXIS
      SUBROUTINE VAXIS(VECT,PVPX,VNORM,VMAT,SIGNZ)
!********1*********2*********3*********4*********5*********6*********7**
! VAXIS            00/00/00            8804.0    PGMR - TVM
!
! FUNCTION: COMPUTE THE VARIATIONAL PARTIALS OF AN INERTIALLY
!           ORIENTED NORMAL TO THE S/C POSITION VECTOR.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   VECT               INERTIAL VECTOR NORMAL TO S/C POSITION VECTOR.
!   PVPX               PARTIAL OF "VECT" W.R.T. POSITION (POSITION
!                      PARTIALS) OR VELOCITY (VELOCITY PARTIALS)
!   VNORM              1/^VECT^
!   VMAT               PARTIALS OF YAXIS W.R.T. PSAT (AND W.R.T. VECT IF
!                      IS VEL.).
!   SIGNZ              SIGN OF ZAXIS: +1 = RADIAL, -1 = EARTH POINTING.
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION VECT(3),VP(3),PVPX(3,3),VMAT(3,3)
      DATA ZERO/0.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! COMPUTE VECTOR TIMES PARTIALS OF VECTOR W.R.T. POSITION (OR VELOCITY).
      VNORM2=       VNORM**2
      VNORM1= SIGNZ*VNORM
      DO 2000 J=1,3
      SUM=ZERO
      DO 1000 I=1,3
      SUM=SUM+VECT(I)*PVPX(I,J)
 1000 END DO
      VP(J)=SUM*VNORM2
 2000 END DO
! CHAIN PARTIALS
      DO 4000 J=1,3
      VJ=VP  (J)
      DO 3000 I=1,3
      VMAT(I,J)=VNORM1*(PVPX(I,J)-VECT(I)*VJ)
 3000 END DO
 4000 END DO
      RETURN
      END
