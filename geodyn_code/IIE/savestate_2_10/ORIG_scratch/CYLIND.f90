!$CYLIND
      SUBROUTINE CYLIND(STAP,CYL,DXMDPM,LXYON,LPARON)
!********1*********2*********3*********4*********5*********6*********7**
! CYLIND           83/10/25            8311.0    PGMR - D. ROWLANDS
!
! FUNCTION:  1) TO COMPUTE CYLINDRICAL R,LAMBDA,Z FROM
!               GEOCENTRIC X, Y, Z
!            2) TO COMPUTE PARTIAL DERIVATIVES OF GEOCENTRIC
!               COORDINATES WITH RESPECT TO CYLINDRICAL
!               COORDINATES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   STAP     I    A    XYZ STATION POSITION
!   CYL     I/O   A    INPUT (IF LPARON=.TRUE.) OUTPUT OTHERWISE
!   CYL(1)  I/O   A    XY COMPONENT OF RADIUS VECT
!   CYL(2)  I/O   A    LONGITUDE
!   CYL(3)  I/O   A    Z
!   DXMDPM  I/O   A    PARTIALS OF XYZ WRT CYLINDRICAL
!                      OUTPUT (IF LXYON=.FALSE.)
!   LXYON    I    S    .TRUE. IF THE PARTIALS OF GEOCENTRIC WRT
!                      CYLINDRICAL ARE NOT NEEDED.
!   LPARON   I    S    .TRUE.IF ONLY THE PARTIALS OF GEOCENTIC WRT
!                      CYLINDRICAL ARE NEEDED
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION STAP(3),DXMDPM(3,3),CYL(3)
      DATA DELTA/0.001D0/
      DATA ZERO/0.0D0/,ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(LPARON) GO TO 100
      CYL(1)=STAP(1)*STAP(1)+STAP(2)*STAP(2)
      CYL(1)= SQRT(CYL(1))
      CYL(2)= ATAN2(STAP(2),STAP(1))
      CYL(3)=STAP(3)
! CALCULATE CYLINDRICAL COORDINATES
      IF(LXYON) RETURN
  100 CONTINUE
!   CALCULATE SOME CONSTANTS NECESSARY FOR PARTIALS
      COSL= COS(CYL(2))
      SINL= SIN(CYL(2))
      DXMDPM(1,1)=COSL
      DXMDPM(2,1)=-CYL(1)*SINL
      DXMDPM(3,1)=ZERO
      DXMDPM(1,2)=SINL
      DXMDPM(2,2)=CYL(1)*COSL
      DXMDPM(3,2)=ZERO
      DXMDPM(1,3)=ZERO
      DXMDPM(2,3)=ZERO
      DXMDPM(3,3)=ONE
      RETURN
      END
