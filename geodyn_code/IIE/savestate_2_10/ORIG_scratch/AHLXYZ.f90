!$AHLXYZ
      SUBROUTINE AHLXYZ(X1)
!********1*********2*********3*********4*********5*********6*********7**
! PLRXYZ           95/11/20            9510.0    PGMR- DESPINA PAVLIS
!
! FUNCTION:  CONVERT SPHERICAL PLANETARY COORDINATES TO PLANET CENTERED
!            FIXED CARTESIAN
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   X1       I    A    INERTIAL CARTESIAN POSITION VECTORS
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/PTAROT/TAROT(3,3),RADLAN
      DIMENSION X1(3),X2(3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      write(6,*)' enter AHL ',x1(1),x1(2),x1(3)
      HEIGHT=X1(2)
      COSL=COS(X1(3))
      SINL=SIN(X1(3))
      AXIS=X1(1)
      X1(1)= AXIS*COSL
      X1(2)= AXIS*SINL
      X1(3)= HEIGHT
      write(6,*)' dbg x1 2 3 ',x1(1),x1(2),x1(3)
! PARIALS ROTATION MATRIX
      TAROT(1,1)= COSL
      TAROT(1,2)= SINL
      TAROT(1,3)= 0.D0
      TAROT(2,1)= 0.D0
      TAROT(2,2)= 0.D0
      TAROT(2,3)= 1.D0
      TAROT(3,1)=-AXIS*SINL
      TAROT(3,2)= AXIS*COSL
      TAROT(3,3)= 0.D0
      RETURN
      END
