!$PLRXYZ
      SUBROUTINE PLRXYZ(X1)
!********1*********2*********3*********4*********5*********6*********7**
! PLRXYZ           95/11/20            9510.0    PGMR- DESPINA PAVLIS
!
! FUNCTION:  CONVERT GEODETIC PLANETARY COORDINATES TO PLANET CENTERED
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
      COSB=COS(X1(1))
      COSL=COS(X1(2))
      SINB=SIN(X1(1))
      SINL=SIN(X1(2))
      RADLAN=X1(3)
      X1(1)= X1(3)*COSB*COSL
      X1(2)= X1(3)*COSB*SINL
      X1(3)= X1(3)*SINB
      TAROT(1,1)=-SINB*COSL*RADLAN
      TAROT(1,2)=-SINB*SINL*RADLAN
      TAROT(1,3)= COSB*     RADLAN
      TAROT(2,1)=-COSB*SINL*RADLAN
      TAROT(2,2)= COSB*COSL*RADLAN
      TAROT(2,3)= 0.D0
      TAROT(3,1)= COSB*COSL
      TAROT(3,2)= COSB*SINL
      TAROT(3,3)= SINB
      RETURN
      END
