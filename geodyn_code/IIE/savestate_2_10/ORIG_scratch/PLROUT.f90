!$PLROUT
      SUBROUTINE PLROUT(X1,X2)
!********1*********2*********3*********4*********5*********6*********7**
! PLROUT                                         PGMR - D. PAVLIS
!
! FUNCTION:  1) TO COMPUTE GEODETIC PHI, LAMBDA, RADIUS FROM
!               PLANETOCENTRIC X, Y, Z
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   X1       I    A    XYZ TARGET POSITION
!   X2       O    A    PLR TARGET POSITION
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      DIMENSION X1(3),X2(3)
      DATA ZERO/0.0D0/,ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! CALCULATE RADIUS
      X2(3)= SQRT(X1(1)**2.D0+X1(2)**2.D0+X1(3)**2.D0)
! CALCULATE LATITUDE
      SINPHI=X1(3)/X2(3)
      COSPHI=SQRT(1.D0-SINPHI**2.D0)
      PHI=ATAN2(SINPHI,COSPHI)
      X2(1)=PHI/DEGRAD
! CALCULATE LONGITUDE
      XLAMDA=ATAN2(X1(2),X1(1))
      X2(2)=XLAMDA/DEGRAD
      RETURN
      END
