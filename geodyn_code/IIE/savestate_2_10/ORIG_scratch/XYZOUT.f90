!$XYZOUT
      SUBROUTINE XYZOUT(INSYS,CIN,XYZ,AE,FLTINV)
!********1*********2*********3*********4*********5*********6*********7**
! XYZOUT           03/05/84            0000.0    PGMR - GEODYNI/SEVITSKI
!                  06/20/84                      PGMR - EDDY
!
! FUNCTION:      CONVERT STATION COORDINATES FROM GEODETIC,CYLINDRICAL
!                OR SPHERICAL TO CARTESIAN
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   INSYS    I         SYSTEM OF INPUT FOR STATION COORDINATES
!                             1 - GEODETIC
!                             2 - CARTESIAN
!                             3 - CYLINDRICAL
!                             4 - SPHERICAL
!   CIN      I         INPUT COORDINATES OF THE STATION
!   XYZ      O         COORDINATES OF STATION IN CARTESIAN
!   AE       I         AE TO BE USED FOR THIS STATION CONVERSION
!   FLTINV   I         INVERSE FLATTENING TO BE USED FOR CONVERSION
!
! UPDATE HISTORY
! 06/20/84 - EDDY       MODIFIED ROUTINE TO WORK FOR ONE STATION AT A
!                       TIME AND ADDED DOCUMENTATION
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION CIN(3),XYZ(3)
!
      DATA ZERO/0.0D0/
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      GOTO (100,200,300,100),INSYS
!********************************************************************
! PHI,LAMBDA,HEIGHT OR RADIUS TO CARTESIAN
!********************************************************************
  100 CONTINUE
      RLAT  =CIN(1)
      RLON  =CIN(2)
      H     =CIN(3)
      FLAT  =1.0D0/FLTINV
      ESQ1  =(1.0D0-FLAT)**2
      ESQ   =1.0D0-ESQ1
      SLATG =SIN(RLAT)
      SLTGSQ=SLATG*SLATG
      CLATG =SQRT(1.0D0-SLTGSQ)
      R     =AE/SQRT(1.0D0-ESQ*SLTGSQ)
      IF(INSYS.EQ.4) R=ZERO
      RCL   =CLATG*(H+R)
      XYZ(1)=RCL*COS(RLON)
      XYZ(2)=RCL*SIN(RLON)
      XYZ(3)=SLATG*(H+ESQ1*R)
      RETURN
!********************************************************************
! XYZ
!********************************************************************
  200 CONTINUE
      XYZ(1)=CIN(1)
      XYZ(2)=CIN(2)
      XYZ(3)=CIN(3)
      RETURN
!********************************************************************
! S.A.D.,LAMBDA,Z  TO CARTESIAN
!********************************************************************
  300 CONTINUE
      XYZ(1)=CIN(1)*COS(CIN(2))
      XYZ(2)=CIN(1)*SIN(CIN(2))
      XYZ(3)=CIN(3)
      RETURN
      END
