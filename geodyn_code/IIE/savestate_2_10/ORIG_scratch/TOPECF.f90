!$TOPECF
      SUBROUTINE TOPECF(XLAT,XLON,TCTOCF)
!********1*********2*********3*********4*********5*********6*********7**
! TOPECF           08.09.91            9107.0    LUCIA TSAOUSSI
!
!
! FUNCTION:  COMPUTE THE ROTATION MATRIX FROM THE
!            CONVENTIONAL TOPOCENTRIC SYSTEM
!            TO THE CRUST FIXED GEOCENTRIC SYSTEM
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XLAT     I    S    THE SITE LATITUDE (RAD)
!   XLON     I    S    THE SITE LONGITUDE (RAD)
!   TCTOCF   O    A    THE ROTATION MATRIX FROM TOPOC. TO CRUST FIXED
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      PARAMETER ( ZERO=0.D0,ONE=1.D0 )
      DIMENSION TCTOCF(3,3),RY(3,3),RZ(3,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!     WRITE(6,*) ' TOPECF : LAT,LON   ',XLAT,XLON
      DO 100 I=1,3
      DO 100 J=1,3
      RY(I,J)=ZERO
      RZ(I,J)=ZERO
  100 CONTINUE
      CLAT=COS(XLAT)
      SLAT=SIN(XLAT)
      CLON=COS(XLON)
      SLON=-SIN(XLON)
!  ***  COMPUTE ROTATION RZ = R3(-LAMDA)  ***
      RZ(1,1)=CLON
      RZ(1,2)=SLON
      RZ(2,1)=-SLON
      RZ(2,2)=CLON
      RZ(3,3)=ONE
!
!  ***  COMPUTE ROTATION RY = R2(PHI)  ***
!
      RY(1,1)=CLAT
      RY(1,3)=-SLAT
      RY(2,2)=ONE
      RY(3,1)=SLAT
      RY(3,3)=CLAT
!
!  ***  COMPUTE THE ROTATION RZ * RY = TCTOCF
!
      ND=3
      CALL MATPRD(RZ,RY,TCTOCF,ND,ND,ND)
!     WRITE(6,*) ' TOPECF : TCTOCF MATRIX    '
!     DO 300 I=1,3
!     WRITE(6,*) (TCTOCF(I,J),J=1,3)
!300  CONTINUE
      RETURN
      END
