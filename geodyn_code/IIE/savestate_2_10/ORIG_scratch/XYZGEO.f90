!$XYZGEO
      SUBROUTINE XYZGEO(DLAT,DLON,DHT,NM,DN,DX,DY,DZ)
!********1*********2*********3*********4*********5*********6*********7**
! XYZGEO           00/03/22            9912.0    PGMR - SCOTT B. LUTHCKE
!
! FUNCTION:  COMPUTE ECF CARTESIAN FROM GEODETIC COORDINATES
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   DLAT     I    A    GEODETIC LATITUDE
!   DLON     I    A    LONGITUDE
!   DHT      I    A    ELLIPSOID HT.
!   NM       I    S    YOU BETTER AT LEAST KNOW WHAT THIS IS
!   DN       -    A    SCRATCH SPACE USED TO COMPUTE N
!   DX       O    A    X Planet CF CARTESIAN COORDINATE
!   DY       O    A    Y Planet CF CARTESIAN COORDINATE
!   DZ       O    A    Z Planet CF CARTESIAN COORDINATE
!
! COMMENTS:
!
!           ANGLES ARE EXPECTED TO BE IN RADIANS
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

      PARAMETER( ONE = 1.0D0)

      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CCBODY/ESQ,ESQP1,OMESQ

      DIMENSION DLAT(NM),DLON(NM),DHT(NM)
      DIMENSION DN(NM)
      DIMENSION DX(NM),DY(NM),DZ(NM)

!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      DO N=1,NM
       DN(N)=AE/SQRT(ONE-ESQ*SIN(DLAT(N))**2)
       DX(N)=(DN(N)+DHT(N))*COS(DLAT(N))*COS(DLON(N))
       DY(N)=(DN(N)+DHT(N))*COS(DLAT(N))*SIN(DLON(N))
       DZ(N)=(DN(N)+DHT(N)-ESQ*DN(N))*SIN(DLAT(N))
      ENDDO

      RETURN
      END
