!$NADIR
      SUBROUTINE NADIR(XSAT,ZNADIR)
!*******************************************************************
!  ROUTINE NAME:   NADIR   DATE: 01/22/97      PGMR: A. MARSHALL
!
!   FUNCTION - TO ROTATE THE SPACECRAFT POSITION VECTOR FROM
!              GEOCENTRIC TO GEODETIC POINTING
!
!  I/O PARAMETERS:
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   XSAT     A      I    S/C GEOCENTRIC POSITION VECTOR
!   ZNADIR   A      O    GEODETIC POINTING VECTOR
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CEARTH/AEG,AEGSQ,FGINV,FG,EGSQ,EGSQP1,C1,C2,FOURC1,TWOC2,  &
     &              XH2,XL2,WREF,RMEAN,REFC20,REFC40,REFC60,BEG,CBLTC,  &
     &              WAE2,GAMMAA,FSTAR,F4
!
      DIMENSION XSAT(3),ZNADIR(3),ZG(3),AXIS(3)
!
      DATA ZERO/0.0D0/,ONE/1.0D0/
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
      ZMAG=SQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
      ZG(1)=-XSAT(1)/ZMAG
      ZG(2)=-XSAT(2)/ZMAG
      ZG(3)=-XSAT(3)/ZMAG
!
! COMPUTE GEODETIC LATITUDE (ALA SUBTRK)
      XY2=XSAT(1)**2+XSAT(2)**2
      CALL ELIPSE(XSAT(3),XY2,DUM,DUM1,HT,ZT,RT,1)
      XY2=SQRT(XY2)
      DLAT=(ZT/XY2)
      DLAT=ATAN(DLAT)
      DS=SIN(DLAT)
      DC=COS(DLAT)
      W=SQRT(1.D0-EGSQ*DS*DS)
      XN=AEG/W
      DLON=ATAN2(XSAT(2),XSAT(1))
!
!  COMPUTE COORDINATRS OF SUB SAT POINT
      XSB=XN*DC*COS(DLON)
      YSB=XN*DC*SIN(DLON)
      ZSB=XN*(1.D0-EGSQ)*DS
!
      Z1=XSB-XSAT(1)
      Z2=YSB-XSAT(2)
      Z3=ZSB-XSAT(3)
      ZR=SQRT(Z1*Z1+Z2*Z2+Z3*Z3)
      ZNADIR(1)=Z1/ZR
      ZNADIR(2)=Z2/ZR
      ZNADIR(3)=Z3/ZR
! ROTATE THE S/C POINTING VECTOR FROM GEOCENTRIC TO GEODETIC
      RETURN
      END
