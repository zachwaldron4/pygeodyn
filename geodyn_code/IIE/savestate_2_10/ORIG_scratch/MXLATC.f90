!$MXLATC
      SUBROUTINE MXLATC(NM,NNL,NOFF,XLATS,SLAT)
!********1*********2*********3*********4*********5*********6*********7**
! MXLATC           00/00/00            97xx.0    PGMR - S.LUO
!
!
! FUNCTION:  DETERMINE THE LATITUDE ZONE OF SUBSATELLITE POINTS ,
!            READ OFF NUMBER FOR READING YANGMING's MSS FILE AND
!            FIRST LATITUDE OF THE ARRAY (XLATS) FOR THIS BLOCK
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NM       I    S    NUMBER OF POINTS FOR WHICH CALCULATION REQUIRED
!   SLAT     I    A    SATELLITE GEODETIC LATITUDES
!   NNL      O    S    NUMBER OF RECORDS READ IN TO ARRAY(NNL,5764)
!   NOFF     O    S    NUMBER of READING OFF RECORDS
!   XLATS    O    S    LATITUDE FOR THE FIRST UNIT OF ARRAY WHICH WAS
!                      FILLED BY MSS FROM YANGMING'S MSS FILE
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE
!
!
      DIMENSION SLAT(NM)
!
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
! DETERMINE THE MAX AND MIN LATITUDE IN THIS BLOCK
      XLATMA=SLAT(1)
      XLATMI=SLAT(1)
      DO 15 I = 2,NM
      IF(SLAT(I).GT.XLATMA)XLATMA=SLAT(I)
      IF(SLAT(I).LT.XLATMI)XLATMI=SLAT(I)
   15 END DO
!
! DETERMINE THE LATITUDE SPACE COVERED BY MSS FILE(-82.to 80. LATITUDE)
! NORTH-WEST CORNER
!
      XLATMA=XLATMA+1.50
      XLATMI=XLATMI-1.50
      IF(XLATMA.GT.80.)XLATMA=80.0
      IF(XLATMI.LT.-82.)XLATMI=-82.0
!
      MXLAT=NINT(XLATMA)
      MNLAT=NINT(XLATMI)
      NNL=(MXLAT-MNLAT)*16+1
      XLATS=DBLE(MNLAT)
!     XLATS=FLOAT(MNLAT)-30.0
! DETERMINE READ OFF NUMBER OF READING MSS FILE
      NOFF=(80-MXLAT)*16
      RETURN
      END
