!$SUBTRK
      SUBROUTINE SUBTK2(XYZ,XY2,ZT,RT,SATLAT,SATLON,SATH,NM)
!********1*********2*********3*********4*********5*********6*********7**
! SUBTRK           83/05/13            8305.0    PGMR - TOM MARTIN
!
! FUNCTION:  COMPUTE THE S/C HEIGHT ABOVE THE EARTH ELLIPSOID
!            AND THE SUBSATELLITE LATITUDE & EAST LONGITUDE
!            FOR A VECTOR OF S/C POSITIONS THAT HAVE BEEN GIVEN
!            IN EARTH FIXED CAETESIAN COORDINATES
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XYZ      I         XYZ S/C ECF POSITIONS
!   XY2      O         SUM OF SQUARES OF S/C X AND Y POSITIONS
!   ZT       O         Z COMPONENT OF S/C POSITIONS W.R.T. SURFACE
!                      CENTER OF CURVATURE
!   RT       O         DISTANCE FROM S/C TO SURFACE CENTER OF CURV.
!   SATLAT   O         SUBSATELLITE GEODETIC LATITUDES
!   SATLON   O         SUBSATELLITE EAST LONGITUDES
!   SATH     O         S/C HEIGHTS ABOVE EARTH ELLIPSOID
!   NM       I         NUMBER OF MEASUREMENTS
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      DIMENSION XYZ(NM,3),XY2(NM),ZT(NM),SATLAT(NM),     &
     &   SATLON(NM),SATH(NM),RT(NM)
      DATA C360/360.0D0/,C720/720.0D0/,ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      DO  800 N=1,NM
      XY2(N)=       XYZ(N,1)*XYZ(N,1)
  800 END DO
      DO 1800 N=1,NM
      XY2(N)=XY2(N)+XYZ(N,2)*XYZ(N,2)
 1800 END DO
      CALL ELIPSE(XYZ(1,3),XY2,SATLAT,SATLON,SATH,ZT,RT,NM)
      DO 2800 N=1,NM
      SATLON(N)=ATAN2(XYZ(N,2),XYZ(N,1))
 2800 END DO
          RADDEG=ONE/DEGRAD
      DO 3800 N=1,NM
      SATLON(N)=SATLON(N)*RADDEG
 3800 END DO
      DO 4800 N=1,NM
      SATLON(N)=SATLON(N)+C720
 4800 END DO
      DO 5800 N=1,NM
      SATLON(N)=MOD(SATLON(N),C360)
 5800 END DO
      DO 6800 N=1,NM
      XY2(N)=SQRT(XY2(N))
 6800 END DO
      DO 7800 N=1,NM
      SATLAT(N)=ZT(N)/XY2(N)
 7800 END DO
      DO 8800 N=1,NM
      SATLAT(N)=ATAN(SATLAT(N))*RADDEG
 8800 END DO
      RETURN
      END
