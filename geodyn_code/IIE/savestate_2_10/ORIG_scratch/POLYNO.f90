!$POLYNO
      SUBROUTINE POLYNO(COSLAT,SINLAT)
!********1*********2*********3*********4*********5*********6*********7**
! POLYNO           00/00/00            8803.0    PGMR - WEI XIA
!
! FUNCTION:  CALCULATE THE LEGENDRE POLYNOMIAL
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   COSLAT   I    S    COSINE OF LATITUDE
!   SINLAT   I    S    SINE OF LATITUDE
!
! COMMENTS:  OUTPUT GOES INTO COMMON/DTMPOL/
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/DTMPOL/P10,P20,P30,P40,P50,P11,P21,P31,P51,P22,P32,P33
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      R=SINLAT
      R2=R**2
      R4=R2**2
      RR=COSLAT
      P10=R
      P20=0.5D0*(3.0D0*R2-1.0D0)
      P30=0.5D0*R*(5.0D0*R2-3.0D0)
      P40=0.125D0*(35.0D0*R4-30.0D0*R2+3.0D0)
      P50=0.125D0*R*(63.0D0*R4-70.0D0*R2+15.0D0)
      P11=RR
      P21=RR*3.0D0*R
      P31=RR*1.5D0*(5.0D0*R2-1.0D0)
      P51=(RR*15.0D0*(21.0D0*R4-14.0D0*R2+1.0D0))/8.0D0
      P22=3.0D0*(RR**2)
      P32=15.0D0*R*(RR**2)
      P33=15.0D0*(RR**3)
      RETURN
      END
