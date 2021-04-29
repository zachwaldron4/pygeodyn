!$SP2000
      SUBROUTINE SP2000( MJDSBL,FSECN,SP2)
!********1*********2*********3*********4*********5*********6*********7**
! SP2000           04/10/26                                  D.Pavlis
!
! FUNCTION:   THIS SUBROUTINE COMPUTES THE QUANTITY s'
!   Start with the equation of transformation between the Terrestrial
!
!   System:
!
!              [CRS] = Q(t)R(t)W(t) [TRS]
!
!   where Q is the combined Preccesion-Nutation matrix
!         R is the earth rotation  matrix and,
!         W is the polar motion matrix
!
!      The computation of the polar motion matrix W(t) requires the pola
!      coordinates xp, yp and a quantity s' used as follows:
!
!                        W(t)= R3(-s')R1(yp)R2(xp)
!
!      The quantity s' which was neglected prior to 1 January 2003 is th
!      accumulated displacement of the TEO (terr. earth. origin) on the
!      True Equator due to PM (Polar Motion) . Now it necessary to provi
!      the exact realization of the instantaneous prime meridian.
!
!      The mathematical expression for s' contains Chandlerian and annua
!      wobbles and it's value is less than 0.4mas after a century. Using
!      the current mean amplitudes for both wobbles, gives s'=-47microar
!      (t in Julian Centuries after J2000)
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSBL   I    S    MJD SECONDS OF CURRENT TIME
!   FSECN    I    S    FRACTIONAL SECONDS OF CURRENT TIME
!   SP       O    S    S PRIME VARIAVLE AT THIS TIME
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!  Arcseconds to radians
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

!  Reference epoch (J2000), JD
      PARAMETER ( DJ0 = 2451545D0 )

!  Days per Julian century
      PARAMETER ( DJC = 36525D0 )

      DATA IZERO/0/
      DATA CENTUR/36525.0D0/
      DATA D2000/51544.5D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
! COMPUTE TIMES
! The following calculations will produce DATE1 and DT
! DATE1 is the time at the beginning of the day expressed in Julian
! centuries. DT is the fraction of the day also expressed in Julian
! centuries.
!
      CALL MJDYMD(MJDSBL,IYMDAY,IHMSEC,4)
      CALL MJDYMD(MJDS,IYMDAY, IZERO, 2)
      DATE1=(DBLE(MJDS)-D2000)/CENTUR
      ITMP1=(IHMSEC/10000)+0.5
      ITMP2=MOD(IHMSEC,10000)
      ITMP3=(ITMP2/100)+0.5
      ITMP4=MOD(ITMP2,100)
      SECS=DBLE(ITMP4+ITMP3*60+ITMP1*3600)
!
      DT=(SECS+FSECN)/86400.D0
      DT=DT/CENTUR

!  Interval between fundamental epoch J2000.0 and current date (JC).
      T = ( DATE1+ DT )

!  Approximate S'.
!  Also convert SP2 from microarcsec to radians.
      SP2 = -47.D-6 * T * DAS2R

      RETURN
      END
