      FUNCTION ZENDEL_SAA ( PRES, PHI_GCN, HEI_ELL )
! ************************************************************************
! *                                                                      *
! *   Routine ZENDEL_SAA computes hydrostatic zenith path delay          *
! *   according to Saaastamoinen expression.                             *
! *                                                                      *
! *   REFERENCES:                                                        *
! *   1) Saastamoinen, J. "The Use of Artificial Satellites for          *
! *         Geodesy", Geophys. Monograph Series, vol. 15, ed. by         *
! *         S.W.Henriksen et al, AGU, Washington, D.C., pp 247-251, 1972.*
! *   2) Davis, J.L., et al., "Geodesy by radio interferometry:          *
! *         Effects of atmospheric modeling errors on estimates of       *
! *         baseline length", Radio Science, 20, 1593-1607, 1985.        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    PRES ( REAL*8    ) -- surface pressure (Pa).                      *
! * PHI_GCN ( REAL*8    ) -- geocentric latitude, (rad).                 *
! * HEI_ELL ( REAL*8    ) -- height above the reference ellipsoid (m).   *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <ZENDEL_SAA> ( REAL*8    ) -- hydrostatic zenith path delay (sec).   *
! *                                                                      *
! *  ### 27-AUG-2002   ZENDEL_SAA  v1.1 (c)  L. Petrov  29-JAN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      DOUBLE PRECISION ZENDEL_SAA, PRES, PHI_GCN, HEI_ELL
      DOUBLE PRECISION REA, FE, EXC_SQ, ACC_EQU, GRV_LAT, GRV_H, R, KD, MD, C
      PARAMETER  ( REA     = 6378136.3D0 )       ! Earth's equatorial rad
      PARAMETER  ( FE      = 1.D0/298.257D0 )    ! Earth's flattening
      PARAMETER  ( EXC_SQ  = 2.D0*FE - FE**2 )   ! Earth's eccentricity
      PARAMETER  ( ACC_EQU = 9.7803184558D0 )    ! Equatorial gravity acc
      PARAMETER  ( GRV_H   = -2.D0*ACC_EQU/REA ) ! D(ACC_EQU)/DH
      PARAMETER  ( GRV_LAT = 0.001931663  )      ! D(ACC_EQU)/D(phi)
      PARAMETER  ( KD = 7.7604D-7     ) ! Refractivity of dry air
      PARAMETER  (  R = 8.31434D3     ) ! Universal gas constant J/K*mol
      PARAMETER  ( MD = 28.9644       ) ! Molar mass of dry air
      PARAMETER  (  C = 299792458.0D0 ) ! Speed of light
      DOUBLE PRECISION G_ACC
!
! --- Compute local gravity acceleration of the air column centroid at
! --- height 0.9*HEI_ELL + 7300.0
!
      G_ACC=ACC_EQU* (1.D0 + GRV_LAT* SIN(PHI_GCN)**2)/ &
     &        SQRT  (1.D0 - EXC_SQ*  SIN(PHI_GCN)**2) + &
     &        GRV_H*(0.9*HEI_ELL + 7300.0D0)
!
      ZENDEL_SAA = PRES * KD*R/(C*MD*G_ACC)
!
      RETURN
      END
