!$FCULZD_HPA
      subroutine fculzd_hPa(latitude, ellip_ht, pressure, wvp,          &
     &                  lambda_um, fcul_ztd, fcul_zhd, fcul_zwd)

!********1*********2*********3*********4*********5*********6*********7**
! FCULZD_HPA              04/06/29            0406.29   PGMR - V. MENDES
!
! FUNCTION:
! This function calculates the Mendes-Pavlis zenith total delay,
! for optical wavelengths, valid for infrared to ultraviolet.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!  latitude  I          - latitude (degrees)
!  ellip_ht  I          - height (metres)
!  pressure  I          - surface pressure (hPa) (mbars)
!  wvp       I          - water vapor pressure (hPa) (mbars)
!  lambda_um I          - wavelength (micrometers)
!  fcul_ztd  O          - zenith total delay (m)
!  fcul_zhd  O          - zenith hydrostatic (dry) delay (m)
!  fcul_zwd  O          - zenith wet delay (m)
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**

!     Creation: 29June2004, V.B. Mendes, for the RSG of the ILRS

!      This subroutine computes the zenith total delay, for optical
!      wavelengths
!      For details see: Mendes, V. B. and E. C. Pavlis (2004),
!                       High-accuracy zenith delay prediction at
!                       optical wavelengths. Geophysical Research
!                       Letters, Vol. 31, doi:10.1029/2004GL020308.
!
!      Input parameters (note that pressures are given in hectoPascal un
!          latitude  - geodetic latitude (degree)
!          ellip_ht  - height above ellipsoid (meter)
!          pressure  - surface pressure (hPa, i.e. mbars)
!          wvp       - water vapor pressure (hPa, i.e. mbars)
!          lambda_um - laser wavelength (micrometer)
!      Local parameters:
!          sigma     - wave number (1/lambda_um)
!          xc        - CO2 content, in ppm
!          c         - speed of light in vacuum (m/s)
!          pi        - pi (radians)
!      Output parameters:
!          fcul_ztd  - Zenith total delay (m)
!          fcul_zhd  - zenith hydrostatic delay (m)
!          fcul_zwd  - zenith non-hydrostatic delay (m)
!
      implicit none
      save

      DOUBLE PRECISION c, pi, ellip_ht, latitude, f, pressure, wvp
      DOUBLE PRECISION k0, k1, k2, k3
      DOUBLE PRECISION xc, corr
      DOUBLE PRECISION sigma, w0, w1, w2, w3, fh, fnh
      DOUBLE PRECISION lambda_um
      DOUBLE PRECISION fcul_zhd, fcul_zwd, fcul_ztd

                                    ! speed of light, m/s
      parameter (c = 2.99792458d8)
      parameter (pi = 3.141592654)
!
      xc = 375.0D0
!         constant values to be used in Equation (20)
!         k1 and k3 are k1* and k3*
      k0 = 238.0185D0
      k1 = 19990.975D0
      k2 = 57.362D0
      k3 = 579.55174D0

!         constant values to be used in Equation (32)
      w0 = 295.235D0
      w1 = 2.6422D0
      w2 = -0.032380D0
      w3 = 0.004028D0
!
      sigma = 1/lambda_um

!     correction factor - Equation (24)
      f = 1 - 0.00266*COS(2*pi/180*latitude) - 0.00028D-3*ellip_ht

!     correction for CO2 content
      corr = 1.0D0 + 0.534D-6*(xc-450)

!     dispersion equation for the hydrostatic component - Equation (20)
      fh = 0.01D0*corr*((k1*(k0+sigma**2))/((k0-sigma**2)**2) +         &
     &     k3*(k2+sigma**2)/((k2-sigma**2)**2))

!     computation of the hydrostatic component - Equation (26)
!     caution: pressure in hectoPascal units
      fcul_zhd = 2.416579D-3*fh*pressure/f

!     dispersion equation for the non-hydrostatic component - Equation (
      fnh = 0.003101D0*(w0+3.0*w1*sigma**2 +                            &
     &      5.0*w2*sigma**4+7.0*w3*sigma**6)

!     computation of the non-hydrostatic component - Equation (38)
!     caution: pressure in hectoPascal units
      fcul_zwd = 1.D-4*(5.316D0*fnh-3.759*fh)*wvp/f

!      compute the zenith total delay
      fcul_ztd = fcul_zhd + fcul_zwd

      return
      END
