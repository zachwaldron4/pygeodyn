      recursive subroutine fu_nodal( omega, p, constituent, n, f, u )
!
!  Computes tidal nodal (& perigee) corrections "f" and "u"
!  for n tidal constituents,
!  given the mean longitudes p and omega.
!
!  Language: Fortran 90.
!
!-------------------------------------------------------
!  Input:
!     omega - mean longitude of lunar node, in degrees.
!     p     - mean longitude of lunar perigee.
!     consituent - array of constituent names (char*4)
!     n  - length of arrays {f,u,constituent}.
!  Output:
!     f  - modulation factor for given tide(s).
!     u  - phase correction for given tide, in degrees.
!-------------------------------------------------------
!
!  Note: omega = -N' (where N' is the 5th Doodson variable);
!       omega is decreasing in time whereas N' is increasing.
!
!  This is not a very efficient routine, but it needn't be
!  called very often, so is ok.
!
!  R Ray  21 August 2003
!  Revised 9/15/08.
!  Revised 6/30/09 - stuck in kludgy code for special M2 modulation.
!  Revised 3/28/11 - fixed Mt, MSt.
!  Revised - from time to time, another constituent is added.
!
!
      implicit none
      INTEGER,          intent(in)  ::  n
      double precision, intent(in)  ::  omega, p
      character*4,      intent(in)  ::  constituent(*)
      double precision, intent(out) ::  f(*), u(*)
!
      double precision s,h
      double precision term1,term2,ftmp(4),utmp(4)
      character*4      ctmp(4)
      INTEGER          i
      double precision         cosn,cos2n,sinn,sin2n,sin3n
      double precision         sinp,cosp,sin2p,cos2p
      double precision two,three
      parameter       (two=2.d0,three=3.d0)

      double precision pi, rad
      parameter       (pi=3.141592654d0, rad=pi/180.D0)
      double precision special_f                        ! override f for
      LOGICAL          override_f
      data override_f/.false./
!
!     Various trig functions of astronomical longitudes
!     -------------------------------------------------
      sinn = SIN(omega*rad)
      cosn = COS(omega*rad)
      sin2n = SIN(two*omega*rad)
      cos2n = COS(two*omega*rad)
      sin3n = SIN(three*omega*rad)
      sinp  = SIN(p*rad)
      cosp  = COS(p*rad)
      sin2p = SIN(two*p*rad)
      cos2p = COS(two*p*rad)
!
!     Compute standard nodal corrections f and u
!     ------------------------------------------
      do i=1,n

         select case ( constituent(i) )

            case ( 'Mm  ','MSm ' )
               term1 = -.0534*sin2p - .0219*SIN((two*p-omega)*rad)
               term2 = 1.0 - .1308*cosn - .0534*cos2p                   &
     &                 - .0219*COS((two*p-omega)*rad)

            case ( 'Mf  ','MSq ','MSp ','Mq  ' )
               term1 = -0.04324*sin2p - 0.41465*sinn - 0.03873*sin2n
               term2 = 1.0 +0.04324*cos2p +0.41465*cosn +0.03873*cos2n

            case ( 'MSf ' )     !  This is linear tide, not compound.
               term1 = 0.137*sinn
               term2 = 1.0

            case ( 'Mt  ' )
               term1 = -0.018*sin2p - 0.4145*sinn - 0.040*sin2n
               term2 = 1.0 +0.018*cos2p +0.4145*cosn +0.040*cos2n

            case ( 'MSt ' )
               term1 = -0.380*sin2p - 0.413*sinn - 0.037*sin2n
               term2 = 1.0 +0.380*cos2p +0.413*cosn +0.037*cos2n

            case ( 'O1  ','SO3 ' )
               term1 = 0.1886*sinn - 0.0058*sin2n - 0.0065*sin2p
               term2 = 1.0 + 0.1886*cosn - 0.0058*cos2n - 0.0065*cos2p

            case ( '2Q1 ','Q1  ','rho1','sig1' )
               term1 = 0.1886*sinn
               term2 = 1.0 + 0.1886*cosn
            case ( 'tau1' )
               term1 = 0.219*sinn
               term2 = 1.0 - 0.219*cosn
            case ( 'bet1' )
               term1 = 0.226*sinn
               term2 = 1.0 + 0.226*cosn

            case ( 'M1  ' )        ! This assumes M1 argument includes p.
               term1 = -0.2294*sinn - 0.3594*sin2p                      &
     &                   - 0.0664*SIN((two*p-omega)*rad)
               term2 = 1.0 + 0.1722*cosn + 0.3594*cos2p                 &
     &                   + 0.0664*COS((two*p-omega)*rad)
            case ( 'chi1' )
               term1 = -0.222*sinn
               term2 = 1.0 + 0.222*cosn
            case ( 'P1  ' )
               term1 = -0.0112*sinn
               term2 = 1.0 - 0.0112*cosn
            case ( 'K1  ','SK3 ' )
               term1 = -0.1554*sinn + 0.0031*sin2n
               term2 = 1.0 + 0.1158*cosn - 0.0028*cos2n
            case ( 'J1  ','the1' )
               term1 = -0.227*sinn
               term2 = 1.0 + 0.169*cosn
            case ( 'OO1 ','ups1' )
               term1 = -0.640*sinn - 0.134*sin2n - 0.150*sin2p
               term2 = 1.0 + 0.640*cosn + 0.134*cos2n + 0.150*cos2p

            case ( 'M2  ','2N2 ','mu2 ','N2  ','nu2 ','lam2',           &
     &             'MS4 ','eps2','2SM6','2SN6' )
               term1 = -0.03731*sinn + 0.00052*sin2n
               term2 = 1.0 - 0.03731*cosn + 0.00052*cos2n

            case ( 'L2  ' )
               term1 = -0.250*sin2p - 0.110*SIN((two*p-omega)*rad)      &
     &                   - 0.037*sinn
               term2 = 1.0 - 0.250*cos2p - 0.110*COS((two*p-omega)*rad) &
     &                   - 0.037*cosn
            case ( 'K2  ','SK4 ' )
               term1 = -0.3108*sinn - 0.0324*sin2n
               term2 = 1.0 + 0.2853*cosn + 0.0324*cos2n

            case ( 'eta2','zet2','xi2 ' )
               term1 = -0.436*sinn
               term2 = 1.0 + 0.436*cosn

            case ( 'M3  ','O3  ','F3  ' )   ! Linear 3rd-deg terms
               term1 = -0.056*sinn
               term2 = 1.0 - 0.056*cosn
            case ( 'J3  ' )
               term1 = -0.43*sinn
               term2 = 1.0 + 0.43*cosn

            case default
               term1 = 0.
               term2 = 1.

         end select

         f(i) = SQRT(term1**2 + term2**2)
         u(i) = ATAN( term1/term2 )/rad

!  special M2 override -- messes with only f, not u...
         if (override_f .and. constituent(i)=='M2  ') then
            f(i) = SQRT( (1.0-special_f*cosn)**2 + (special_f*sinn)**2 )
         endif


!        Following tides are all compound &  use recursion
!        -------------------------------------------------

         select case ( constituent(i) )

            case ( '2K2 ' )
               ctmp(1)='K1  '
               call fu_nodal( omega, p, ctmp, 1, ftmp, utmp )
               f(i) = ftmp(1)**2
               u(i) = 2.0*utmp(1)

            case ( 'KO2 ' )
               ctmp(1:2) = (/'K1  ','O1  '/)
               call fu_nodal( omega, p, ctmp, 2, ftmp, utmp )
               f(i) = ftmp(1)*ftmp(2)
               u(i) = utmp(1) + utmp(2)

            case ( 'M4  ','MN4 ','MNS2','2MS2','N4  ','Mnu4','2MS6',    &
     &             '2MS8' )
               ctmp(1)='M2  '
               call fu_nodal( omega, p, ctmp, 1, ftmp, utmp )
               f(i) = ftmp(1)**2
               u(i) = 2.0*utmp(1)

            case ( 'M6  ','2MN6','3MS8' )
               ctmp(1)='M2  '
               call fu_nodal( omega, p, ctmp, 1, ftmp, utmp )
               f(i) = ftmp(1)**3
               u(i) = 3.0*utmp(1)

            case ( 'MK3 ','NK3 ','MSK5' )
               ctmp(1:2) = (/'M2  ','K1  '/)
               call fu_nodal( omega, p, ctmp, 2, ftmp, utmp )
               f(i) = ftmp(1)*ftmp(2)
               u(i) = utmp(1) + utmp(2)
            case ( 'MO3 ','NO3 ','MSO5' )
               ctmp(1:2) = (/'M2  ','O1  '/)
               call fu_nodal( omega, p, ctmp, 2, ftmp, utmp )
               f(i) = ftmp(1)*ftmp(2)
               u(i) = utmp(1) + utmp(2)

            case ( '2MO5','2NO5','MNO5' )
               ctmp(1:2) = (/'M2  ','O1  '/)
               call fu_nodal( omega, p, ctmp, 2, ftmp, utmp )
               f(i) = ftmp(1)**2*ftmp(2)
               u(i) = 2.*utmp(1) + utmp(2)

            case ( 'MK4 ','MKS2','MSK6' )
               ctmp(1:2) = (/'M2  ','K2  '/)
               call fu_nodal( omega, p, ctmp, 2, ftmp, utmp )
               f(i) = ftmp(1)*ftmp(2)
               u(i) = utmp(1) + utmp(2)

            case ( 'MSK2' )
               ctmp(1:3) = (/'M2  ','S2  ','K2  '/)
               call fu_nodal( omega, p, ctmp, 3, ftmp, utmp )
               f(i) = ftmp(1)*ftmp(2)*ftmp(3)
               u(i) = utmp(1) + utmp(2) - utmp(3)

            case ( 'MNK6' )
               ctmp(1:3) = (/'M2  ','N2  ','K2  '/)
               call fu_nodal( omega, p, ctmp, 3, ftmp, utmp )
               f(i) = ftmp(1)*ftmp(2)*ftmp(3)
               u(i) = utmp(1) + utmp(2) + utmp(3)

            case ( 'M8  ','3MN8','2MN8','4MSA' )          !   hex A = 10
               ctmp(1)='M2  '
               call fu_nodal( omega, p, ctmp, 1, ftmp, utmp )
               f(i) = ftmp(1)**4
               u(i) = 4.0*utmp(1)

            case ( 'M10 ','4MNA' )
               ctmp(1)='M2  '
               call fu_nodal( omega, p, ctmp, 1, ftmp, utmp )
               f(i) = ftmp(1)**5
               u(i) = 5.0*utmp(1)
            case ( 'M12 ' )
               ctmp(1)='M2  '
               call fu_nodal( omega, p, ctmp, 1, ftmp, utmp )
               f(i) = ftmp(1)**6
               u(i) = 6.0*utmp(1)

            case ( 'MNK8' )          ! This is really 2MNK8
               ctmp(1:2) = (/'M2  ','K2  '/)
               call fu_nodal( omega, p, ctmp, 2, ftmp, utmp )
               f(i) = ftmp(1)**3 * ftmp(2)
               u(i) = 3.*utmp(1) + utmp(2)

            case ( 'MfDW' )   !  special test of old Doodson-Warburg form
               f(i) = 1.043 + 0.414*cosn
               u(i) = -23.7*sinn + 2.7*sin2n - 0.4*sin3n

         end select

      end do

      return
      end
