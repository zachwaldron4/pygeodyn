      subroutine fu_group( perihelion, omega, perigee, hsolar,          &
     &                     constituent, n, f, u )
!
!  Computes tidal corrections "f" and "u" for n tidal constituents,
!  given the mean longitudes h, p, and omega.
!
!  The f,u corrections are a generalization of the standard nodal
!  corrections used in tidal analyses.  Rather than accounting for
!  sidelines within a constituent (as the standard method does),
!  this routine accounts for sidelines within a tidal group.
!  Only certain tidal groups can be so handled; if this routine is
!  called for a group not handled, it defaults to routine fu_nodal.
!
!  Language: Fortran 90.
!
!-------------------------------------------------------
!  Input:
!     perhelon- mean longitude of sun's perihelion.
!               (For times around J2000, this is about 282 degrees.)
!     omega   - mean longitude of lunar node, in degrees.
!     perigee - mean longitude of lunar perigee.
!     hsolar  - mean longitude of sun.
!     consituent - array of constituent names (char*4)
!          This name should be the main line within the group.
!     n  - length of arrays {f,u,constituent}.
!  Output:
!     f  - modulation factor for given tide.
!     u  - phase correction for given tide, in degrees.
!-------------------------------------------------------
!
!  R Ray  21 May 2004
!
!
      implicit none
      INTEGER         , intent(in) :: n
      double precision, intent(in) :: omega, perigee, hsolar, perihelion
      double precision, intent(out):: f(*), u(*)
      character*4     , intent(in) :: constituent(*)
!
      DOUBLE PRECISION             h,p,pp,o
      double precision term1,term2
      INTEGER          i
      double precision pi, rad
      parameter       (pi=3.141592654d0, rad=pi/180.D0)


      h = hsolar*rad
      p = perigee*rad
      o = -omega*rad
!CC   pp = 4.94
      pp = perihelion*rad
!
!
!     Compute corrections f and u
!     ---------------------------
      do i=1,n

         select case ( constituent(i) )

            case ( 'Mm  ' )
               term1 = .0191*SIN(-2.*h + 2.*p)                          &
     &                -.0657*SIN(-o) - .0651*SIN(o)                     &
     &                -.0534*SIN(2.*p) - .0219*SIN(2.*p + o)
               term2 = 1.0 + .0191*COS(-2.*h + 2.*p)                    &
     &                - .0657*COS(-o) - .0651*COS(o)                    &
     &                - .0534*COS(2.*p) - .0219*COS(2.*p + o)

            case ( 'Mf  ' )
               term1 = .0875*SIN(-2.*h) + .0432*SIN(-2.*p)              &
     &               + .4145*SIN(o) + .0387*SIN(2.*o)
               term2 = 1.0 + .0875*COS(2.*h) + .0432*COS(2.*p)          &
     &               + .4145*COS(o) + .0387*COS(2.*o)

            case ( 'Mt  ' )
               term1 = .0721*SIN(-2.*h) + .1897*SIN(-2.*h + 2.*p)       &
     &               + .0784*SIN(-2.*h + 2.*p + o) + .4146*SIN(o)
               term2 = 1.0 + .0721*COS(2.*h) + .1897*COS(-2.*h + 2.*p)  &
     &               + .0784*COS(-2.*h + 2.*p + o) + .4146*COS(o)

            case ( 'Mq  ' )
               term1 = 1.207*SIN(-2.*h + 2.*p)                          &
     &               + .497*SIN(-2.*h + 2.*p + o) + .414*SIN(o)
               term2 = 1.0 + 1.207*COS(-2.*h + 2.*p)                    &
     &               + .497*COS(-2.*h + 2.*p + o) + .414*COS(o)

            case ( '2Q1 ' )
               term1 = .1886*SIN(-o) + .2274*SIN(2.*h - 2.*p - o)       &
     &               + 1.2078*SIN(2.*h - 2.*p)
               term2 = 1.0 + .1886*COS(o) + .2274*COS(2.*h - 2.*p - o)  &
     &               + 1.2078*COS(2.*h - 2.*p)

            case ( 'Q1  ' )
               term1 = .1886*SIN(-o) + .0359*SIN(2.*h - 2.*p - o)       &
     &               + .1900*SIN(2.*h - 2.*p)
               term2 = 1.0 + .1886*COS(o) + .0359*COS(2.*h - 2.*p - o)  &
     &               + .1900*COS(2.*h - 2.*p)

            case ( 'O1  ' )
               term1 = -.0058*SIN(-2*o) + .1886*SIN(-o)                 &
     &                - .0065*SIN(2*p) - .0131*SIN(2.*h)
               term2 = 1.0 -.0058*COS(2*o) + .1886*COS(o)               &
     &                - .0065*COS(2*p) - .0131*COS(2.*h)

                                ! Note: central line is 155.655
            case ( 'M1  ' )
               term1 = .0941*SIN(-2.*h) + .0664*SIN(-2.*p - o)          &
     &               + .3594*SIN(-2.*p) + .2008*SIN(o)                  &
     &               + .1910*SIN(2.*h - 2.*p) + .0422*SIN(2.*h-2.*p+o)
               term2 = 1.0 + .0941*COS(2.*h) + .0664*COS(2.*p + o)      &
     &               + .3594*COS(2.*p) + .2008*COS(o)                   &
     &               + .1910*COS(2.*h - 2.*p) + .0422*COS(2.*h-2.*p+o)

            case ( 'J1  ' )
               term1 = .1916*SIN(-2.*h + 2.*p)                          &
     &               + .0378*SIN(-2.*h + 2.*p + o)                      &
     &               - .0291*SIN(-o) + .1984*SIN(o)
               term2 = 1.0 + .1916*COS(-2.*h + 2.*p)                    &
     &               + .0378*COS(-2.*h + 2.*p + o)                      &
     &               - .0291*COS(-o) + .1984*COS(o)

            case ( 'OO1 ' )
               term1 = .3029*SIN(-2.*h) + .0593*SIN(-2.*h + o)          &
     &               + .1497*SIN(-2.*p)                                 &
     &               + .6404*SIN(o) + .1337*SIN(2.*o)
               term2 = 1.0 + .3029*COS(-2.*h) + .0593*COS(-2.*h + o)    &
     &               + .1497*COS(-2.*p)                                 &
     &               + .6404*COS(o) + .1337*COS(2.*o)

            case ( 'eps2' )
               term1 = .385*SIN(-2.*h + 2.*p)
               term2 = 1.0 + .385*COS(-2.*h + 2.*p)

            case ( '2N2 ' )
               term1 = -.0374*SIN(-o) + 1.2067*SIN(2.*h - 2.*p)         &
     &                - .0450*SIN(2.*h - 2.*p - o)                      &
     &                + .0812*SIN(3.*h - 2.*p - 4.939)
               term2 = 1.0 - .0374*COS(o) + 1.2067*COS(2.*h - 2.*p)     &
     &                - .0450*COS(2.*h - 2.*p - o)                      &
     &                + .0812*COS(3.*h - 2.*p - pp)

            case ( 'N2  ' )
               term1 = -.0084*SIN(-h + pp) - .0373*SIN(-o)              &
     &                + .0093*SIN(h - pp) + .1899*SIN(2.*h - 2.*p)      &
     &                - .0071*SIN(2.*h - 2.*p - o)
               term2 = 1.0 -.0084*COS(-h + pp) - .0373*COS(o)           &
     &                + .0093*COS(h - pp) + .1899*COS(2.*h - 2.*p)      &
     &                - .0071*COS(2.*h - 2.*p - o)

            case ( 'M2  ' )
               term1 = -.0030*SIN(-2.*h + 2.*p)                         &
     &                - .0373*SIN(-o) + .0064*SIN(h - pp)
               term2 = 1.0 -.0030*COS(-2.*h + 2.*p) - .0373*COS(o)

            case ( 'L2  ' )
               term1 = .2609*SIN(-2.*h + 2.*p) - .0370*SIN(-o)          &
     &               - .2503*SIN(2.*p) - .1103*SIN(2.*p + o)
               term2 = 1.0 + .2609*COS(-2.*h + 2.*p) - .0370*COS(o)     &
     &               - .2503*COS(2.*p) - .1103*COS(2.*p + o)

            case default
               term1 = 0.D0
               term2 = 0.D0

         end select

         if (term2 == 0.D0) then

          call fu_nodal( omega, perigee, constituent(i), 1, f(i), u(i))

         else

            f(i) = SQRT(term1**2 + term2**2)
            u(i) = ATAN( term1/term2 )/rad
         endif

      end do

      return
      END
