      module DEMprojections_m
      use DEMglobals_m
      implicit none

      contains

      elemental subroutine project_gnomonic(lond, latd, lond0, latd0, &
              R, x, y)
          DOUBLE PRECISION, intent(in) :: lond, latd, lond0, latd0, R
          DOUBLE PRECISION, intent(out) :: x, y

          DOUBLE PRECISION :: cosc, lon, lat, lon0, lat0

          lon = lond * pi / 180.0D0
          lat = latd * pi / 180.0D0
          lon0 = lond0 * pi / 180.0D0
          lat0 = latd0 * pi / 180.0D0

          cosc = SIN(lat)*SIN(lat0) + COS(lat0)*COS(lat)*SIN(lon-lon0)

          x = R * COS(lat) * SIN(lon-lon0) / cosc
          y = R * (COS(lat0)*SIN(lat)-SIN(lat0)*COS(lat)*COS(lon-lon0)) &
                  / cosc
      end subroutine

      elemental subroutine unproject_gnomonic(xp, yp, lond0, latd0, R0, &
              lond, latd)
          DOUBLE PRECISION, intent(in) :: xp, yp, lond0, latd0, R0
          DOUBLE PRECISION, intent(out) :: lond, latd

          DOUBLE PRECISION :: rho, c, lon0, lat0

          lon0 = lond0 * pi / 180.0D0
          lat0 = latd0 * pi / 180.0D0

          rho = SQRT(xp*xp+yp*yp)/R0
          c = ATAN(rho)

          latd = 180.0D0/pi * ASIN(COS(c)*SIN(lat0)+yp*SIN(c)/rho &
                  * COS(lat0))
          lond = 180.0D0/pi * lon0+ATAN2(xp*SIN(c),rho*COS(c)*COS(lat0) &
                  - yp*SIN(c)*SIN(lat0))

          lond = modulo(lond, 360.0D0)
      end subroutine

      elemental subroutine project_stereographic(lond, latd, lond0, &
              latd0, R, x, y)
          DOUBLE PRECISION, intent(in) :: lond, latd, lond0, latd0, R
          DOUBLE PRECISION, intent(out) :: x, y

          DOUBLE PRECISION :: rho, c, lon0, lat0, lon, lat

          lon0 = lond0 * pi / 180.0D0
          lat0 = latd0 * pi / 180.0D0
          lon = modulo(lond, 360.0D0) * pi / 180.0D0
          lat = latd * pi / 180.0D0

          rho = (2*R)/(1+SIN(lat0)*SIN(lat)+COS(lat0)*COS(lat) &
                  * COS(lon-lon0))
          x=rho*COS(lat)*SIN(lon-lon0)
          y=rho*(COS(lat0)*SIN(lat)-SIN(lat0)*COS(lat)*COS(lon-lon0))

          if (lond == lond0 .and. latd == latd0) then
              x = 0.0D0
              y = 0.0D0
          end if
      end subroutine

      elemental subroutine unproject_stereographic(x, y, lond0, latd0, &
              R, lond, latd)
          DOUBLE PRECISION, intent(in) :: x, y, lond0, latd0, R
          DOUBLE PRECISION, intent(out) :: lond, latd

          DOUBLE PRECISION :: rho, c, lon0, lat0

          lon0 = lond0 * pi / 180.0D0
          lat0 = latd0 * pi / 180.0D0

          rho = SQRT(x**2 + y**2)
          c = 2.0D0 * ATAN2(rho, 2*R)

          latd = 180.0D0/pi * ASIN(COS(c)*SIN(lat0) &
                  + (COS(lat0)*y*SIN(c))/rho)
          lond = modulo(lond0 + 180.0D0/pi*ATAN2(x*SIN(c),COS(lat0) &
                  * rho*COS(c)-SIN(lat0)*y*SIN(c)),360.0D0)

          if (x == 0.0D0 .and. y == 0.0D0) then
              latd = latd0
              lond = lond0
          end if

          lond = modulo(lond, 360.0D0)
      end subroutine

      elemental subroutine project_lambertconformalconic(lond, latd, &
              lond0, latd0, latd1, latd2, offsetx, offsety, nx, &
              ny, map_ppd, x, y)
      DOUBLE PRECISION, intent(in) :: lond, latd, lond0, latd0, latd1, &
     &            latd2, offsetx, offsety, nx, ny, map_ppd
      DOUBLE PRECISION, intent(out) :: x, y

      DOUBLE PRECISION :: n, F, rho, rho0, lon0, lat0, lat1, lat2, lon,&
     &            lat, map_res

          lon0 = lond0 * pi / 180.0D0
          lat0 = latd0 * pi / 180.0D0
          lat1 = latd1 * pi / 180.0D0
          lat2 = latd2 * pi / 180.0D0
          lon = modulo(lond, 360.0D0) * pi / 180.0D0
          lat = latd * pi / 180.0D0

          map_res=2.0D0*pi/360.0D0/map_ppd
          n=LOG(COS(lat1)/COS(lat2))/LOG(TAN(pi/2.0D0+lat2/2.0D0) &
     &            /TAN(pi/2.0D0+lat1/2))
          F=COS(lat1)*TAN(pi/2.0D0+lat1/2.0D0)**n/n/map_res
          rho=F/(TAN(pi/2.0D0+lat/2.0D0)**n)
          rho0=F/(TAN(pi/2.0D0+lat0/2.0D0)**n)

          x=rho*SIN(n*(lon-lon0))+offsetx
          y=rho0-rho*COS(n*(lon-lon0))+ny-offsety
      end subroutine

      elemental subroutine spherical_to_cartesian(lond, latd, r, &
     &        xc, yc, zc)
          DOUBLE PRECISION, intent(in) :: lond, latd, r
          DOUBLE PRECISION, intent(out) :: xc, yc, zc

          DOUBLE PRECISION :: theta, phi

          theta = (90.0D0 - latd) * pi / 180.0D0
          phi = lond * pi / 180.0D0

          xc = r * SIN(theta) * COS(phi)
          yc = r * SIN(theta) * SIN(phi)
          zc = r * COS(theta)
      end subroutine

      elemental subroutine cartesian_to_spherical(xc, yc, zc, &
              lond, latd, r)
          DOUBLE PRECISION, intent(in) :: xc, yc, zc
          DOUBLE PRECISION, intent(out) :: lond, latd, r

          r=SQRT(xc**2+yc**2+zc**2)
          lond=MOD(ATAN2(yc,xc)*180.0D0/pi,360.0D0)
          latd=ATAN2(zc,SQRT(xc**2+yc**2))*180.0D0/pi
      end subroutine

      end module
