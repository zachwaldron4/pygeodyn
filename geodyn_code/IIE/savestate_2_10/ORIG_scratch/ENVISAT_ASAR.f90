

!$ ENVISAT_ASAR
      SUBROUTINE ENVISAT_ASAR(mass,x_sysm_n,y_sysm_n,z_sysm_n,busASAR)
      IMPLICIT NONE

      DOUBLE PRECISION mass
      DOUBLE PRECISION DOT_NN
      DOUBLE PRECISION x_sysm_n(0:2),y_sysm_n(0:2),z_sysm_n(0:2)
      DOUBLE PRECISION asar_mag
      DOUBLE PRECISION asarVEC(0:2)
      DOUBLE PRECISION busASAR(0:2)
      DOUBLE PRECISION c
!     speed of light in vacuum
      PARAMETER(c = 299792458.0D0)

!     ASAR average power output contribution as accn magnitude
      asar_mag=1000.D0/(mass*c)!1000W average power

!     unit ASAR output normal * -1.0 in BFS coords
!     (0.441638622,0.0,0.897193027)
!     scale by power output as accn for current mass
      asarVEC(0)=0.441638622D0*asar_mag
      asarVEC(1)=0.0D0
      asarVEC(2)=0.897193027D0*asar_mag

!     'project' asar_vec onto ECI_TOD BFS axes
!     busASAR(0)=DOT_NN(x_sysm_n,asarVEC)
      busASAR(0) = x_sysm_n(0)*asarVEC(0) + x_sysm_n(1)*asarVEC(1) &
     &           + x_sysm_n(2)*asarVEC(2)
!     busASAR(1)=DOT_NN(y_sysm_n,asarVEC)
      busASAR(1) = y_sysm_n(0)*asarVEC(0) + y_sysm_n(1)*asarVEC(1) &
     &           + y_sysm_n(2)*asarVEC(2)
!     busASAR(2)=DOT_NN(z_sysm_n,asarVEC)
      busASAR(2) = z_sysm_n(0)*asarVEC(0) + z_sysm_n(1)*asarVEC(1) &
     &           + z_sysm_n(2)*asarVEC(2)
!     busASAR now gives final ms^-2 accelerations in ECI_TOD!

      RETURN
      END
