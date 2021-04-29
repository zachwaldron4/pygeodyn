!-----------------------------------
!     calculates the accelerations due to SRP
!     in the BFS for a single component
!     MKZ 16th July, 1999
!     Converted to Fortran & modified slightly by AS 2004
!     area is in metres, sun is the normalised sun_probe vector in BFS
!     norm is the component normal in BFS, m is the s/c mass in kg
!     acc is the resultant accelerations along the BFS (which are i.t.o. the ECI
!-----------------------------------
      SUBROUTINE PROCESS_COMPONENT(area,refl,spec,sun,norm,acc)

      IMPLICIT NONE

      DOUBLE PRECISION area,refl,spec,nom_mass,rnorm
      PARAMETER(nom_mass=8040.8) !nominal mass used to create SRP model
      DOUBLE PRECISION sun(0:2),norm(0:2),acc(0:2)
      DOUBLE PRECISION DOT_N
      double precision diff

      DOUBLE PRECISION E,c
!     //solar constant in W/m^2
      PARAMETER(E = 1368.0)
!     speed of light in vacuum
      PARAMETER(c = 299792458.0)

!     shear force vector and itself normalised
      DOUBLE PRECISION Sprime(0:2),Sprime_n(0:2)
      DOUBLE PRECISION temp1(0:2)

!     scalar product of Sun-probe vector and component normal
      DOUBLE PRECISION sdotn
!     angle between negative direction of sun-probe vector and the component nor
      DOUBLE PRECISION theta
!     scalar forces, normal and shear
      DOUBLE PRECISION Fn,Fs

!     calculate the shear vector, and normalise it
      CALL CROPDT(sun,norm,temp1)
      CALL CROPDT(norm,temp1,Sprime)
      CALL VNORM(Sprime,Sprime_n,rnorm)
!     calc 'angle of incidence'
!     sdotn=DOT_N(sun,norm)
      sdotn=sun(0)*norm(0)+sun(1)*norm(1)+sun(2)*norm(2)
      diff=sdotn+1.D0
      if(diff.lt.0.0000000001) sdotn=-0.999999999999D0
      theta=ACOS(-sdotn)

!     calculate the normal and shear forces

      Fn = -(area*E/c)*COS(theta)*(COS(theta)*(1.0+refl*spec)+&
     &(2.0*refl*(1.0-spec)/3.0))
      Fs =  (area*E/c)*COS(theta)*SIN(theta)*(1.0-refl*spec)

      acc(0) = (Sprime_n(0)*Fs+norm(0)*Fn)/nom_mass
      acc(1) = (Sprime_n(1)*Fs+norm(1)*Fn)/nom_mass
      acc(2) = (Sprime_n(2)*Fs+norm(2)*Fn)/nom_mass

      RETURN
      END
