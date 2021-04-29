!$SOLRAD_TUM
      subroutine solrad_tum(iflag1,sunvn,bvn,                       &
     &acceltot,sppartials,opartials,paramsp,paramxp,paramzp,        &
     &paramzm,paramyb,iblock,nprn,signepsdot,                       &
     &sinepsilon,cosepsilon,s0fact,iflag3,ladj,lboxwing_present)
!
!********1*********2*********3*********4*********5*********6*********7**
! SOLRAD_TUM            02/20/13            1304.0    Oscar Colombo
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   iflag1   I
!   iflag3   I  Two flags explained at the beginning of "solartum".
!   sunvn    I  unit vector pointing from the geocenter to the heliocenter.
!   bvn      I  unit vector in the plane of the solar panels and normal
!               to their axis.
!   iblock   I  Block of GPS satellite (see comments in "solartum").
!   nprn     I  Satellite PRN.
! sinepsilon,
! cosepsilon O  sine and cosine of the Earth-satellite-Sun
!               angle epsilon.
! signepsdot O  sign of the time derivative of epsilon (= +1 or -1).
!   s0fact   O  Sunlight-dimming factor. It depends on the proportion
!               of the sun not eclipsed by the Earth and on the actual
!               distance to the Sun.
!   acceltot O  solrad acceleration vector.
! sppartials O  solar panel "wings" parameter partials (forcing terms
!                of the corresponding variational equations).
!  opartials O  as above, but for the parameters of the "box".
!  paramsp,
!  paramxp,
!  paramzp,
!  paramzm,
!  paramyb:  O
!               arrays containing the values of adjusted parameters
!               from the latest solution iteration.
!               Endings signify: sp = solar panels;
!               xp, zp, zm = X+, Z+ and Z- sides of the "box";
!               yb = Y-bias.
!
! This subroutine computes the solar radiation acceleration and optical
! parameter partials of the box-wing model from the TH Muenchen.
!
! Main reference:
! "Adjustable box-wing model for solar radiation pressure impacting
! GPS satellites", by C.J. Rodriguez-Solano, U. Hugentobler, P. Steigenberger,
! in "Advances in Space Research, 0273-1177, 2012 COSPAR.
! Published by Elsevier Ltd, doi:10.1016/j.asr.2012.01.016.
!
! As written, it is based on the assumption that the orientation
! is the nominal one for GPS.
! When the use of a non-nominal yaw during eclipses is implemented,
! as signaled with "iyaw" and "iyawn" (see calling subroutine "solartum")
! another subroutine will be written to deal with that situation,
! based this subroutine but substantially modified.
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION   (A-H,O-Z),LOGICAL (L)
      SAVE

      parameter(nb=6)
      DOUBLE PRECISION mass
!
      COMMON/CITER /NINNER,NARC,NGLOBL
! To pass to main program the a priori parameters:
      COMMON/PARAM0/spsf0(6),deltasp(6),rhosp(6),rhoxp(6),   &
     & rhozp(6),rhozm(6),aplusdxp(6),aplusdzp(6),aplusdzm(6),&
     & asmcsp0(6),asmcxp0(6),asmczp0(6),asmczm0(6),XPRM0
!
      dimension paramiisp(4),paramiirsp(4),paramiifsp(4),       &
     &paramiixp(4),paramiirxp(4),paramiifxp(4),                 &
     &paramiizp(4),paramiirzp(4),paramiifzp(4),                 &
     &paramiizm(4),paramiirzm(4),paramiifzm(4),                 &
     &paramisp(4),paramixp(4),paramizp(4),paramizm(4),          &
     &sunvn(3),bvn(3),mass(nb),                                 &
     &xpn(3),ypn(3),zpn(3),                                     &
     &accelsp(3),acceltot(3),rm(3,3),               &
     &sppartials(3,2),opartials(3,7),paramsp(2),           &
     &paramxp(2),paramzp(2),paramzm(2),paramyb(1)

! A priori vaalues of the box-wing model for the various satellite blocks:
      data paramiisp/11.851D0,0.746D0,0.057D0,0.197D0/,         &
     &paramiixp/2.719,0.500D0,0.400D0,0.100D0/,                 &
     &paramiizp/2.881D0,0.440D0,0.448D0,0.112D0/,               &
     &paramiizm/2.881D0,0.582D0,0.335D0,0.083D0/,               &
     &paramiirsp/13.920D0,0.707D0,0.044D0,0.249D0/,             &
     &paramiirxp/4.110D0,0.940D0,0.060D0,0.D0/,                 &
     &paramiirzp/4.250D0,0.940D0,0.060D0,0.D0/,                 &
     &paramiirzm/4.250D0,0.940D0,0.060D0,0.D0/,                 &
     &paramiifsp/22.25D0,0.770D0,0.035D0,0.196D0/,              &
     &paramiifxp/5.72D0,0.440D0,0.448D0,0.112D0/,               &
     &paramiifzp/5.40D0,0.440D0,0.448D0,0.112D0/,               &
     &paramiifzm/5.40D0,1.D0,0.D0,0.D0/,                        &
     &paramisp/6.053D0,0.722D0,0.042D0,0.236D0/,                &
     &paramixp/2.047D0,0.432D0,0.134D0,0.434D0/,                &
     &paramizp/1.510D0,0.140D0,0.215D0,0.645D0/,                &
     &paramizm/1.510D0,0.535D0,0.093D0,0.372D0/,                &
     &mass/490.D0,880.D0,975.D0,1100.D0,1100.D0,1630.D0/ ! Block I : aver

      data xpn/1.D0,0.D0,0.D0/,ypn/0.D0,1.D0,0.D0/,zpn/0.D0,0.D0,1.D0/, &
     &istart/1/,c/299792458.0D0/,s0/1367.D0/ !  s0 is the nominal
!                                        solar irradiance at 1 AU in Watt/m^2.
!
! Calculate the solrad acceleration from the solar panels and the SP partials:
!
! accelsp = -Asp/M*S/c*{(5/3*aplusd+2*rho)*sunvn
!           + 2(deltasp/3+rhosp)*sign(epsilondot}*dthetasp*bvn} Equation (9).
!
!
! *************   SOLAR PANELS ******************************************
!
! Find the values of the solar panels' contribution to the solrad
! acceleration and of the corresponding partials:
!
      LNADJ=.NOT.LADJ

! For a non box wing adjustment runa and for iteration 1
      if(.not. lboxwing_present) then
      aplusdxp0 = aplusdxp(iblock) ! for the corresponding sat. block,
      aplusdzp0 = aplusdzp(iblock) ! bringing them in only when necessary
      aplusdzm0 = aplusdzp(iblock)
      rhoxp0 = rhoxp(iblock)
      rhozp0 = rhozp(iblock)
      rhozm0 = rhozm(iblock)
      spsf00  = spsf0(iblock)
      spsf = spsf00
      else
! Not the first iteration: use the adjusted
! parameter values from the previous one.
      spsf     = paramsp(1)
      dthetasp = paramsp(2)
      endif

      asmcsp = asmcsp0(iblock)*s0fact

      do i = 1,3
      if(iflag1.eq.1) then
! Calculate full term, or just contribution from "dthetasp".
! Equation (9).
      accelsp(i) = asmcsp*(spsf*sunvn(i)+                     &
     &             2.D0*(0.333333333333D0*deltasp(iblock)+    &
     &             rhosp(iblock))*signepsdot*dthetasp*bvn(i))
      else
      accelsp(i) = asmcsp*2.D0*(0.333333333333D0*deltasp(iblock)+ &
     &rhosp(iblock))*signepsdot*dthetasp*bvn(i)
      endif
      enddo
!
! Calculate the solar panels' partials:
!
      if(lnadj) goto 100
      do i = 1,3
      if(iflag1.eq.1) then
      sppartials(i,1) = asmcsp*sunvn(i)                   ! Equation (10)
      else
      sppartials(i,1) = 0.D0
      endif
      sppartials(i,2) = asmcsp*2.D0*(0.333333333333*deltasp(iblock) &
     &                  +rhosp(iblock))*signepsdot*bvn(i) ! Equation (15)
      enddo
100   continue
!
      do i = 1,3
      acceltot(i) = accelsp(i)
      enddo
      if(iflag1.eq.0) return
!
! *************   SOLAR PANELS END ***************************************
!
! Calculate the accelerations and partials of the X+, Z+ and Z- sides
! of the bus' box:
! (The equation numbers in the comments are those in Rodriguez-Solano et al.)
! The X+ contribution to the total acceleration, and the partials for
! the next estimate of aplusd (alpha+delta) and rho for X+:
!
      if(.not. lboxwing_present) then
      aplusd = aplusdxp0
      rho    = rhoxp0
      else
      aplusd = paramxp(1)
      rho    = paramxp(2)
      endif

      asmcxp = asmcxp0(iblock)*s0fact
      do i = 1,3
      accelxp_i   = asmcxp*sinepsilon*(aplusd*(sunvn(i)+ &
     &0.666666666666D0*xpn(i))+2.D0*rho*sinepsilon*xpn(i))
      acceltot(i) = acceltot(i)+accelxp_i                    ! Accel. fro
! From equation (9).

      if(lnadj) goto 200
      opartials(i,1) =  asmcxp*sinepsilon*(sunvn(i)+     &
     &0.666666666666D0*xpn(i))                               ! Partials,
      opartials(i,2) =  asmcxp*2.D0*sinepsilon**2*xpn(i)    ! Equations (
200   continue
      enddo
!
! The Z+ contribution to the total acceleration, and the partials for
! the next iteration of the values of aplusd (alpha+delta) and rho
! for surface Z+:
!
!   COSEPSILON IS COMPUTED IN SOLARTUM THE NEXT ACC MIGHT BE 0
      icosepsilon = 0
      if(cosepsilon.gt.0.D0) icosepsilon = 1
!      = 1 Z+ is illuminated;
!      = 0 Z- is.

      if(icosepsilon.eq.1) then
      if(.not. lboxwing_present) then
      aplusd = aplusdzp0
      rho    = rhozp0
      else
      aplusd = paramzp(1)
      rho    = paramzp(2)
      endif
      asmczp = asmczp0(iblock)*s0fact
      do i = 1,3
      accelzp_i   = asmczp*cosepsilon*(aplusd*(sunvn(i)+ &
     &0.666666666666D0*zpn(i))+2.D0*rho*cosepsilon*zpn(i))
      acceltot(i) = acceltot(i)+accelzp_i                    ! Accel. fro
! Equation (9).
      if(lnadj) goto 300
      opartials(i,3) =  asmczp*cosepsilon*(sunvn(i)+     &
     &0.666666666666D0*zpn(i))                               ! Partials,
      opartials(i,4) =  asmczp*2.D0*cosepsilon**2*zpn(i)     ! Equations
300   continue
      enddo
      else
      if(lnadj) goto 301
      do i = 1,3
      opartials(i,3) = 0.D0
      opartials(i,4) = 0.D0
      enddo
301   continue
      endif
!
! The Z- contribution to the total acceleration, and the partials for
! the next iteration values of aplusd (alpha+delta) and rho for
! surface Z-:
!
      if(icosepsilon.eq.0) then
      if(.not. lboxwing_present) then
      aplusd = aplusdzm0
      rho    = rhozm0
      else
      aplusd = paramzm(1)
      rho    = paramzm(2)
      endif
      asmczm = asmczm0(iblock)*s0fact
      do i = 1,3
! Terms in "zpn" have a minus
      accelzm_i   = asmczm*cosepsilon*(aplusd*(-sunvn(i)+  &
     &0.666666666666D0*zpn(i))-2.D0*rho*cosepsilon*zpn(i))   ! sign, as z
      acceltot(i) = acceltot(i)+accelzm_i                    ! Accel. fro
      if(lnadj) goto 400
      opartials(i,5) = asmczm*cosepsilon*(-sunvn(i)+ &
     &0.666666666666D0*zpn(i))                               ! Partials,
      opartials(i,6) = -asmczm*2.D0*cosepsilon**2*zpn(i)
400   continue
      enddo
      else
      if(lnadj) goto 401
      do i = 1,3
      opartials(i,5) = 0.D0
      opartials(i,6) = 0.D0
      enddo
401   continue
      endif
!
!     Acceleration and partials for the Y-bias:
!
      if(iflag3.eq.1) then ! If Y-bias accel. and partials required.
      if(lboxwing_present) then
      yb = paramyb(1)
      endif
      do i = 1,3
      accelyb_i = yb*ypn(i)
      acceltot(i) = acceltot(i)+accelyb_i
      enddo
      if(lnadj) goto 500
      do i = 1,3
      opartials(i,7) = ypn(i)
      enddo
500   continue
      else
      if(lnadj) goto 501
      do i = 1,3
      opartials(i,7) = 0.D0
      enddo
501   continue


      endif
!
      return
      end
