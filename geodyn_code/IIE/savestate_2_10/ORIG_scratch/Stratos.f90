!$STRATOS
      Subroutine Stratos(z75,T75,p75,M75,zF,TF,pF,MF,Rstar,rref,        &
     & gz,z,Tz,pz,dz,Hz,tgrad,z50,T50,zFp,ipopt,Hrho)
!********1*********2*********3*********4*********5*********6*********7**
! STRATOS          00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME         I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   z75(z50,zF)   I    S    Areographic Altitude correspoding to 75 km
!                           (50 km ,zF km) areopotential altitude above
!                           local surface
!   T75(T50,TF)   I    S    TEMPERATURE COR. 75 (50,zF) km
!   M75 (MF)      I    S    MOLECULAR WEIGHT AT 75 (zF) km
!   p75 (pF)      I    S    PRESUURE AT 75(zF) km
!   Rstar         I    S    Universal gas constant
!   rref          I    S
!   gz            I    S    Acceleration of Gravity at z
!   z             I    S    Areopotetial altitude of spacecraft
!   ipopt         I    S    INTERPOLATION OPTION ( 0 = regression,
!                                                  1 = hydrostatic  )
!   zFp           O    S
!   Tz,pz,Hz      O    S    Temperature, pressure and scale height at z
!    dz           O    S    Density at z
!   tgrad         O    S    Temperature gradients
!   Hrho
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
       DOUBLE PRECISION M75,MF,M
!...  z75 = areographic altitude above reference ellipsoid corres-
!...  ponding to 75 km areopotential altitude above local surface.
!...  H75 and H50 are areopotential altitudes above reference
!     ellipsoid, corresponding to z75 and Z50
      H75 = z75/(1.D0 + z75/rref)
      H50 = z50/(1.D0 + Z50/rref)
!...  HF = areopotential altitude corresponding to zF, base of
!...  thermosphere areographic altitude
      HF = zF/(1.D0 + zF/rref)
!...  H = areopotential altitude corresponding to current areo-
!...  graphic altitude z, relative to reference ellipsoid
      H = z/(1.D0 + z/rref)
!...  interpolate to get M = molecular weight at height z
      dH = (H - H75)/(HF - H75)
      M = M75 + (MF - M75)*dH
!...  HP and HFP = molecular-weight-adjusted areopotential heights
      HFP = MF*HF/M75
      HP = M*H/M75
!...  HM = midpoint (adjusted) height between z75 and ZF
      HM = 0.5D0*(HFP + H75)
!...  Use quadratic interpolation to get temperature at height HM
      TM = -0.25D0*T50*(HFP-H75)**2/((H75-H50)*(HFP-H50))               &
     &  + 0.5D0*T75*(HM-H50)/(H75-H50) + 0.5D0*TF*(HM-H50)/(HFP-H50)
      If (TM.eq.T75)TM = T75 - 1.D0
      If (TM.eq.TF)TM = TF - 1.D0
!...  Compute temperature gradients for layers H75 to HM and HM to HF
      gam1 = (TM - T75)/(HM - H75)
      If (ipopt.ne.1)Then
        gam2 = (TF - TM)/(HFP - HM)
!...    Find exponent factors for pressure variation in the 2 layers
        goR = LOG(p75/pF)/(LOG(TM/T75)/gam1 + LOG(TF/TM)/gam2)
        pM = p75*(T75/TM)**(goR/gam1)
      Else
        goR = (1000.D0*M75*gz/Rstar)*(1.D0 + z/rref)**2
        pM = p75*(T75/TM)**(goR/gam1)
        HFP = HM + (TF - TM)*LOG(PM/pF)/(goR*LOG(TF/TM))
        gam2 = (TF - TM)/(HFP - HM)
        HF = M75*HFP/MF
        zFp = HF/(1.D0 - HF/rref)
      Endif
!...  Interpolate for temperature Tz and pressure pz at height z,
!     using linear temperature gradients in each of the two layers
      If (HP .lt. HM)Then
        tgrad = gam1
        Tz = T75 + tgrad*(HP - H75)
        expon = goR/gam1
        pz = p75*(T75/Tz)**expon
      Else
        tgrad = gam2
        Tz = TM + tgrad*(HP - HM)
        expon = goR/gam2
        If (HFP.gt.HM)Then
          pz = pM*(TM/Tz)**expon
        Else
          pz = pM
          Tz = TM
        Endif
      Endif
!...  Convert units on temperature gradient for output
      tgrad = 0.001D0*tgrad
!...  Compute density dz at height z
  100 dz = M*pz/(Rstar*Tz)
!...  Hz = scale height at height z
      Hz = 0.001D0*Rstar*Tz/(gz*M)
      Hrho = Hz/(1.D0 + tgrad*Rstar/(M*gz))
      Return
      END
