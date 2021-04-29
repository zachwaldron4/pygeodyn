      FUNCTION   NMF_H ( MJD, SEC, PHI_GCN, HEI_ELL, EL )
! ************************************************************************
! *                                                                      *
! *   Routine  NMF_H  computes the value of the NMH hydrostatic mapping  *
! *   function above the station with given geocentric latitude and      *
! *   ellipsoid height in a direction with given elevation on the given  *
! *   date.                                                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MJD ( INTEGER*4 ) -- Integer modified Julian date on the       *
! *                            midnight of the epoch of interest.        *
! *       SEC ( REAL*8    ) -- Seconds fraction of the date: time in     *
! *                            seconds elapsed from the midnight.        *
! *   PHI_GCN ( REAL*8    ) -- Geocentric latitude of the station,       *
! *                            in rad, in range [-pi/2, pi/2].           *
! *   HEI_ELL ( REAL*8    ) -- Station height above the reference        *
! *                            ellipsoid, in meters.                     *
! *        EL ( REAL*8    ) -- Elevation above the horizons in rad.      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   <NMF_H> ( REAL*8    ) -- Value of the mapping function.            *
! *                                                                      *
! *  ### 23-AUG-2002     NMF_H     v1.2 (c)  L. Petrov  20-APR-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  MJD
      DOUBLE PRECISION SEC, PHI_GCN, HEI_ELL, EL, NMF_H
      INTEGER    MJD__NMF, LAT_RAN
      PARAMETER  ( MJD__NMF = 44238+28 ) ! 28-JAN-80
      PARAMETER  ( LAT_RAN  = 5 )
      DOUBLE PRECISION PI, PI2
      PARAMETER  ( PI=3.141592653589793D0, PI2=PI*2.D0 )
      DOUBLE PRECISION ABC_AVG(LAT_RAN,3), ABC_AMP(LAT_RAN,3),        &
     &           LAT_NMF(LAT_RAN),                              &
     &           A_HEI, B_HEI, C_HEI
      DATA ABC_AVG / &
     &     1.2769934D-3, 1.2683230D-3, 1.2465397D-3, 1.2196049D-3,   &
     &     1.2045996D-3, &
     &     2.9153695D-3, 2.9152299D-3, 2.9288445D-3, 2.9022565D-3,   &
     &     2.9024912D-3, &
     &     62.610505D-3, 62.837393D-3 ,63.721774D-3 ,63.824265D-3,   &
     &     64.258455D-3 &
     &             /
!
      DATA ABC_AMP / &
     &     0.0,   1.2709626D-5, 2.6523662D-5, 3.4000452D-5,         &
     &            4.1202191D-5, &
     &     0.0,   2.1414979D-5, 3.0160779D-5, 7.2562722D-5,         &
     &            11.723375D-5, &
     &     0.0,   9.0128400D-5, 4.3497037D-5, 84.795348D-5,         &
     &            170.37206D-5 &
     &             /
!
      DATA LAT_NMF / &
     &               0.2617993877991D0,  & ! 15.0
     &               0.5235987755984D0,  & ! 30.0
     &               0.7853981633974D0,  & ! 45.0
     &               1.0471975511966D0,  & ! 60.0
     &               1.3089969389957D0   & ! 75.0
     &             /
      DATA   A_HEI / 2.53D-5 / &
     &       B_HEI / 5.49D-3 / &
     &       C_HEI / 1.14D-3 /
!
      DOUBLE PRECISION A1, B1, C1, A2, B2, C2, A, B, C, PHAS, SIN_EL
      DOUBLE PRECISION FLIN8
      INTEGER  IP
      INTEGER  IXMN8
!
      IF ( EL == 0.0D0 ) THEN
           NMF_H = 0.0D0
           RETURN
      END IF
!
      IP = IXMN8 ( LAT_RAN, LAT_NMF, ABS(PHI_GCN) )
      IF ( IP .EQ. -1 ) THEN
           A1 = ABC_AVG(1,1)
           B1 = ABC_AVG(1,2)
           C1 = ABC_AVG(1,3)
           A2 = ABC_AMP(1,1)
           B2 = ABC_AMP(1,2)
           C2 = ABC_AMP(1,3)
         ELSE IF ( IP .EQ. -2 ) THEN
           A1 = ABC_AVG(LAT_RAN,1)
           B1 = ABC_AVG(LAT_RAN,2)
           C1 = ABC_AVG(LAT_RAN,3)
           A2 = ABC_AMP(LAT_RAN,1)
           B2 = ABC_AMP(LAT_RAN,2)
           C2 = ABC_AMP(LAT_RAN,3)
         ELSE
       A1 =FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,1), IP )
       B1 =FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,2), IP )
       C1 =FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,3), IP )
!
       A2 =FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AMP(1,1), IP )
       B2 =FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AMP(1,2), IP )
       C2 =FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AMP(1,3), IP )
      END IF
!
      SIN_EL = SIN(EL)
!
      PHAS = (MJD - MJD__NMF + SEC/86400.0)/365.25D0*PI2
      IF ( PHI_GCN .LT. 0.0D0 ) PHAS = PHAS + PI
!
      A = A1 - A2*COS(PHAS)
      B = B1 - B2*COS(PHAS)
      C = C1 - C2*COS(PHAS)
!
      NMF_H  = (1.D0   + A/(1.D0   + B/(1.D0   + C) ) )/ &
     &         (SIN_EL + A/(SIN_EL + B/(SIN_EL + C) ) )  + &
     &         ( 1.D0/SIN_EL - &
     &         (1.D0   + A_HEI/(1.D0   + B_HEI/(1.D0    + C_HEI) ) )/ &
     &          (SIN_EL + A_HEI/(SIN_EL + B_HEI/(SIN_EL  + C_HEI) ) ) &
     &         )*1.D-3*HEI_ELL
!
      RETURN
      END  !#!  NMF_H  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   NMF_W ( PHI_GCN, EL )
! ************************************************************************
! *                                                                      *
! *   Rourinte NMF_W  computes the value of the NMF wet mapping function *
! *   at the station with given geocentroc latitude in a direction with  *
! *   given elevation.                                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   PHI_GCN ( REAL*8    ) -- Geocentric latitude of the station,       *
! *                            in rad, in range [-pi/2, pi/2].           *
! *        EL ( REAL*8    ) -- Elevation above the horizons in rad.      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   <NMF_W> ( REAL*8    ) -- Value of the mapping function.            *
! *                                                                      *
! *  ### 23-AUG-2002     NMF_W     v1.1 (c)  L. Petrov  20-APR-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      DOUBLE PRECISION PHI_GCN, EL, NMF_W
      INTEGER    LAT_RAN
      PARAMETER  ( LAT_RAN  = 5)
      DOUBLE PRECISION PI, PI2
      PARAMETER  ( PI=3.141592653589793D0, PI2=PI*2.D0 )
      DOUBLE PRECISION ABC_AVG(LAT_RAN,3), LAT_NMF(LAT_RAN)
      DATA ABC_AVG / &
     &     5.8021897D-4, 5.6794847D-4, 5.8118019D-4, 5.9727542D-4,      &
     &     6.1641693D-4, &
     &     1.4275268D-3, 1.5138625D-3, 1.4572752D-3, 1.5007428D-3,      &
     &     1.7599082D-3, &
     &     4.3472961D-2, 4.6729510D-2, 4.3908931D-2, 4.4626982D-2,      &
     &     5.4736038D-2 &
     &             /
!
      DATA LAT_NMF / &
     &               0.2617993877991D0,  & ! 15.0
     &               0.5235987755984D0,  & ! 30.0
     &               0.7853981633974D0,  & ! 45.0
     &               1.0471975511966D0,  & ! 60.0
     &               1.3089969389957D0   & ! 75.0
     &             /
!
      DOUBLE PRECISION A, B, C, SIN_EL
      DOUBLE PRECISION FLIN8
      INTEGER  IP
      INTEGER  IXMN8
!
      IF ( EL == 0.0D0 ) THEN
           NMF_W = 0.0D0
           RETURN
      END IF
!
      IP = IXMN8 ( LAT_RAN, LAT_NMF, ABS(PHI_GCN) )
      IF ( IP .EQ. -1 ) THEN
           A = ABC_AVG(1,1)
           B = ABC_AVG(1,2)
           C = ABC_AVG(1,3)
         ELSE IF ( IP .EQ. -2 ) THEN
           A = ABC_AVG(LAT_RAN,1)
           B = ABC_AVG(LAT_RAN,2)
           C = ABC_AVG(LAT_RAN,3)
         ELSE
        A= FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,1), IP )
        B= FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,2), IP )
        C= FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,3), IP )
      END IF
!
      SIN_EL = SIN(EL)
!
      NMF_W  = (1.D0   + A/(1.D0   + B/(1.D0   + C) ) )/ &
     &         (SIN_EL + A/(SIN_EL + B/(SIN_EL + C) ) )
!
      RETURN
      END  !#!  NMF_W  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   NMF_H_RATE ( MJD, SEC, PHI_GCN, HEI_ELL, EL, EL_RATE )
! ************************************************************************
! *                                                                      *
! *   Routine  NMF_H  computes the rate of change of the NMH hydrostatic *
! *   mapping function above the station with a given geocentric         *
! *   latitude and ellipsoid height in a direction with a given          *
! *   elevation on the given date.                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MJD ( INTEGER*4 ) -- Integer modified julian date on the       *
! *                            midnight of the epoch of interest.        *
! *       SEC ( REAL*8    ) -- Seconds fraction of the date: time in     *
! *                            seconds elapsed from the midnight.        *
! *   PHI_GCN ( REAL*8    ) -- Geocentric latitude of the station,       *
! *                            in rad, in range [-pi/2, pi/2].           *
! *   HEI_ELL ( REAL*8    ) -- Station height above the reference        *
! *                            ellipsoid, in meters.                     *
! *        EL ( REAL*8    ) -- Elevation above the horizons in rad.      *
! *   EL_RATE ( REAL*8    ) -- Rate of change of elevation rate in rad/s *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   <NMF_H> ( REAL*8    ) -- Value of the mapping function.            *
! *                                                                      *
! *  ### 23-AUG-2002   NMF_H_RATE  v2.1 (c)  L. Petrov  20-APR-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  MJD
      DOUBLE PRECISION SEC, PHI_GCN, HEI_ELL, EL, EL_RATE, NMF_H_RATE
      INTEGER    MJD__NMF, LAT_RAN
      PARAMETER  ( MJD__NMF = 44238+28 ) ! 28-JAN-80
      PARAMETER  ( LAT_RAN  = 5)
      DOUBLE PRECISION PI, PI2
      PARAMETER  ( PI=3.141592653589793D0, PI2=PI*2.D0 )
      DOUBLE PRECISION PN,  PD,  PN_DER,  PD_DER,  CONS, &
     &           PNH, PDH, PNH_DER, PDH_DER, CONS_H
      DOUBLE PRECISION ABC_AVG(LAT_RAN,3), ABC_AMP(LAT_RAN,3),      &
     &           LAT_NMF(LAT_RAN), &
     &           A_HEI, B_HEI, C_HEI, A_DER
      DATA ABC_AVG / &
     &     1.2769934D-3, 1.2683230D-3, 1.2465397D-3, 1.2196049D-3,    &
     &     1.2045996D-3, &
     &     2.9153695D-3, 2.9152299D-3, 2.9288445D-3, 2.9022565D-3,    &
     &     2.9024912D-3, &
     &     62.610505D-3, 62.837393D-3 ,63.721774D-3 ,63.824265D-3,    &
     &     64.258455D-3 &
     &             /
!
      DATA ABC_AMP / &
     &     0.0, 1.2709626D-5,2.6523662D-5, 3.4000452D-5, 4.1202191D-5, &
     &     0.0, 2.1414979D-5,3.0160779D-5, 7.2562722D-5, 11.723375D-5, &
     &     0.0, 9.0128400D-5,4.3497037D-5, 84.795348D-5, 170.37206D-5 &
     &             /
!
      DATA LAT_NMF / &
     &               0.2617993877991D0,  & ! 15.0
     &               0.5235987755984D0,  & ! 30.0
     &               0.7853981633974D0,  & ! 45.0
     &               1.0471975511966D0,  & ! 60.0
     &               1.3089969389957D0   & ! 75.0
     &             /
      DATA   A_HEI / 2.53D-5 / &
     &       B_HEI / 5.49D-3 / &
     &       C_HEI / 1.14D-3 /
!
      DOUBLE PRECISION A1, B1, C1, A2, B2, C2, A, B, C, PHAS, SIN_EL, COS_EL
      DOUBLE PRECISION FLIN8
      INTEGER  IP
      INTEGER  IXMN8
!
      IF ( EL == 0.0D0 ) THEN
           NMF_H_RATE = 0.0D0
           RETURN
      END IF
!
      IP = IXMN8 ( LAT_RAN, LAT_NMF, ABS(PHI_GCN) )
      IF ( IP .EQ. -1 ) THEN
           A1 = ABC_AVG(1,1)
           B1 = ABC_AVG(1,2)
           C1 = ABC_AVG(1,3)
           A2 = ABC_AMP(1,1)
           B2 = ABC_AMP(1,2)
           C2 = ABC_AMP(1,3)
         ELSE IF ( IP .EQ. -2 ) THEN
           A1 = ABC_AVG(LAT_RAN,1)
           B1 = ABC_AVG(LAT_RAN,2)
           C1 = ABC_AVG(LAT_RAN,3)
           A2 = ABC_AMP(LAT_RAN,1)
           B2 = ABC_AMP(LAT_RAN,2)
           C2 = ABC_AMP(LAT_RAN,3)
         ELSE
       A1= FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,1), IP )
       B1= FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,2), IP )
       C1= FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,3), IP )
!
       A2= FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AMP(1,1), IP )
       B2= FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AMP(1,2), IP )
       C2= FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AMP(1,3), IP )
      END IF
!
      SIN_EL = SIN(EL)
      COS_EL = COS(EL)
!
      PHAS = (MJD - MJD__NMF + SEC/86400.0)/365.25D0*PI2
      IF ( PHI_GCN .LT. 0.0D0 ) PHAS = PHAS + PI
!
      A = A1 - A2*COS(PHAS)
      B = B1 - B2*COS(PHAS)
      C = C1 - C2*COS(PHAS)
      A_DER = A2/86400.0D0/365.25D0*PI2*SIN(PHAS)
!
      CONS   = (1.D0   + A/(1.D0 + B/(1.D0 + C) ) )
      CONS_H = (1.D0   + A_HEI/(1.D0 + B_HEI/(1.D0 + C_HEI) ) )
!
      PN = SIN_EL**2 + C*SIN_EL + B
      PD = SIN_EL**3 + C*SIN_EL**2 + (A+B)*SIN_EL + A*C
      PN_DER = (2.0D0*SIN_EL + C)*COS_EL*EL_RATE
      PD_DER=(3.0D0*SIN_EL**2+2.0D0*C*SIN_EL+(A+B))*COS_EL*EL_RATE + &
     &          A_DER*(SIN_EL + C)
!
      PNH = SIN_EL**2 + C_HEI*SIN_EL + B_HEI
      PDH = SIN_EL**3 + C_HEI*SIN_EL**2 + (A_HEI+B_HEI)*SIN_EL +      &
     &   A_HEI*C_HEI
      PNH_DER = (2.0D0*SIN_EL + C_HEI)*COS_EL*EL_RATE
      PDH_DER = (3.0D0*SIN_EL**2 + 2.0D0*C_HEI*SIN_EL + &
     &           (A_HEI+B_HEI) )*COS_EL*EL_RATE
!
      NMF_H_RATE  = CONS*(PN_DER*PD - PD_DER*PN)/PD**2 + &
     &              A_DER/(1.D0   + B/(1.D0   + C) )*PN/PD + &
     &              (-COS_EL*EL_RATE/SIN_EL**2 - &
     &              CONS_H*(PNH_DER*PDH - PDH_DER*PNH)/PDH**2 )* &
     &              HEI_ELL*1.D-3
!
      RETURN
      END  FUNCTION   NMF_H_RATE
