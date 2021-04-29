!
! ------------------------------------------------------------------------
!
      FUNCTION   NMF_W_RATE ( PHI_GCN, EL, EL_RATE )
! ************************************************************************
! *                                                                      *
! *   Rourinte NMF_W  computes the rate of change of the NMF wet mapping *
! *   function at the station with given geocentroc latitude in          *
! *   a direction with given elevation.                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   PHI_GCN ( REAL*8    ) -- Geocentric latitude of the station,       *
! *                            in rad, in range [-pi/2, pi/2].           *
! *        EL ( REAL*8    ) -- Elevation above the horizons in rad.      *
! *   EL_RATE ( REAL*8    ) -- Rate of change of elevation rate in rad/s *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   <NMF_W> ( REAL*8    ) -- Value of the mapping function.            *
! *                                                                      *
! *  ### 23-AUG-2002   NMF_W_RATE  v1.1 (c)  L. Petrov  20-APR-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      DOUBLE PRECISION PHI_GCN, EL, NMF_W_RATE
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
      DOUBLE PRECISION PN, PD, PN_DER, PD_DER, EL_RATE, CONS
      DOUBLE PRECISION A, B, C, SIN_EL, COS_EL
      DOUBLE PRECISION FLIN8
      INTEGER  IP
      INTEGER  IXMN8
!
      IF ( EL == 0.0D0 ) THEN
           NMF_W_RATE = 0.0D0
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
      A = FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,1), IP )
      B = FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,2), IP )
      C = FLIN8 ( ABS(PHI_GCN), LAT_RAN, LAT_NMF, ABC_AVG(1,3), IP )
      END IF
!
      SIN_EL = SIN(EL)
      COS_EL = COS(EL)
!
      CONS = (1.D0   + A/(1.D0   + B/(1.D0   + C) ) )
!
      PN = SIN_EL**2 + C*SIN_EL + B
      PD = SIN_EL**3 + C*SIN_EL + (A+B)*SIN_EL + A*C
      PN_DER = (2.0D0*SIN_EL + C)*COS_EL*EL_RATE
      PD_DER = (3.0D0*SIN_EL**2 + 2.0D0*C*SIN_EL + (A+B) )*COS_EL*EL_RATE
!
      NMF_W_RATE  = CONS*(PN_DER*PD - PD_DER*PN)/PD**2
!
      RETURN
      END  FUNCTION  NMF_W_RATE
