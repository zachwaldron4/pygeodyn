!$NUTDT2
       BLOCK DATA NUTDT2
!********1*********2*********3*********4*********5*********6*********7
!                                               PGMR - S.LUO
!
!  FUNCTION
!       GET THE CONSTANTS FOR MARS ORBIT ELEMENTS CALCULATION
!
! PARAMETERS
!
!       W = W(1) + W(2) * T + W(3) * T**2 + W(4) * T**3
!                + W(5) * T**4 + W(6) * T**5
!
!      OM = OM(1) + OM(2) * T + OM(3) * T**2 + OM(4) * T**3
!                + OM(5) * T**4 + OM(6) * T**5
!
!      DL = DL(1) + DL(2) * T + DL(3) * T**2 + DL(4) * T**3
!
!      CI = CI(1) + CI(2) * T + CI(3) * T**2
!
!   W     :  THE LONGITUDE OF MARS PERIHELION
!  OM     :  THE LONGITUDE OF MARS ORBIT ASCENDING NODE
!  DL     :  MARS MEAN LONGITUDE
!   T     :  TIME INTERVAL IN JULIAN CENTURY FROM J2000.0
!
!
!*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL (L)
      SAVE
!
      COMMON/ELEMTM/W(6),OM(6),DL(4),CI(3),DPERIM
      DATA W/336.06023398D0,1.841044079D0,1.35117D-4,2.7D-7,            &
     &         3.9639D-10,1.056D-11/
      DATA OM/49.55809305D0,0.772095684D0,1.6069D-5,2.291D-6,           &
     &         1.2D-8,2.794D-10/
      DATA DL/355.433274627D0,19141.697030144D0,3.109719D-3,            &
     &         1.5D-8/
      DATA CI/1.8497265D0, -.601047D-3, 0.12758D-4/
      END
