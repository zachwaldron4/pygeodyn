!$NLUNCN
      SUBROUTINE NLUNCN( D,DW,ALPHA, ADOT, DELTA, DDOT, W, WDOT )
!********1*********2*********3*********4*********5*********6*********7**
! NLUNCN           00/00/00            9011.0    PGMR - J MCCARTHY
!                                                PGMR - S. B. LUTHCKE
!
!  FUNCTION:     DETERMINE ELEMENTS OF PRECESSION SINCE 2000.0
!                FOR THE EARTH'S MOON.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   D        I         TIME IN DAYS = -21544.5 (MJD start - J2000)
!   DW       I         TIME IN DAYS BETWEEN INTERPOLATION INTERVAL AND
!                      J2000
!   ALPHA    O         RIGHT ASCENSION OF MOON'S POLE AT TIME SPECIFIED
!   ADOT     O         TIME DERIVATIVE OF ALPHA
!   DELTA    O         DECLINATION OF MOON'S POLE AT TIME SPECIFIED
!   DDOT     O         TIME DERIVATIVE OF DELTA
!   W        O         ANGLE OF PRIME MERIDIAN
!   WDOT     O         TIME DERIVATIVE OF THE ANGLE OF PRIME MERIDIAN
!
! Reference : Table II, Page 191,
!             "Report of the IAU/IAG/COSPAR Working Group on
!             Cartographic Coordinates and Rotational Elements
!             of the Planets and Satellites: 1988", M.E. Davies
!             et al., Celestial Mechanics and Dynamic Astronomy,
!             v. 46, pp. 187-204, 1989
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION ECON(2,5), E(5), EDOT(5), ACON(5), DCON(5), WCON(7)
      DIMENSION SINE(5), COSE(5)
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
!
      DATA ONE/1.D0/,TWO/2.D0/,THREE/3.D0/,DSEC/86400.D0/
      DATA ZERO/0.0D0/
!
      DATA  ECON(1,1)/+125.045D0/,  ECON(2,1)/-0.052992D0/
!CCCCCDATA  ECON(1,2)/+249.390D0/,  ECON(2,2)/-0.105984D0/
      DATA  ECON(1,2)/+250.090D0/,  ECON(2,2)/-0.105984D0/
!CCCCCDATA  ECON(1,3)/+196.694D0/,  ECON(2,3)/-13.012000D0/
      DATA  ECON(1,3)/+260.008D0/,  ECON(2,3)/+13.012001D0/
!CCCCCDATA  ECON(1,4)/+176.630D0/,  ECON(2,4)/+13.340716D0/
      DATA  ECON(1,4)/+176.625D0/,  ECON(2,4)/+13.340716D0/
!CCCCCDATA  ECON(1,5)/+358.219D0/,  ECON(2,5)/-0.985600D0/
      DATA  ECON(1,5)/+357.529D0/,  ECON(2,5)/+0.985600D0/
!
      DATA  ACON(1)/+270.000D0/,   ACON(2)/-3.878D0/
      DATA  ACON(3)/-0.120D0/,     ACON(4)/+0.070D0/
      DATA  ACON(5)/-0.017D0/
!
!CCCCCDATA  DCON(1)/+66.534D0/,    DCON(2)/+1.543D0/
      DATA  DCON(1)/+66.541D0/,    DCON(2)/+1.543D0/
      DATA  DCON(3)/+0.024D0/,     DCON(4)/-0.028D0/
      DATA  DCON(5)/+0.007D0/
!
!CCCCCDATA  WCON(1)/+38.314D0/,    WCON(2)/+13.1763581D0/
      DATA  WCON(1)/+38.317D0/,    WCON(2)/+13.1763582D0/
      DATA  WCON(3)/+3.558D0/,     WCON(4)/+0.121D0/
      DATA  WCON(5)/-0.064D0/,     WCON(6)/+0.016D0/
      DATA  WCON(7)/+0.025D0/
!
!     ADT IN RADIANS PER CENT IS .003
      DATA ADT/.8213552361D-7/
!     DDT IN RADIANS PER CENT IS .013
      DATA DDT/.3559206023D-6/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!
!
!
!     ....CALCULATE E VALUES
!
      DO 10 I=1,5
         E(I) = ECON(1,I) + ECON(2,I) * D
         E(I) = E(I) * DEGRAD
         EDOT(I) = ECON(2,I) * DEGRAD / DSEC
         SINE(I) = SIN( E(I) )
         COSE(I) = COS( E(I) )
   10 END DO
!
!     ....CALCULATE ALPHA
!
      ALPHA = (ACON(1)+D*ADT)*DEGRAD
      ADOT = ADT*DEGRAD/DSEC
      DO 20 I=1,4
         ALPHA = ALPHA + SINE(I) * (ACON(I+1)*DEGRAD)
         ADOT  = ADOT  + COSE(I) * (ACON(I+1)*DEGRAD) * EDOT(I)
   20 END DO
!
!     ....CALCULATE DELTA
!
      DELTA = (DCON(1)+D*DDT)*DEGRAD
      DDOT = DDT*DEGRAD/DSEC
      DO 30 I=1,4
         DELTA = DELTA + COSE(I) * (DCON(I+1)*DEGRAD)
         DDOT  = DDOT  - SINE(I) * (DCON(I+1)*DEGRAD) * EDOT(I)
   30 END DO
!
!     ....CALCULATE E VALUES
!
      DO 35 I=1,5
         E(I) = ECON(1,I) + ECON(2,I) * DW
         E(I) = E(I) * DEGRAD
         EDOT(I) = ECON(2,I) * DEGRAD / DSEC
         SINE(I) = SIN( E(I) )
         COSE(I) = COS( E(I) )
   35 END DO
!
!     ....CALCULATE W AND WDOT
!
      W     = (WCON(1)*DEGRAD) + (WCON(2)*DEGRAD) * DW
      WDOT  =           WCON(2)*DEGRAD / DSEC
      DO 40 I=1,5
         W    = W    + SINE(I) * (WCON(I+2)*DEGRAD)
         WDOT = WDOT + COSE(I) * (WCON(I+2)*DEGRAD) * EDOT(I)
   40 END DO
!
      RETURN
      END
