!$PDPNCN
      SUBROUTINE PDPNCN( NMOON, MJDSEC, FSEC,                           &
     &                   ALPHA, ADOT, DELTA, DDOT, W, WDOT )
!********1*********2*********3*********4*********5*********6*********7**
! PDPNCN           00/00/00            96xx.0    PGMR - J MCCARTHY
!
!  FUNCTION:     DETERMINE ELEMENTS OF PRECESSION SINCE 2000.0
!                FOR THE MOONS OF MARS -- PHOBOS, DEIMOS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NMOON    I    S    MOON NUMBER - PHOBOS=1  DEIMOS=2
!   MJDSEC   I    S    TIME IN MJD seconds from Geodyn Reference Time
!   FSEC     I    S    fractional MJD seconds
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
!             of the Planets and Satellites: 1991", M.E. Davies
!             et al., Celestial Mechanics and Dynamic Astronomy,
!             v. 53, pp. 377-397, 1992
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

      PARAMETER ( ZERO = 0.D0 )
      PARAMETER ( ONE = 1.D0 )
      PARAMETER ( TWO = 2.D0 )
      PARAMETER ( THREE = 3.D0 )
      PARAMETER ( DAYSEC = 86400.D0 )

      DIMENSION  ACON(2,3), DCON(2,3), WCON(2,5)
      DIMENSION  XMCON(3,3)
!  WORKING ARRAY
      DIMENSION  A(3,3),B(3,3),C(3,3)
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CPDTO2/PDTOJ2(3,3)
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
!
      DATA  LFIRST /.FALSE./
!
      DATA  XMCON(1,1)/+169.51D0/
      DATA  XMCON(1,2)/-0.4357640D0/
      DATA  XMCON(1,3)/+0.000D0/
!
      DATA  XMCON(2,1)/+192.93D0/
      DATA  XMCON(2,2)/+1128.4096700D0/
      DATA  XMCON(2,3)/+8.864D0/
!
      DATA  XMCON(3,1)/+53.47D0/
      DATA  XMCON(3,2)/-0.0181510D0/
      DATA  XMCON(3,3)/+0.000D0/
!
      DATA  ACON(1,1)/+317.68D0/
      DATA  ACON(1,2)/-0.108D0/
      DATA  ACON(1,3)/+1.79D0/
!
      DATA  ACON(2,1)/+316.65D0/
      DATA  ACON(2,2)/-0.108D0/
      DATA  ACON(2,3)/+2.98D0/
!
      DATA  DCON(1,1)/+52.90D0/
      DATA  DCON(1,2)/-0.061D0/
      DATA  DCON(1,3)/-1.08D0/
!
      DATA  DCON(2,1)/+53.52D0/
      DATA  DCON(2,2)/-0.061D0/
      DATA  DCON(2,3)/+1.78D0/
!
      DATA  WCON(1,1)/+35.06D0/
      DATA  WCON(1,2)/+1128.8445850D0/
      DATA  WCON(1,3)/+8.864D0/
      DATA  WCON(1,4)/-1.42D0/
      DATA  WCON(1,5)/-0.78D0/
!
      DATA  WCON(2,1)/+79.41D0/
      DATA  WCON(2,2)/+285.1618970D0/
      DATA  WCON(2,3)/-0.52D0/
      DATA  WCON(2,4)/-2.58D0/
      DATA  WCON(2,5)/+0.19D0/
!
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!
      days = DBLE( mjdsec ) / DAYSEC + TMGDN2 - 51544.50D0
!CC   days = float( mjdsec ) / DAYSEC  - 21544.5D0
      days = days + fsec / DAYSEC

      Tcent = days / 36525.D0
      Tcent2 = Tcent**2

      DdDt = ONE / DAYSEC
      DTDt = DdDt / 36525.D0


      if( NMOON .eq. 1 ) then

!        ....PHOBOS

!         XM1 = 169.51D0 - 0.4357640D0 * days
         XM1 = XMCON(1,1) + XMCON(1,2) * days
         XM1 = XM1 * DEGRAD
         DXM1Dt = XMCON(1,2) * DdDt

!         XM2 = 192.93D0 + 1128.4096700D0 * days + 8.864D0 * Tcent2
         XM2 = XMCON(2,1) + XMCON(2,2) * days + XMCON(2,3) * Tcent2
         XM2 = XM2 * DEGRAD
         DXM2Dt = XMCON(2,2) * DdDt + TWO * XMCON(2,3) * Tcent *DTDt

!        ....CALCULATE ALPHA

         ALPHA = ACON(1,1) + ACON(1,2) * Tcent + ACON(1,3) * SIN( XM1 )
         ALPHA = ALPHA * DEGRAD

         ADOT =  ACON(1,2) * DTDt  + ACON(1,3) * COS( XM1 ) * DXM1Dt
         ADOT = ADOT * DEGRAD

!        ....CALCULATE DELTA

         DELTA = DCON(1,1) + DCON(1,2) * Tcent + DCON(1,3) * COS( XM1 )
         DELTA = DELTA * DEGRAD

         DDOT = DCON(1,2) * DTDt  - DCON(1,3) * SIN( XM1 ) * DXM1Dt
         DDOT = DDOT * DEGRAD

!        ....CALCULATE W AND WDOT

         WRATE = + WCON(1,2) * days  + WCON(1,3) * Tcent2               &
     &                 + WCON(1,4) * SIN( XM1 )                         &
     &                 + WCON(1,5) * SIN( XM2 )
         WRATE=MOD(WRATE,360.D0)
         IF(WRATE.LT.0.D0)WRATE=360+WRATE
         W = WCON(1,1) + WRATE
         W = MOD(W,360.D0)
         W = W * DEGRAD

         WDOT  =  WCON(1,2) * DdDt  + TWO * WCON(1,3) * Tcent * DTDt    &
     &                 - WCON(1,4) * COS( XM1 ) * DXM1Dt                &
     &                 - WCON(1,5) * COS( XM2 ) * DXM2Dt
         WDOT = WDOT * DEGRAD

      else

!        ....DEIMOS

!         XM3 = 53.47D0 - 0.0181510D0 * days
         XM3 = XMCON(3,1) + XMCON(3,2) * days
         XM3 = XM3 * DEGRAD

         DXM3Dt = XMCON(3,2) * DdDt

!        ....CALCULATE ALPHA

         ALPHA = ACON(2,1) + ACON(2,2) * Tcent + ACON(2,3) * SIN( XM3 )
         ALPHA = ALPHA * DEGRAD

         ADOT =  ACON(2,2) * DTDt  + ACON(2,3) * COS( XM3 ) * DXM3Dt
         ADOT = ADOT * DEGRAD

!        ....CALCULATE DELTA

         DELTA = DCON(2,1) + DCON(2,2) * Tcent + DCON(2,3) * COS( XM3 )
         DELTA = DELTA * DEGRAD

         DDOT = DCON(2,2) * DTDt  - DCON(2,3) * SIN( XM3 ) * DXM3Dt
         DDOT = DDOT * DEGRAD

!        ....CALCULATE W AND WDOT

         WRATE = + WCON(2,2) * days  + WCON(2,3) * Tcent2               &
     &                 + WCON(2,4) * SIN( XM3 )                         &
     &                 + WCON(2,5) * COS( XM3 )
         WRATE=MOD(WRATE,360.D0)
         IF(WRATE.LT.0.D0)WRATE=360+WRATE
         W = WCON(2,1) + WRATE
         W = MOD(W,360.D0)
         W = W * DEGRAD

         WDOT  =  WCON(2,2) * DdDt  + TWO * WCON(2,3) * Tcent * DTDt    &
     &                 + (    WCON(2,4) * COS( XM3 )                    &
     &                      - WCON(2,5) * SIN( XM3 )  ) * DXM3Dt
         WDOT = WDOT * DEGRAD

      endif

!
! MAKE A MATRICS for converting a vector in Phobos(Deimos) body-fixed
!                system to J2000 system:COMMON/CPDTO2/PDTOJ2(3,3)
!
         WNEG=-W
         CALL ROTAT(WNEG,A,3)
         DD=DELTA-PI/2.D0
         CALL ROTAT(DD,B,1)
         CALL MULTI(B,A,C,3,3,3)
         AP90=-(ALPHA+PI/2.D0)
         CALL ROTAT(AP90,A,3)
         CALL MULTI(A,C,PDTOJ2,3,3,3)
!         PRINT *,'PDPNCN..test.PDTOJ2',PDTOJ2
!
!
      IF( LFIRST) THEN
         WRITE(6,*) ' '
         WRITE(6,*) 'PDPNCN: NMOON  ', NMOON
         WRITE(6,*) 'PDPNCN: MJDSEC,FSEC  ', mjdsec,fsec
         OUTXX = ALPHA / DEGRAD
         WRITE(6,*) 'PDPNCN: ALPHA   ',                                 &
     &                       ALPHA,' RAD   ', OUTXX, ' DEGREES '
         OUTXX = ADOT / DEGRAD
         WRITE(6,*) 'PDPNCN: ADOT   ',                                  &
     &                       ADOT,' RAD/SEC   ', OUTXX, ' DEGREES/SEC '
         OUTXX = DELTA / DEGRAD
         WRITE(6,*) 'PDPNCN: DELTA   ',                                 &
     &                       DELTA,' RAD   ', OUTXX, ' DEGREES '
         OUTXX = DDOT / DEGRAD
         WRITE(6,*) 'PDPNCN: DDOT   ',                                  &
     &                       DDOT,' RAD/SEC   ', OUTXX, ' DEGREES/SEC '
         OUTXX = W / DEGRAD
         WRITE(6,*) 'PDPNCN: W   ',                                     &
     &                       W,' RAD   ', OUTXX, ' DEGREES '
         OUTXX = WDOT / DEGRAD
         WRITE(6,*) 'PDPNCN: WDOT   ',                                  &
     &                       WDOT,' RAD/SEC   ', OUTXX, ' DEGREES/SEC '
         WRITE(6,*) 'PDPNCN: PDTOJ2 ',PDTOJ2
         WRITE(6,*) ' '
         LFIRST = .FALSE.
      ENDIF
!
      RETURN
      END
