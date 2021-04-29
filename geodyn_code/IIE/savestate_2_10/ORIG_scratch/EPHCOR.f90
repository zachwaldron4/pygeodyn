!$EPHCOR
      SUBROUTINE EPHCOR(X0,X,T,GMS,GMP,DXDEBC)
!***********************************************************************
!
! VERSION           - 9201.0  DATE  01.17.91        LUCIA TSAOUSSI
!
! FUNCTION          - COMPUTE THE PARTIAL DERIVATIVES OF THE PLANET
!                     STATE VECTOR WITH RESPECT TO THE BROUWER-CLEMENCE
!                     SET III ORBITAL ELEMENTS. THE ELEMENTS THEMSELVES
!                     ARE COMPUTED ONCE AT EPOCH TIME, WHICH IS CHOSEN
!                     TO BE THE EARLIEST OF ALL SATELLITE EPOCHS.
!
! INPUT PARAMETERS
!                     X0 = PLANET STATE VECTOR AT OSCULATING EPOCH T0
!                          AT J2000.0 SYSTEM (EPHEMERIS OPTION)
!                     X  = PLANET STATE VECTOR AT TIME T AFTER THE EPOCH
!                          AT J2000.0 SYSTEM (EPHEMERIS OPTION)
!                     T  = TIME AFTER EPOCH IN SECONDS
!                     GMS = GM OF SUN
!                     GMP = GM OF PLANET
!
! OUTPUT PARAMETERS - DXDEBC(I,J) = PARTIAL MATRIX DX(I)/DEBC(J)
!                     WHERE EBC = ELLIPTICAL ORBITAL ELEMENTS :
!                           BROUWER AND CLEMENCE SET III
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

!  COMMON/CONSTR/ PI, TWOPI, DEGRAD, SECDEG, SECDAY
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/COREPH/EMJDS0(2),EFSEC0(2),EMODE(2),ECORR(2),              &
     &              PSTAE0(12),DEBC(12),SGMEBC(12),BODYID(2),XCOEPH
!
      DIMENSION X(6), X0(6), P(3), Q(3), W(3)
      DIMENSION  DXDEBC(6,6), ACC(3)
      DATA ZERO/0.0D0/
      DATA ONE/1.0D0/, TWO/2.D0/, X1O2/0.5D0/, X3O2/1.5D0/
!
!***********************************************************************
!   START OF EXECUTABLE CODE
!***********************************************************************
!
!     WRITE(6,*) 'EPHCOR: X0 ', X0
!     WRITE(6,*) 'EPHCOR: X ', X
!     WRITE(6,*) 'EPHCOR: T ', T
!     WRITE(6,*) 'EPHCOR: GMS, GMP ', GMS, GMP
!
!
!     WRITE(6,*) 'EPHCOR: X0 ', X0
!     ....DO THE FOLLOWING SECTION ONCE AT EPOCH TIME
!
!
      GM = GMS + GMP
      GMRT = SQRT( GM )
!     WRITE(6,*) 'EPHCOR: GM, GMRT ', GM, GMRT
!
!     ....R0, V0 ARE POSITION AND VELOCITY AT EPOCH
!
      R02 =DOTPDT(X0,X0)
      R0 = SQRT( R02 )
      V02 =DOTPDT(X0(4),X0(4))
      V0 = SQRT( V02 )
!     WRITE(6,*) 'EPHCOR: R0, R02  ', R0, R02
!     WRITE(6,*) 'EPHCOR: V0, V02  ', V0, V02
!
!     ....A = SEMI-MAJOR AXIS
!
      A = ONE / ( TWO / R0 - V02 / GM )
!     WRITE(6,*) 'EPHCOR: A, ART  ', A, ART
      ART = SQRT( A )
!     WRITE(6,*) 'EPHCOR: A, ART  ', A, ART
!
!     ....XN = MEAN MOTION
!
      XN = GMRT / ART**3
!     WRITE(6,*) 'EPHCOR: MEAN MOTION XN  ', XN
!
!     ...ECC IS ECCENTRICITY,  E0 IS ECCENTRIC ANOMALY
!
      ECOSE0 = ONE - R0 / A
      ESINE0 =DOTPDT(X0,X0(4)) / GMRT / ART
!     WRITE(6,*) 'EPHCOR: ECOSE0, ESINE0  ', ECOSE0, ESINE0
      ECC2 = ( ECOSE0**2 + ESINE0**2 )
      ECC = SQRT( ECC2 )
      X1ECC2 = (ONE - ECC2)
!     WRITE(6,*) 'EPHCOR: ECC, ECC2  ', ECC, ECC2
      COSE0 = ECOSE0 / ECC
      SINE0 = ESINE0 / ECC
!     WRITE(6,*) 'EPHCOR: COSE0, SINE0  ', COSE0, SINE0
!
!     ....P, Q  AND  W  UNIT VECTORS
!     ....P POINTS FROM FOCUS TO PERIFOCUS
!     ....Q POINTS 90 DEG AHEAD OF P
!     ....W = P X Q
!
!     ....THIS COMPUTATION COMPUTES P AND W AND THEN Q = W X P
!
      F1 = COSE0 / R0
      F2 = ( ART / GMRT ) * SINE0
      F3 = ONE / ( SQRT( X1ECC2 ) * ART * GMRT )
!     WRITE(6,*) 'EPHCOR: F1,F2,F3 ', F1, F2, F3
!
      CALL CROPDT( X0, X0(4), W )
!     WRITE(6,*) 'EPHCOR: W = R X V ', W
!
      DO 10 I=1,3
         P(I) = F1 * X0(I) - F2 * X0(I+3)
         W(I) = F3 * W(I)
   10    CONTINUE
!     WRITE(6,*) 'EPHCOR: P ', P
!     WRITE(6,*) 'EPHCOR: W ', W
!
      CALL CROPDT( W, P, Q )
!     WRITE(6,*) 'EPHCOR: Q ', Q
!
!
!-----------------------------------------------------------------------
!
!     ....COMPUTE PARTIALS OF X AT TIME T WRT KEPLERIAN ELEMENTS
!
      CALL CLEARA(DXDEBC,36)
!
!     ....R, V ARE POSITION AND VELOCITY AT TIME T
!
      R2 = DOTPDT( X, X )
      R = SQRT( R2 )
      V2 = DOTPDT(X(4), X(4) )
      V = SQRT( V2 )
      RRDOT = DOTPDT(X, X(4) )
      RDOT  = RRDOT / R
!
!     WRITE(6,*) 'EPHCOR: T ', T
!     WRITE(6,*) 'EPHCOR: R, R2 AT T ', R, R2
!     WRITE(6,*) 'EPHCOR: V, V2 AT T ', V, V2
!     WRITE(6,*) 'EPHCOR: RRDOT, RDOT AT T ', RRDOT, RDOT
!
      DO 20 I=1,3
         ACC(I) = -GM * X(I) / R / R2
   20    CONTINUE
!     WRITE(6,*) 'EPHCOR: ACC ', ACC
!
      DT3O2 = - X3O2 * T
      H1 = ( R - A * (ONE + ECC2) ) / ( A * ECC * X1ECC2 )
      XK1 = RRDOT * ( ONE + R / ( A * X1ECC2 ) )                        &
     &         / ( ECC * ( A * XN )**2 )
      XNINV = ONE / XN
!     WRITE(6,*) 'EPHCOR: DT3O2, XNINV ', DT3O2, XNINV
!     WRITE(6,*) 'EPHCOR: H1, XK1 ', H1, XK1
!
      DO 30 I=1,3
!
!     ....DX/D(DEL(A)/A)
!
         DXDEBC(I,1) = X(I) + DT3O2 * X(I+3)
!
!     ....DX/D(ECC)
!
         DXDEBC(I,2) = H1 * X(I) + XK1 * X(I+3)
!
!     ....DX/D(DEL(M0)+DEL(W))
!
         DXDEBC(I,3) = X(I+3) * XNINV
!
   30    CONTINUE
!
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,1) ', (DXDEBC(JJJ,1),JJJ=1,3)
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,2) ', (DXDEBC(JJJ,2),JJJ=1,3)
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,3) ', (DXDEBC(JJJ,3),JJJ=1,3)
!
!     ....DX/D( DEL(P) )
!
      CALL CROPDT( P, X, DXDEBC(1,4) )
!
!     ....DX/D( DEL(Q) )
!
      CALL CROPDT(Q, X, DXDEBC(1,5) )
!
!     ....DX/D( ECC*DEL(W) )
!
      CALL CROPDT( W, X, DXDEBC(1,6) )
!
      ECCINV = ONE / ECC
      DO 40 I=1,3
         DXDEBC(I,6) = ( DXDEBC(I,6) - X(I+3) * XNINV ) * ECCINV
   40    CONTINUE
!
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,4) ', (DXDEBC(JJJ,4),JJJ=1,3)
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,5) ', (DXDEBC(JJJ,5),JJJ=1,3)
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,6) ', (DXDEBC(JJJ,6),JJJ=1,3)
!
!     ....POSITION PARTIALS ARE NOW COMPLETED
!
!-----------------------------------------------------------------------
!
!     ....COMPUTE VELOCITY PARTIALS
!
      AOR = A / R
      AINV= ONE/A
      H2 = ONE + AOR * X1ECC2
      H2 = ONE - AOR * H2
      H2 = H2 * RDOT * AINV * ECCINV  / X1ECC2
      XK2 = ( ONE - ( R / A) ) * ECCINV / X1ECC2
!
!     WRITE(6,*) 'EPHCOR: AOR, X1ECC2 ', AOR, X1ECC2
!     WRITE(6,*) 'EPHCOR: H2, XK2 ', H2, XK2
!
      DO 50 I=1,3
!
!     ....DXDOT/D(DEL(A)/A)
!
         DXDEBC(I+3,1) = -X(I+3) * X1O2 + ACC(I) * DT3O2
!
!     ....DXDOT/D(ECC)
!
         DXDEBC(I+3,2) = H2 * X(I) + XK2 * X(I+3)
!
!     ....DXDOT/D(DEL(M0)+DEL(W))
!
         DXDEBC(I+3,3) = ACC(I) * XNINV
!
   50    CONTINUE
!
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,1) ', (DXDEBC(JJJ,1),JJJ=4,6)
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,2) ', (DXDEBC(JJJ,2),JJJ=4,6)
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,3) ', (DXDEBC(JJJ,3),JJJ=4,6)
!
!     ....DXDOT/D( DEL(P) )
!
      CALL CROPDT( P, X(4), DXDEBC(4,4) )
!
!     ....DXDOT/D( DEL(Q) )
!
      CALL CROPDT( Q, X(4), DXDEBC(4,5) )
!
!     ....DXDOT/D( ECC*DEL(W) )
!
      CALL CROPDT( W, X(4), DXDEBC(4,6) )
!
      DO 60 I=1,3
         DXDEBC(I+3,6) = ( DXDEBC(I+3,6) - ACC(I) * XNINV ) * ECCINV
   60   CONTINUE
!
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,4) ', (DXDEBC(JJJ,4),JJJ=4,6)
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,5) ', (DXDEBC(JJJ,5),JJJ=4,6)
!     WRITE(6,*) 'EPHCOR: DXDEBC(I,6) ', (DXDEBC(JJJ,6),JJJ=4,6)
!
      RETURN
      END
