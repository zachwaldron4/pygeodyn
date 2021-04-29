!$GDELRB
      SUBROUTINE GDELRB(SSOF,SOF,PK,DAY,CHL,SHL,C2HL,S2HL,C3HL,         &
     &S3HL,A,GDEL,FLAG,P,MJDSEC)
!********1*********2*********3*********4*********5*********6*********7**
! GDELRB           00/00/00    8912.0            PGMR - UTOPIA CODE
!
! FUNCTION:  COMPUTE THE DENSITY FUNCTION IN TERMS OF SPHERICAL
!            HARMONIC COEFFICIENTS REPRESENTING DIFFERENT CONSTITUENTS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   SSOF     I    S    AVERAGE SOLAR FLUX VALUES WHERE AVERAGING IS
!                      PERFORMED OVER 3 SOLAR CYCLES
!   SOF      I    S    SOLAR FLUX VALUES FROM THE PRECEDING DAY
!   KP       I    S    3 HOURLY MAGNETIC FLUX KP VALUES
!   DAY      I    S    DAY OF THE YEAR
!   CHL      I    S    THE COSINE OF HL(LOCAL HOUR)
!   SHL      I    S    THE SINE OF HL(LOCAL HOUR)
!   C2HL     I    S    THE COSINE OF 2*HL
!   S2HL     I    S    THE SINE OF 2*HL
!   C3HL     I    S    THE COSINE OF 3*HL
!   S3HL     I    S    THE SINE OF 3*HL
!   A        I    A    TABULAR SPHERICAL HARMONIC COEFFICIENTS FOR
!                      THERMOPAUSE TEMPERATURE FOR EACH OF THE
!                      CONSTITUENTS
!   GDEL     O    S    COMPUTED DENSITY FUNCTION, OR PARTIAL OF DENSITY
!                      FUNCTION WRT LATITUDE
!                      TEMPERATURE OF EACH CONSTITUENT
!   FLAG     I    S    FLAG =1 FOR ATOMIC OXYGEN,NYTROGEN AND
!                              TEMPERATURE
!                      FLAG =0 FOR HELIUM
!   P        I    A    EVALUATED LEGENDRE POLYNOMIALS
!   MJDSEC   I    S    INTEGER ET SECONDS SINCE GEODYN REF. TIME
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/DTMPOL/P10,P20,P30,P40,P50,P11,P21,P31,P51,P22,P32,P33
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      DIMENSION A(36),P(12)
      DATA RADYR/0.17202791D-01/
      DATA XJD0/2443509.5D0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! FOR SATELLITES EARLIER THAN JANUARY 1 1978 THE ORIGINAL COMPUTATION
! FOR THE DAY OF THE YEAR WILL BE USED. FOR SATELLITES LATER THAN JAN
! 1 1978 GEODYN USES THE SAME METHOD OF COMPUTATIUONS AS IN UTOPIA.
! THIS WAS AGREED AS PART OF THE TOPEX SOFTWARE INTERCOMPARISON PLAN.
! Jan.1, 0h,1978 =JD 2443509.5 ...luo
!
      XJD=DBLE(MJDSEC)/86400.D0+TMGDN1
      DAY0= XJD0 - TMGDN1
      IF(XJD.LT.XJD0) THEN
         FRACYR=DAY*RADYR
      ELSE
!CC      FRACYR=MOD(DAY-13509.0D0,365.2422D0)*RADYR
         FRACYR=MOD(DAY-DAY0,365.2422D0)*RADYR
      ENDIF
      COSTE= COS(FRACYR-A(18))
      COSTF= COS(FRACYR-A(11))
      COS2E= COS(2.D0*(FRACYR-A(14)))
      COS2F= COS(2.D0*(FRACYR-A(20)))
!
!...DEPENDENCE OF DIRECT SOLAR FLUX
      DELTF= SOF - SSOF
      F0   = A(4)*DELTF + A(5)*DELTF**2 + A(6)*(SSOF-150.D0)
!
!...COMPUTE CONSTITUENT DEPENDENT FACTOR
      BETA = 1.D0 + F0*FLAG
!
!...DEPENDENCE OF ZONAL LATITUDE
      GDEL = 1.D0 + A(2)*P(2) + A(3)*P(4)                               &
     &     +  F0                                                        &
     &     + (A(7) + A(8)*P(2))*PK                                      &
     &     + BETA*( (A(9) + A(10)*P(2))*COSTF                           &
     &     + (A(12) + A(13)*P(2))*COS2E                                 &
     &     + (A(15)*P(1) + A(16)*P(3) + A(17)*P(5))*COSTE               &
     &     + (A(19)*P(1))*COS2F )
!
!...DEPENDENCE OF SOLAR FLUX
!
!...DEPENDENCE OF GEOMAGNETIC EFFECT
!
!...DEPENDENCE OF EVEN (LAT) ANNUAL VARIATION
!
!...DEPENDENCE OF EVEN SEMI-ANNUAL VARIATION
!
!...DEPENDENCE OF ODD ANNUAL VARIATION
!
!...DEPENDENCE OF ODD SEMI-ANNUAL VARIATON
!
!...DEPENDENCE OF DIURNAL VARIATION
      ACH = A(21)*P(6) + A(22)*P(8) + A(23)*P(9)
      BCH = A(26)*P(6) + A(27)*P(8) + A(28)*P(9)
      CCH = A(24)*P(6) + A(25)*P(7)
      DCH = A(29)*P(6) + A(30)*P(7)
!
      GDEL = GDEL + BETA*( (ACH + CCH*COSTE)*CHL                        &
     &                  +  (BCH + DCH*COSTE)*SHL                        &
     &                  +  (A(31)*P(10) + A(32)*P(11)*COSTE)*C2HL       &
     &                  +  (A(33)*P(10) + A(34)*P(11)*COSTE)*S2HL       &
     &                  +   A(35)*P(12)*C3HL                            &
     &                  +   A(36)*P(12)*S3HL )
!
!...DEPENDENCE OF SEMI-DIURNAL VARIATION
!
!...DEPENDENCE OF TERI-DIURNAL VARIATION
!
      RETURN
!
      END
