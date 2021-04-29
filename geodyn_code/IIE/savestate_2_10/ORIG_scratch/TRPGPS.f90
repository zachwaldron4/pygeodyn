!$TRPGPS
      SUBROUTINE TRPGPS(WETCOR,DRYCOR,SINEL,COSEL,P0,T0,E0)
!********1*********2*********3*********4*********5*********6*********7**
! TROPCO           10/04/91            9110.0    PGMR - JAM
!
!
! FUNCTION:  COMPUTES TROPOSPHERIC CORRECTION USING CHAO MODEL
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   P0       I    S    SURFACE PRESSURE, BAR
!   T0       I    S    SURFACE TEMP., KELVIN
!   E0       I    S    SURFACE WATER VAPOR PRESSURE
!   COSEL    I    S    COSINE OF ELEVATION
!   SINEL    I    S    SINE OF ELEVATION
!   DRYCOR   I/O  S    DRY TROPOSPHERIC REFRACTION CORRECTION
!   WETCOR   I/O  S    WET TROPOSPHERIC REFRACTION CORRECTION
!
!
! COMMENTS:
!      SEE REFERENCE "THE TROPOSPHERIC CALIBRATION MODEL FOR MARINER
!          MARS 1971", CHAO, C.C., JPL TECH. REPORT 32-1587.
!
!      CFA2.2 CODE TAKEN FROM CALC PROGRAM CFACALC
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      DIMENSION CFA22A(6),CFA22B(6)
!
      DATA ZERO/0.0D0/,ONE/1.0D0/
! INITIALIZE CHAO MODEL COEFFICIENTS
      DATA WETA/3.5D-4/,WETB/1.7D-2/,DRYA/1.43D-3/,DRYB/4.45D-2/
! INITIALIZE CFA2.2 MODEL COEFFICIENTS
      DATA CFA22A/                                                      &
     &       0.1185D-2,                                                 &
     &       0.6070D-4,                                                 &
     &      -0.1471D-3,                                                 &
     &       0.3072D-2,                                                 &
     &       0.1965D-1,                                                 &
     &      -0.5645D-2/
      DATA CFA22B/                                                      &
     &       0.1144D-2,                                                 &
     &       0.1164D-4,                                                 &
     &       0.2795D-3,                                                 &
     &       0.3109D-2,                                                 &
     &       0.3038D-1,                                                 &
     &      -0.1217D-1/
      DATA BETA/-5.6D-3/, TROPHT/10.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
! INITIALIZE
! IF MAPPING FUNCTION IS NOT SPECIFIED ON REFRAC CARD, DO NOT CHANGE
! WET OR DRY CORRECTION
      RWET =ONE
      RDRY =ONE
      TANEL = SINEL/COSEL
!
! COMPUTE WET COMPONENT
! CHAO MODEL
        IF(MAPWET(MTYPE).EQ.0) THEN
           RWET = ONE/(SINEL+(WETA/(TANEL+WETB)))
        ENDIF
! CFA2.2 MODEL
! TC - TEMPERATURE IN CENTIGRADE
! TROPHT - HEIGHT OF TROPOSPHERE, IN KM
! DHT - HEIGHT OF TROPOSPHERE RELATIVE TO 11.231 KM
! DP - PRESSURE RELATIVE TO 1000 MBAR
! DT - TEMPERATURE RELATIVE TO 20 DEG C
      IF(MAPWET(MTYPE).EQ.1) THEN
!**********************************************************************
!  THIS CODE CONVERTS RELATIVE HUMIDITY TO WATER VAPOR PRESSURE
!  THIS IS DONE IN METDAT IN GEODYN.  HOWEVER, THE GEODYN AND CALC
!  FORMULATIONS ARE SLIGHTLY DIFFERENT AND I HAVE LEFT THE CALC
!  CODE FOR REFERENCE
!     TEMP = TC+237.3D0   !CHANGED FROM 273.3 ON 9-08-87
!     ESAT = 6.11D0 * EXP(17.269D0*TC/TEMP)
!     ESATDOT = ESAT * (17.269D0/(TEMP) - 17.269D0*TC/(TEMP*TEMP))*TCDOT
!     ENOT = ESAT*RH
!     ENOTDOT = ESATDOT*RH + ESAT*RHDOT
!**********************************************************************
      ENOT = E0
      DP = P0 - 1000.D0
      TC = T0 - 273.16
      DT = TC - 20.D0
      DBETA = BETA*1.D3 + 6.50D0
      DHT = TROPHT - 11.231D0
!
      AM =    CFA22A(1)*(1.0D0 +CFA22A(2)*DP + CFA22A(3)*ENOT           &
     &        + CFA22A(4)*DT + CFA22A(5)*DBETA + CFA22A(6)*DHT)
!
!     AMDOT = A(1) * ( A(2)*PDOT + A(3)*ENOTDOT + A(4)*TCDOT)
!
!
      BM =  CFA22B(1)* (1.0D0 + CFA22B(2)*DP + CFA22B(3)*ENOT           &
     &         + CFA22B(4)*DT + CFA22B(5)*DBETA + CFA22B(6)*DHT)
!
!     BMDOT = B(1) * ( B(2)*PDOT + B(3)*ENOTDOT + B(4)*TCDOT)
!
      CM = -0.0090D0
!     CMDOT = 0.0D0
!
!-----CALCULATE MAPPING FUNCTION.
!
      X1 = SINEL + CM
      X2 = TANEL + BM/X1
      X3 = SINEL + AM/X2
      RWET = 1.0D0/X3
      ENDIF
!
! APPLY MAPPING FUNCTION TO WET CORRECTION
      WETCOR = RWET * WETCOR
!
! COMPUTE DRY COMPONENT
! CHAO MODEL
        IF(MAPDRY(MTYPE).EQ.0) THEN
           RDRY = ONE/(SINEL+(DRYA/(TANEL+DRYB)))
        ENDIF
! CFA2.2 MODEL
! TC - TEMPERATURE IN CENTIGRADE
! TROPHT - HEIGHT OF TROPOSPHERE, IN KM
! DHT - HEIGHT OF TROPOSPHERE RELATIVE TO 11.231 KM
! DP - PRESSURE RELATIVE TO 1000 MBAR
! DT - TEMPERATURE RELATIVE TO 20 DEG C
      IF(MAPDRY(MTYPE).EQ.1) THEN
!**********************************************************************
!  THIS CODE CONVERTS RELATIVE HUMIDITY TO WATER VAPOR PRESSURE
!  THIS IS DONE IN METDAT IN GEODYN.  HOWEVER, THE GEODYN AND CALC
!  FORMULATIONS ARE SLIGHTLY DIFFERENT AND I HAVE LEFT THE CALC
!  CODE FOR REFERENCE
!     TEMP = TC+237.3D0   !CHANGED FROM 273.3 ON 9-08-87
!     ESAT = 6.11D0 * EXP(17.269D0*TC/TEMP)
!     ESATDOT = ESAT * (17.269D0/(TEMP) - 17.269D0*TC/(TEMP*TEMP))*TCDOT
!     ENOT = ESAT*RH
!     ENOTDOT = ESATDOT*RH + ESAT*RHDOT
!**********************************************************************
      ENOT = E0
      DP = P0 - 1000.D0
      TC = T0 - 273.16
      DT = TC - 20.D0
      DBETA = BETA*1.D3 + 6.50D0
      DHT = TROPHT - 11.231D0
!
      AM =    CFA22A(1)*(1.0D0 +CFA22A(2)*DP + CFA22A(3)*ENOT           &
     &        + CFA22A(4)*DT + CFA22A(5)*DBETA + CFA22A(6)*DHT)
!
!     AMDOT = A(1) * ( A(2)*PDOT + A(3)*ENOTDOT + A(4)*TCDOT)
!
!
      BM =  CFA22B(1)* (1.0D0 + CFA22B(2)*DP + CFA22B(3)*ENOT           &
     &         + CFA22B(4)*DT + CFA22B(5)*DBETA + CFA22B(6)*DHT)
!
!     BMDOT = B(1) * ( B(2)*PDOT + B(3)*ENOTDOT + B(4)*TCDOT)
!
      CM = -0.0090D0
!     CMDOT = 0.0D0
!
!-----CALCULATE MAPPING FUNCTION.
!
      X1 = SINEL + CM
      X2 = TANEL + BM/X1
      X3 = SINEL + AM/X2
      RDRY = 1.0D0/X3
      ENDIF
!
! APPLY MAPPING FUNCTION TO WET CORRECTION
      DRYCOR = RDRY * DRYCOR
!
      RETURN
      END
