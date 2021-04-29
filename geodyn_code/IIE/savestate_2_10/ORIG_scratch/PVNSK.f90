!$PVNSK
      SUBROUTINE PVNSK(XYZ, RLEI,IDRAD,GM)
!********1*********2*********3*********4*********5*********6*********7**
! PVNSK            00/00/00            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  TRANSFORMS NON SINGULAR KEPLER ELEMENTS INTO
!            CARTESIAN ELEMENTS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XYZ      O         CARTESIAN ELEMENTS(METERS)
!   RLEI     I         NON SINGULAR KEPLER ELEMENTS IN METERS AND
!                      DEGREES OR RADIANS DEPENDING ON IDRAD
!   IDRAD    I         INDICATOR OF ANGULAR UNITS FOR RLEI
!                      1 - DEGREES ; 2 - RADIANS
!   GM       I         UNIVERSAL GRAVITATIONAL CONSTANT TIMES THE
!                      MASS OF THE EARTH
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!
      DIMENSION RLEI(6),XYZ(6),RXQ(9),Q(3),QDOT(3)
!
      DATA C1P0/1.0D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      SCALE=DEGRAD
      IF(IDRAD.EQ.2) SCALE=C1P0
      ICNT=0
      A=RLEI(1)
      ECWN=RLEI(2)
      ESWN=RLEI(3)
      SISN=RLEI(4)
      SICN=RLEI(5)
      RMWN=RLEI(6)*SCALE
      ECWN2=ECWN*ECWN
      ESWN2=ESWN*ESWN
! CALCULATE E+W+N BY NEWTON RAPHSON METHOD
      EWN=RMWN
   50 FEWN=RMWN-EWN+ECWN*SIN(EWN)-ESWN*COS(EWN)
      FEWNP=-1.D0+ECWN*COS(EWN)+ESWN*SIN(EWN)
      EWN2=EWN-FEWN/FEWNP
      ICNT=ICNT+1
      IF(ABS(EWN2-EWN).LE.1.0D-8) GO TO 200
      EWN=EWN2
      IF(ICNT.LT.16) GO TO 50
      WRITE(IOUT6,1000,IOSTAT=IERR) ICNT
  200 EWN=EWN2
      COSEWN=COS(EWN)
      SINEWN=SIN(EWN)
      SISN2=SISN*SISN
      SICN2=SICN*SICN
! COSI=COS(I/2)
      COSI=SQRT(1.0D0-SISN2-SICN2)
! FORM RXQ PRIME ARRAY
      RXQ(1)=1.0D0-2.0D0*SISN2
      RXQ(2)=2.0D0*SISN*SICN
      RXQ(3)=2.0D0*COSI*SISN
      RXQ(4)=RXQ(2)
      RXQ(5)=1.0D0-2.0D0*SICN2
      RXQ(6)=2.0D0*COSI*SICN
      RXQ(7)=-RXQ(3)
      RXQ(8)=RXQ(6)
      RXQ(9)=1.0D0-2.0D0*(SISN2+SICN2)
!  FORM Q PRIME ARRAY
      B=1.0D0/(1.0D0+SQRT(1.0D0-ESWN2-ECWN2))
      BSCWN=B*ESWN*ECWN
      Q(1)=A*((1.0D0-ESWN2*B)*COSEWN+BSCWN*SINEWN-ECWN)
      Q(2)=A*(BSCWN*COSEWN+(1.0D0-ECWN2*B)*SINEWN-ESWN)
      Q(3)=0.0D0
! FORM QDOT PRIME ARRAY
      EWNDOT=SQRT(GM/(A*A*A)) /(1.0D0-ECWN*COSEWN-ESWN*SINEWN)
      AEWNDT=A*EWNDOT
      QDOT(1)=AEWNDT*((ESWN2*B-1.0D0)*SINEWN+BSCWN*COSEWN)
      QDOT(2)=AEWNDT*(-BSCWN*SINEWN+(1.0D0-ECWN2*B)*COSEWN)
      QDOT(3)=0.0D0
! FORM X=RXQ * Q
      XYZ(1)=RXQ(1)*   Q(1)+RXQ(2)*   Q(2)+RXQ(3)*Q(3)
      XYZ(2)=RXQ(4)*   Q(1)+RXQ(5)*   Q(2)+RXQ(6)*Q(3)
      XYZ(3)=RXQ(7)*   Q(1)+RXQ(8)*   Q(2)+RXQ(9)*Q(3)
! FORM XDOT=RXQ * QDOT
      XYZ(4)=RXQ(1)*QDOT(1)+RXQ(2)*QDOT(2)+RXQ(3)*QDOT(3)
      XYZ(5)=RXQ(4)*QDOT(1)+RXQ(5)*QDOT(2)+RXQ(6)*QDOT(3)
      XYZ(6)=RXQ(7)*QDOT(1)+RXQ(8)*QDOT(2)+RXQ(9)*QDOT(3)
      RETURN
!
! FORMATS
!
 1000 FORMAT('0','***** NEWTON RAPHSON ITERATIVE SCHEME HAS NOT ',&
     &'CONVERGED AFTER',I3,' ITERATIONS IN SUBROUTINE PVNSK'/1X )
      END
