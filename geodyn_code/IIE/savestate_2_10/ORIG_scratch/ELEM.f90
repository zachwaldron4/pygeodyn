!$ELEM
      SUBROUTINE ELEM(XI,YI,ZI,XDOTI,YDOTI,ZDOTI,AEI,IDRAD,PKPX,GM)
!********1*********2*********3*********4*********5*********6*********7**
! ELEM             00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  TO CONVERT INERTIAL POSITION AND VELOCITY VECTOR
!            TO OSCULATING ORBITAL ELEMENTS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XI       I    S    CARTESIAN X - COORD OF POSITION OF SATELLITE
!   YI       I    S    CARTESIAN Y - COORD OF POSITION OF SATELLITE
!   ZI       I    S    CARTESIAN Z - COORD OF POSITION OF SATELLITE
!   XDOTI    I    S    CARTESIAN X - COORD OF VELOCITY OF SATELLITE
!   YDOTI    I    S    CARTESIAN Y - COORD OF VELOCITY OF SATELLITE
!   ZDOTI    I    S    CARTESIAN Z - COORD OF VELOCITY OF SATELLITE
!   AEI      O    A    KEPLER,KEP5,NON-SINGULAR ELEMENTS
!   (6)
!   IDRAD     I        0 - KEPLER ELEMENTS IN DEGREES
!                      1                   IN RADIANS
!                      2   KEPLER ELEMENTS AND PARTIAL MATRIX
!                          IN DEGREES
!                      3   KEPLER ELEMENTS AND PARTIAL MATRIX
!                          IN RADIANS
!                      4 - KEP5 ELEMENTS IN DEGREES
!                      5 - KEP5 ELEMENTS IN RADIANS
!                      6 - KEP5 ELEMENTS AND PARTIAL MATRIX
!                          IN DEGREES
!                      7 - KEP5 ELEMENTS AND PARTIAL MATRIX
!                          IN RADIANS
!                      8 - NON-SINGULAR ELEMENTS IN DEGREES
!                      9                         IN RADIANS
!                      10- NON-SINGULAR ELEMENTS AND PARTIAL
!                          MATRIX IN DEGREES
!                      11- NON-SINGULAR ELEMENTS AND PARTIAL
!                          MATRIX IN RADIANS
!   PKPX      O   A    PARTIALS OF THE KEPLER ELEMENTS WITH
!   (6,6)              RESPECT TO POSITION AND VELOCITY.
!                      PKPX(I,J) = D K(I) / D X(J)
!   GM        I   S    BODY - CENTRIC CONSTANT OF CENTRAL BODY (USUALLY
!                      THE GEOCENTRIC CONST. FOR EARTH ORBITING
!                      SATELLITES)
!
!
! COMMENTS:
!
! OUTPUT FILES      NONE
! REFERENCES        'GEODYN SYSTEMS DESCRIPTION'
!                   VOLUME 1 - GEODYN DOCUMENTATION
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!CC      LOGICAL LHYPER,HYPSW,LNEVEN,LKEP5,LNONS,LPARTS
      DIMENSION AEI(6),AEINPM(6),PKPX(6,6),XYZXYZ(6),                   &
     &PH(3,6),CUXYZP(3)
!
      COMMON/CELEMX/TRUE,ECC
      COMMON/CITERM/MAXINR,MININR,MAXLST,IHYPSW,NXITER
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
         EQUIVALENCE (A,AEINPM(1)),(E, AEINPM(2)),(RINCL,AEINPM(3)),    &
     &   (RNODE,AEINPM(4)),(OMEGA,AEINPM(5)),(RMEAN,AEINPM(6)),         &
     &   (X,XYZXYZ(1)),(Y,XYZXYZ(2)),(Z,XYZXYZ(3)),                     &
     &   (XDOT,XYZXYZ(4)),(YDOT,XYZXYZ(5)),(ZDOT,XYZXYZ(6))
      DATA ZERO/0.D0/,ONE/1.D0/,TWO/2.D0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      LNEVEN=MOD(IDRAD,2).EQ.0
      LNONS=IDRAD.GE.8
      LKEP5=IDRAD.GE.4.AND.IDRAD.LE.8
      LPARTS=MOD(IDRAD,4)/2.EQ.1
!
      SCALE=ONE
      IF(LNEVEN) SCALE=ONE/DEGRAD
      XYZXYZ(1)=XI
      XYZXYZ(2)=YI
      XYZXYZ(3)=ZI
      XYZXYZ(4)=XDOTI
      XYZXYZ(5)=YDOTI
      XYZXYZ(6)=ZDOTI
         RRDOT=X*XDOT+Y*YDOT+Z*ZDOT
         R=SQRT(X**2+Y**2+Z**2)
         VSQ=XDOT**2+YDOT**2+ZDOT**2
         AINV=TWO/R-VSQ/GM
! COMPUTE SEMI-MAJOR AXIS
         A=ONE/AINV
!         WRITE(6,*) 'ELEM: A', A
         HX=Y*ZDOT-Z*YDOT
         HY=Z*XDOT-X*ZDOT
         HZ=X*YDOT-Y*XDOT
         HSINI2=HX**2+HY**2
         HSQ=HSINI2+HZ**2
         H=SQRT(HSQ)
!         WRITE(6,*) 'ELEM: H', H
         OME2=HSQ*AINV/GM
! COMPUTE ECCENTRICITY
         ESQ=ONE-OME2
         E=SQRT(ESQ)
      TEMPE=E
         LHYPER=ESQ.GE.ONE
      IF(LHYPER.AND.(IHYPSW.NE.1)) GO TO 500
         COSI=HZ/H
!         WRITE(6,*) 'ELEM: HZ, H, COSI ', HZ, H, COSI
         SINI2=ONE-COSI**2
         SINI=SQRT(SINI2)
!         WRITE(6,*) 'ELEM: SINI ',  SINI
         HSINI=H*SINI
! COMPUTE INCLINATION
         RINCL=ATAN2(SINI,COSI)
!         WRITE(6,*) 'ELEM: RINCL ', RINCL
         IF (RINCL.LT.ZERO) RINCL=RINCL+TWOPI
!         WRITE(6,*) 'ELEM: RINCL ', RINCL
! COMPUTE LONGITUDE OF ASCENDING NODE
         RNODE=ATAN2(HX,-HY)
         IF (RNODE.LT.ZERO) RNODE=RNODE+TWOPI
         SUPROD=-HZ*(X*HX+Y*HY)+Z*HSINI2
         CUPROD=H*(-X*HY+Y*HX)
!         WRITE(6,*) 'ELEM: SUPROD ', SUPROD
!         WRITE(6,*) 'ELEM: CUPROD ', CUPROD
         RESINF=A*OME2*RRDOT/H
         RECOSF=A*OME2-R
!   COMPUTE TRUE ANOMALY
         TRUE=ATAN2(RESINF,RECOSF)
!         WRITE(6,*) 'ELEM: TRUE ', TRUE
         IF (TRUE.LT.ZERO) TRUE=TRUE+TWOPI
!         WRITE(6,*) 'ELEM: TRUE ', TRUE
      SURECF=SUPROD*RECOSF
      CURECF=CUPROD*RECOSF
      CURESF=CUPROD*RESINF
      SURESF=SUPROD*RESINF
!      WRITE(6,*) 'ELEM: SUPROD, CUPROD ', SUPROD, CUPROD
!      WRITE(6,*) 'ELEM: RECOSF, RESINF ', RECOSF, RESINF
!      WRITE(6,*) 'ELEM: SURECF, CURECF ', SURECF, CURECF
!      WRITE(6,*) 'ELEM: SURESF, CURESF ', SURESF, CURESF
!      WRITE(6,*) 'ELEM: CCC ', CCC
!      WRITE(6,*) 'ELEM: SWPROD, CWPROD ', SWPROD, CWPROD
      SWPROD=(SURECF-CURESF)
      CWPROD=(CURECF+SURESF)
! COMPUTE ARGUMENT OF PERIGEE
                                                       !jjm 9/98
      if( SWPROD.eq.0.0D0 .and. CWPROD.eq.0.0D0 ) then
         OMEGA= 0.0D0
                                  ! jjm 9/98
      else
         OMEGA=ATAN2(SWPROD,CWPROD)
                                ! jjm 9/98
      endif
!         WRITE(6,*) 'ELEM: OMEGA ', OMEGA
         IF (OMEGA.LT.ZERO) OMEGA=OMEGA+TWOPI
!         WRITE(6,*) 'ELEM: OMEGA ', OMEGA
         RTOME2=SQRT(ABS(OME2))
         AESINE=RESINF/RTOME2
         AECOSE=A-R
!
         IF (LHYPER) GO TO 20
!
!   COMPUTE ECCENTRIC ANOMALY
         ECC=ATAN2(AESINE,AECOSE)
         IF (ECC.LT.ZERO) ECC=ECC+TWOPI
! COMPUTE MEAN ANOMALY
         RMEAN=ECC-AINV*AESINE
         GO TO 30
!
   20 AESINE=-AESINE
!  COMPUTE ECCENTRIC/MEAN ANOMALY FOR HYPERBOLIC ORBIT
      ECC=LOG((AESINE+AECOSE)/(A*E))
         RMEAN=AESINE*AINV-ECC
!
   30 CONTINUE
         IF (RMEAN.LT.ZERO) RMEAN=RMEAN+TWOPI
      IF(RMEAN.GE.TWOPI) RMEAN=MOD(RMEAN,TWOPI)
      AEI(1)=AEINPM(1)
      AEI(2)=AEINPM(2)
      AEI(3)=AEINPM(3)*SCALE
      AEI(4)=AEINPM(4)*SCALE
      AEI(5)=AEINPM(5)*SCALE
      AEI(6)=AEINPM(6)*SCALE
!
      IF(.NOT.LKEP5) GO TO 888
      AEI(5)=AEI(5)+AEI(6)
      IF (LNEVEN) GO TO 88
      AEI(5)=MOD(AEI(5),TWOPI)
      GO TO 888
   88 AEI(5)=MOD(AEI(5),360.0D0)
  888 CONTINUE
!
      IF((.NOT.LNONS).AND.(.NOT.LPARTS)) RETURN
      IF(.NOT.LNONS) GO TO 111
!  COMPUTE NON-SINGULAR KEPLER ELEMENTS
      HALFI=0.5D0*RINCL
      SINHAF=SIN(HALFI)
      COSHAF=SQRT(ONE-SINHAF*SINHAF)
      SINN=HX/HSINI
      COSN=-HY/HSINI
      CSPROD=SQRT(CWPROD*CWPROD+SWPROD*SWPROD)
      SOMEGA=SWPROD/CSPROD
      COMEGA=CWPROD/CSPROD
      COSON=COMEGA*COSN-SOMEGA*SINN
      SINON=SOMEGA*COSN+COMEGA*SINN
      RINCL=E*SINON
      E=E*COSON
      RMEAN=RMEAN+OMEGA+RNODE
      IF(RMEAN.LT.ZERO) RMEAN=RMEAN+TWOPI
      IF(RMEAN.GE.TWOPI) RMEAN=MOD(RMEAN,TWOPI)
      RNODE=SINHAF*SINN
      OMEGA=SINHAF*COSN
      DO 44 I=1,5
   44 AEI(I)=AEINPM(I)
      AEI(6)=AEINPM(6)*SCALE
      IF(.NOT.LPARTS) RETURN
  111 CONTINUE
!  COMPUTE PARTIALS OF KEPLER ELEMENTS W/R TO CARTESIAN ELEMENTS
         C1=TWO*A**2
         CA1=C1/R**3
         CA2=C1/GM
         CH1=HX/H
         CH2=HY/H
         CH3=HZ/H
      EPROD=OME2/TEMPE
         CE1=EPROD/(A+A)
         CE2=-EPROD/H
         CN1=-HY/HSINI2
         CN2=HX/HSINI2
         CU5=R*HSQ*SINI
         COSU=CUPROD/CU5
         HCOSU=H*COSU
         HZCOSU=HZ*COSU
         SINU=SUPROD/CU5
         HSINU=H*SINU
         CU1=-Y*HSINU-X*HZCOSU
         CU2=X*HSINU-Y*HZCOSU
         CU3=-(X*HX+Y*HY+Z*HZ)*COSU-Z*HZCOSU
         CU4=TWO*Z*HCOSU+(X*HY-Y*HX)*SINU
         CUXYZP(1)=HY*HSINU-HX*HZCOSU
         CUXYZP(2)=-HX*HSINU-HY*HZCOSU
         CUXYZP(3)=H*HCOSU-HZ*HZCOSU
         ADR=A/R
      RE=R*TEMPE
         COSF=RECOSF/RE
         SINF=RESINF/RE
         C1=COSF*RRDOT/H
         C2=C1-SINF
         CF1=OME2*C2/RE
         CF2=-(C2+C2)*ADR
         C2=-CE2*ADR
         CF3=-C2*C1
         CF4=C2*COSF
         CF5=SINF/(RE*R)
         RDA=R*AINV
         CM1=RDA**2/RTOME2
      CM2=-(RDA/OME2+ONE)*AESINE/(A*TEMPE)
      IF(LHYPER) CM2=-CM2
!
         PH(1,1)=ZERO
         PH(2,1)=-ZDOT
         PH(3,1)=YDOT
         PH(1,2)=ZDOT
         PH(2,2)=ZERO
         PH(3,2)=-XDOT
         PH(1,3)=-YDOT
         PH(2,3)=XDOT
         PH(3,3)=ZERO
         PH(1,4)=ZERO
         PH(2,4)=Z
         PH(3,4)=-Y
         PH(1,5)=-Z
         PH(2,5)=ZERO
         PH(3,5)=X
         PH(1,6)=Y
         PH(2,6)=-X
         PH(3,6)=ZERO
!
         DO 100 I=1,3
         PKPX(1,I)=CA1*XYZXYZ(I)
         PKPX(1,I+3)=CA2*XYZXYZ(I+3)
         PPFS=CF5*XYZXYZ(I)
         PPUS=CUXYZP(I)
         JFLIP=I+3
!
         DO 100 J=I,6,3
         PHPS=CH1*PH(1,J)+CH2*PH(2,J)+CH3*PH(3,J)
         PKPX(2,J)=CE1*PKPX(1,J)+CE2*PHPS
      PKPX(3,J)=((COSI*PHPS-PH(3,J))/HSINI)*SCALE
      PKPX(4,J)=(CN1*PH(1,J)+CN2*PH(2,J))*SCALE
         PUPS=(CU1*PH(1,J)+CU2*PH(2,J)+CU3*PH(3,J)+CU4*PHPS+PPUS)/CU5
         PFPS=CF1*PKPX(1,J)+CF2*PKPX(2,J)+CF3*PHPS+CF4*XYZXYZ(JFLIP)+   &
     &        PPFS
      PKPX(5,J)=(PUPS-PFPS)*SCALE
      PKPX(6,J)=(CM1*PFPS+CM2*PKPX(2,J))*SCALE
         PPFS=ZERO
         PPUS=ZERO
  100    JFLIP=I
!
      IF(.NOT.LKEP5) GO TO 999
      DO 9 J=1,6
    9 PKPX(5,J)=PKPX(5,J)+PKPX(6,J)
  999 CONTINUE
!
      IF(.NOT.LNONS) GO TO 333
!  COMPUTE PARTIALS OF NONSINGULAR KEPLER ELEMENTS W/R TO CARTESIAN ELEM
      SCALE2=ONE
      IF(LNEVEN) SCALE2=DEGRAD
      DO 55 J=1,6
      C2J=PKPX(2,J)
      C3J=PKPX(3,J)*SCALE2
      C4J=PKPX(4,J)*SCALE2
      C5J=PKPX(5,J)*SCALE2
      C6J=PKPX(6,J)*SCALE2
      PKPX(2,J)=C2J*COSON-RINCL*(C4J+C5J)
      PKPX(3,J)=C2J*SINON+E*(C4J+C5J)
      PKPX(4,J)=0.5D0*COSHAF*C3J*SINN+COSN*C4J*SINHAF
      PKPX(5,J)=0.5D0*COSHAF*C3J*COSN-SINN*C4J*SINHAF
      PKPX(6,J)=(C4J+C5J+C6J)*SCALE
   55 END DO
  333 CONTINUE
      RETURN
  500 WRITE(IOUT6,10500)
      STOP
10500 FORMAT(' ** ELEM **  CARTESIAN SPACECRAFT COORDINATES EQUIVALENT',&
     &   ' TO HYPERBOLIC TRAJECTORY.'/13X,'EXECUTION TERMINATING.'/)
      END
