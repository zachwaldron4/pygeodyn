!$PDGRAV
      SUBROUTINE PDGRAV(KS,PDMASS,XTEMP,COEFZ,COEFC,XM,XDDOT)
!********1*********2*********3*********4*********5*********6*********7**
! PDGRAV           00/00/00            0000.0    PGMR - S.LUO
!
!
! FUNCTION:  EVALUATE PHOBOS(DEIMOS) ACCELERATION DUE
!            TO ITS GRAVITY FIELD ON MARS AS DESCRIBED BY 4X4
!            HARMONICS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   KS       I    S    INDICATOR : 1 = PHOBOS , 2 = DEIMOS
!
!   COEFZ    I    A    PRECALCULATED FACTORS NEEDED FOR
!                      NORMALIZED TESSERAL POLYNOMIALS
!   COEFC    I    A    PRECALCULATED FACTORS NEEDED FOR
!                      NORMALIZED  POLYNOMIALS
!   XDDOT    O    A    ACCELERATION OF SATELLITE IN TRUE OF REF
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
!YBER      IMPLICIT REAL(A-H,O-Z),LOGICAL(L)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CPDTO2/PDTOJ2(3,3)
      COMMON/CREFMT/REFMT(9)
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      COMMON/PDOBGR/PDC(22,2),PDS(22,2)
      DIMENSION D(22),XTEMP(6),TREFMT(3,3)
      DIMENSION P(22),AORN(4),SINLAM(5),AEPD(2),                        &
     &   COSLAM(5),TANPSI(5),C(22),S(22),                               &
     &   XM(NP),EXPRFL(22,6),ACC(3),ACCJ2(3),                           &
     &   XDDOT(3),PDARAY(14),COEFZ(NP),COEFC(NPMAX,*)
! %%%%%%%%%% COEFC(NPMAX,1) -> COEFC(NPMAX,*)
!
      DATA ZERO/0.D0/,ONE/1.D0/
!
!....LOAD Phobos' mean equatorial radius (Deimos =2)
!    Phobos = 11.12km, Deimos =6.25 km
!
      DATA AEPD/11120.D0,6250.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!      PRINT *,'PDGRAV** XM   ',(XM(I),I=1,22)
!      PRINT *,'PDGRAV** COEFZ',(COEFZ(I),I=1,22)
!      PRINT *,'PDGRAV** COEFC',((COEFC(I,J),I=1,22),J=1,3)
!. LOAD THE COEF. TO THE ASSOCIATE POSITION FROM PDC AND PDS
!
      DO 10 I=1,22
       C(I)=PDC(I,KS)*1.D-2
       S(I)=PDS(I,KS)*1.D-2
   10 END DO
!     PRINT *,'PDGRAV*C= ',C
!     PRINT *,'PDGRAV*S= ',S
!
!   GET the GM for Phobos(Deimos).
!
!     PRINT *,'PDGRAV** PDMASS', PDMASS
      PDGM=PDMASS*6.672D-11
      PDAE=AEPD(KS)
!
!.. FILL PDARAY (14) for Phobos(Deimos)
!
      DO 15 I=1,3
      PDARAY(I)=0.D0
      PDARAY(I+11)=0.D0
   15 END DO
      XX= XTEMP(1)
      YY= XTEMP(2)
      ZZ= XTEMP(3)
      RR=XX*XX+YY*YY+ZZ*ZZ
      R=SQRT(RR)
      PDARAY(4)=R
      PDARAY(5)=RR
      PDARAY(6)=RR
      PDARAY(7)=R
      PDARAY(8)=PDGM/R
      PDARAY(9)= R
      PDARAY(10)=0.D0
      PDARAY(11)=0.D0
      PDAOR=PDAE/R
      PDGMR=PDGM/R
!.....
      CALL PDLGEN(COEFZ,COEFC,PDARAY,P,AORN,SINLAM,COSLAM,              &
     &            TANPSI,4,PDGMR,PDAOR )
!     PRINT *, 'PDGRAV*PDAOR,PDGMR,R  ',PDAOR,PDGMR,R
      NPM1=21
      MPT=15
      DO 900 I=1,5
!     PRINT *,'PDGRAV* XM(MPT+I)',XM(MPT+I)
      EXPRFL(I,4)=COSLAM(I)*XM(MPT+I)
      EXPRFL(I,6)=SINLAM(I)*XM(MPT+I)
  900 END DO
      KRA1=0
      KNL=2
      DO 1100 N=1,4
      DEGPSI=ZERO
      DO 1050 KR=1,KNL
      EXPRFL(KR+KRA1,5)=COSLAM(KR)*C(KR+KRA1)+SINLAM(KR)*S(KR+KRA1)
      EXPRFL(KR+KRA1,2)=P(KR+KRA1+1)*COEFC(KR+KRA1,3)                   &
     &                -TANPSI(KR)*P(KR+KRA1)
      D(KR+KRA1)=P(KR+KRA1+1)*COEFC(KR+KRA1,3)                          &
     &                -TANPSI(2)*P(KR+KRA1)*XM(KR+KRA1)
      EXPRFL(KR+KRA1,3)=EXPRFL(KR,4)*S(KR+KRA1)                         &
     &                 -EXPRFL(KR,6)*C(KR+KRA1)
      EXPRFL(KR+KRA1,1)=EXPRFL(KR+KRA1,5)*P(KR+KRA1)
      DEGPSI=DEGPSI+EXPRFL(KR+KRA1,5)*EXPRFL(KR+KRA1,2)
 1050 END DO
      EXPRFL(N,5)=DEGPSI*AORN(N)
      KRA1=KRA1+N+3
      KNL=KNL+1
 1100 END DO
      MPT=MPT+2
      KRA1=0
      KNL=2
      DO 1200 N=1,4
      DEGR=ZERO
      DEGLAM=ZERO
      DO 1150 KR=1,KNL
      DEGR=DEGR+EXPRFL(KR+KRA1,1)
      DEGLAM=DEGLAM+EXPRFL(KR+KRA1,3)*P(KR+KRA1)
 1150 END DO
      EXPRFL(N,4)=-DEGR*AORN(N)*XM(MPT+N)
      EXPRFL(N,6)=DEGLAM*AORN(N)
      KRA1=KRA1+N+3
      KNL=KNL+1
 1200 END DO
      EXPRFL(4,4)=-DEGR*AORN(4)*(XM(MPT+3)+ONE)
!     PRINT *,'PDGRAV*EXPRFL*',((EXPRFL(I,J),I=1,4),J=4,6)
      DO 1300 J=1,3
      PDARAY(J)=ZERO
      JJ=J+3
      DO 1250 JJJ=3,4
 1250 PDARAY(J)=PDARAY(J)+EXPRFL(JJJ,JJ)
      PDARAY(J)=PDARAY(J)+EXPRFL(1,JJ)
      PDARAY(J)=PDARAY(J)+EXPRFL(2,JJ)
 1300 END DO
!     PRINT *,'PDGRAV**PDARAY(1-3)',PDARAY(1),PDARAY(2),PDARAY(3)
 1301 FORMAT(1X,3D20.10)
! ******* DEBUG
 7005 CONTINUE
! ***CONVERT THE (-)ACC. to MARS CENTER INERTIAL SYSTEM
!    THERE IS A SAME VALUE but OPPOSITE SIGN ACC. on Phobos (Or Deimos)
!    from Phobos(Deimos) Gravity field
!
       DO 1400 I=1,3
 1400  ACC(I)=-PDARAY(I)/PDARAY(4)
       ACC(1)=ACC(1)-PDGMR/PDARAY(4)
!
!      PRINT *,'PDGRAV**ACC**',ACC
       CALL MULTI(PDTOJ2,ACC,ACCJ2,3,3,1)
!      PRINT *,'PDGRAV**ACCJ2**',ACCJ2
!
!  TRANSPOSE REFMT to TREFMT(3,3)
!
         K=0
         DO 1500 I=1,3
            DO 1450 J=1,3
            K=K+1
            TREFMT(I,J)=REFMT(K)
 1450  CONTINUE
 1500  CONTINUE
! Debug ..
!      PRINT *,'PDGRAV**TREFMT',((TREFMT(I,J),I=1,3),J=1,3)
       CALL MULTI(TREFMT,ACCJ2,XDDOT,3,3,1)
!
!     PRINT *,'PDGRAV**XDDOT (1-3)',XDDOT(1),XDDOT(2),XDDOT(3)
      RETURN
      END
