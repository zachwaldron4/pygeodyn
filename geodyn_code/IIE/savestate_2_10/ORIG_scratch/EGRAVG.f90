!$EGRAVG
      SUBROUTINE EGRAVG(COEFZ,COEFC,P,C,S,AORN,SINLAM,COSLAM,           &
!...Begin TJS
     &   TANPSI,XM,XNP1,EXPRFL,XDDOT,VRARAY,D,FSB,DFSBDR)
!...End TJS
!********1*********2*********3*********4*********5*********6*********7**
! EGRAV            00/00/00            0000.0    PGMR - TOM MARTIN
!                                                       DAVE ROWLANDS
!
! FUNCTION:  EVALUATE ACCELERATION ON SATELLITE DUE
!            TO GEOPOTENTIAL AS DESCRIBED BY A SET
!            OF HARMONICS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   COEFZ    I    A    PRECALCULATED FACTORS NEEDED FOR
!                      NORMALIZED TESSERAL POLYNOMIALS
!   COEFC    I    A    PRECALCULATED FACTORS NEEDED FOR
!                      NORMALIZED  POLYNOMIALS
!   P        O    A    LEGENDRE POLYNOMIALS (NORMALIZED)
!   C        I    A    C POTENTIAL COEFFICIENTS
!   S        I    A    S POTENTIAL COEFFICIENTS
!   AORN     O    A    ARRAY OF (GM/R)*(AE/R)**N
!   SINLAM   O    A    ARRAY OF SIN(LAMDA*M)
!   COSLAM   O    A    ARRAY OF COS(LAMDA*M)
!   TANPSI   O    A    ARRAY OF M*TAN(PSI)
!   XM       I    A    ARRAY OF ORDERS TO MATCH C&S ARRAYS
!   XNP1     I    A    ARRAY OF DEGREE PLUS1 TO MATCH C&S ARRAYS
!   EXPRFL   O    A    ARRAY OF EXPLICIT PARTIALS OF ACCELERATION
!                      IN RFL WRT C&S
!   XDDOT    O    A    ACCELERATION OF SATELLITE IN TRUE OF REF
!   VRARAY   O    A    (1-3)ACCELERATION DUE TO GEOPOTENTIAL
!                      IN R,PPSI,LAMDA
!   D        O    A    LEGENDRE POLYNOMIAL EQUATION (NORMALIZED)
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CRMBI/RMBI(9)
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      COMMON/VRBLOK/SINPSI,COSPSI,XLAMDA,GMR,AOR
      DIMENSION D(NP)
      DIMENSION P(NP),AORN(NMAX),SINLAM(NMAXP1),                        &
     &   COSLAM(NMAXP1),TANPSI(NMAXP1),C(NP),                           &
     &   S(NP),XM(NP),XNP1(NP),EXPRFL(NP,6),                            &
     &   XDDOT(3),VRARAY(9),COEFZ(1),COEFC(NPMAX,*)
! %%%%%%%% COEFC(NPMAX,1) -> COEFC(NPMAX,*)
!...Begin TJS
      DIMENSION FSB(3),FSS(3),DFSBDR(3,3),DFSSDR(3,3)
      DIMENSION PRPS(3,3),PSPR(3,3),DFSBDX(3)
!...End TJS
      DATA ZERO/0.D0/,ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! *********** DEBUG
      IF(NMAX.LT.1) GO TO 7005
!************* DEBUG
      CALL LGENDR(.FALSE.,COEFZ,COEFC,VRARAY,P,AORN,SINLAM,COSLAM,      &
     &            TANPSI,NMAX)
      NPM1=NP-1
!YBER      NP1=2
!YBER      KR=1
!YBER      DO 1100 N=1,NMAX
!YBER      EXPRFL(KR,6;NP1)=COSLAM(1;NP1)*AORN(N)
!YBER      EXPRFL(KR,5;NP1)=SINLAM(1;NP1)*AORN(N)
!YBER      KR=KR+N+3
!YBER      NP1=NP1+1
!YBER 1100 CONTINUE
!YBER      EXPRFL(1,1;NP)=XM(1;NP)*P(1;NP)
!YBER      EXPRFL(1,3;NPM1)=P(2;NPM1)*COEFC(1,3;NPM1)
!YBER      EXPRFL(1,1;NP)=EXPRFL(1,3;NP)-TANPSI(2)*EXPRFL(1,1;NP)
!YBER      D(1;NP)=EXPRFL(1,1;NP)
!YBER      EXPRFL(1,3;NP)=EXPRFL(1,6;NP)*EXPRFL(1,1;NP)
!YBER      EXPRFL(1,4;NP)=EXPRFL(1,5;NP)*EXPRFL(1,1;NP)
!YBER      EXPRFL(1,5;NP)=EXPRFL(1,5;NP)*P(1;NP)
!YBER      EXPRFL(1,6;NP)=EXPRFL(1,6;NP)*P(1;NP)
!YBER      EXPRFL(1,1;NP)=-XNP1(1;NP)*EXPRFL(1,6;NP)
!YBER      EXPRFL(1,2;NP)=-XNP1(1;NP)*EXPRFL(1,5;NP)
!YBER      EXPRFL(1,5;NP)=-EXPRFL(1,5;NP)*XM(1;NP)
!YBER      EXPRFL(1,6;NP)=EXPRFL(1,6;NP)*XM(1;NP)
!YBER      DO 1300 J=1,3
!YBER      JJ=(J-1)*2+1
!YBER      J1=JJ+1
!YBER      EXPJ2T=EXPRFL(5,JJ)
!YBER      EXPRFL(5,JJ)=ZERO
!YBER      VRARAY(J)=Q8SDOT(EXPRFL(1,JJ;NP),C(1;NP))
!YBER     .         +Q8SDOT(EXPRFL(1,J1;NP),S(1;NP))
!YBER      EXPRFL(5,JJ)=EXPJ2T
!YBER      VRARAY(J)=VRARAY(J)+EXPJ2T*C(5)
!YBER 1300 CONTINUE
      MPT=NMAX*(NMAX-1)/2+3*(NMAX-1)
      DO 900 I=1,NMAXP1
      EXPRFL(I,4)=COSLAM(I)*XM(MPT+I)
      EXPRFL(I,6)=SINLAM(I)*XM(MPT+I)
  900 END DO
!...Begin TJS
      R=AE/AOR
      RSQ=R**2
      CPHI=COSLAM(2)
      SPHI=SINLAM(2)
      CTHE=SINPSI
      STHE=COSPSI
      STSQ=STHE**2
      COTT=SINPSI/COSPSI
      DO I=1,3
         DO J=1,3
            DFSSDR(I,J)=0.D0
         END DO
      END DO
!...End TJS
      KRA1=0
      KNL=2
      DO 1100 N=1,NMAX
      DEGPSI=ZERO
      DO 1050 KR=1,KNL
! cos(m*phi)*c_n^m+sin(m*phi)*s_n^m
      EXPRFL(KR+KRA1,5)=COSLAM(KR)*C(KR+KRA1)+SINLAM(KR)*S(KR+KRA1)
! d{P}/d{lat}_n^m
      EXPRFL(KR+KRA1,2)=P(KR+KRA1+1)*COEFC(KR+KRA1,3)                   &
     &                -TANPSI(KR)*P(KR+KRA1)
! NOT USED!!!
      D(KR+KRA1)=P(KR+KRA1+1)*COEFC(KR+KRA1,3)                          &
     &                -TANPSI(2)*P(KR+KRA1)*XM(KR+KRA1)
! -sin(m*phi)*c_n^m+cos(m*phi)*s_n^m
      EXPRFL(KR+KRA1,3)=EXPRFL(KR,4)*S(KR+KRA1)                         &
     &                 -EXPRFL(KR,6)*C(KR+KRA1)
! [cos(m*phi)*c_n^m+sin(m phi)*s_n^m]*P_n^m
      EXPRFL(KR+KRA1,1)=EXPRFL(KR+KRA1,5)*P(KR+KRA1)
! [cos(m*phi)*c_n^m+sin(m phi)*s_n^m]*d{P}/d{psi}_n^m
      DEGPSI=DEGPSI+EXPRFL(KR+KRA1,5)*EXPRFL(KR+KRA1,2)
 1050 END DO

! GM/r*(a/r)^n*[cos(m*phi)*c_n^m+sin(m phi)*s_n^m]*d{P}/d{psi}_n^m

      EXPRFL(N,5)=DEGPSI*AORN(N)

      KRA1=KRA1+N+3
      KNL=KNL+1
 1100 END DO
      MPT=MPT+2
      KRA1=0
      KNL=2
      DO 1200 N=1,NMAX
      DEGR=ZERO
      DEGLAM=ZERO
      DO 1150 KR=1,KNL
! [cos(m*phi)*c_n^m+sin(m phi)*s_n^m]*P_n^m
      DEGR=DEGR+EXPRFL(KR+KRA1,1)
! [-sin(m*phi)*c_n^m+cos(m phi)*s_n^m]*P_n^m
      DEGLAM=DEGLAM+EXPRFL(KR+KRA1,3)*P(KR+KRA1)

!...Begin TJS
! dg_r/dtheta
      DFSSDR(1,2)=DFSSDR(1,2)+XM(MPT+N)*AORN(N)*EXPRFL(KR+KRA1,5)*      &
     &            EXPRFL(KR+KRA1,2)/R
! dg_theta/dtheta
      DFSSDR(2,2)=DFSSDR(2,2)+AORN(N)*EXPRFL(KR+KRA1,5)*                &
     &            ((DBLE(KR-1)**2/STSQ-DBLE(N*(N+1)))*P(KR+KRA1)+       &
     &            COTT*EXPRFL(KR+KRA1,2))/R
! dg_phi/dtheta
      DFSSDR(3,2)=DFSSDR(3,2)-                                          &
     &            AORN(N)*EXPRFL(KR+KRA1,3)*(EXPRFL(KR+KRA1,2)+         &
     &            COTT*P(KR+KRA1))/STHE/R
! dg_r/dphi
      DFSSDR(1,3)=DFSSDR(1,3)-XM(MPT+N)*AORN(N)*EXPRFL(KR+KRA1,3)*      &
     &            P(KR+KRA1)/R
! dg_theta/dphi
      DFSSDR(2,3)=DFSSDR(2,3)-AORN(N)*EXPRFL(KR+KRA1,3)*                &
     &            EXPRFL(KR+KRA1,2)/R
! dg_phi/dphi
      DFSSDR(3,3)=DFSSDR(3,3)-DBLE(KR-1)**2*AORN(N)*EXPRFL(KR+KRA1,5)*  &
     &            P(KR+KRA1)/STHE/R
!...End TJS

 1150 END DO

! GM/r*(a/r)^n*(n+1)*[cos(m*phi)*c_n^m+sin(m phi)*s_n^m]*P_n^m

      EXPRFL(N,4)=-DEGR*AORN(N)*XM(MPT+N)

! GM/r*(a/r)^n*[-sin(m*phi)*c_n^m+cos(m phi)*s_n^m]*P_n^m

      EXPRFL(N,6)=DEGLAM*AORN(N)

!...Begin TJS
! dg_r/dr
      DFSSDR(1,1)=DFSSDR(1,1)-DBLE(N+2)*EXPRFL(N,4)/RSQ
! dg_theta/dr
      DFSSDR(2,1)=DFSSDR(2,1)+DBLE(N+2)*EXPRFL(N,5)/RSQ
! dg_phi/dr
      DFSSDR(3,1)=DFSSDR(3,1)-DBLE(N+2)*EXPRFL(N,6)/STHE/RSQ
!...End TJS

      KRA1=KRA1+N+3
      KNL=KNL+1
 1200 END DO
      EXPRFL(NMAX,4)=-DEGR*AORN(NMAX)*(XM(MPT+NMAX-1)+ONE)
      DO 1300 J=1,3
      VRARAY(J)=ZERO
      JJ=J+3
      DO 1250 JJJ=3,NMAX
 1250 VRARAY(J)=VRARAY(J)+EXPRFL(JJJ,JJ)
      VRARAY(J)=VRARAY(J)+EXPRFL(1,JJ)
      VRARAY(J)=VRARAY(J)+EXPRFL(2,JJ)
 1300 END DO
! ******* DEBUG
 7005 CONTINUE
! ******* DEBUG
      VRARAY(1)=VRARAY(1)/VRARAY(4)

!...Begin TJS
! acceleration in spherical system
      FSS(1)= VRARAY(1)
      FSS(2)=-VRARAY(2)/R
      FSS(3)= VRARAY(3)/STHE/R
! include spherical coordinate system change wrt position
      DFSSDR(1,2)=DFSSDR(1,2)-FSS(2)
      DFSSDR(2,2)=DFSSDR(2,2)+FSS(1)
      DFSSDR(1,3)=DFSSDR(1,3)-STHE*FSS(3)
      DFSSDR(2,3)=DFSSDR(2,3)-CTHE*FSS(3)
      DFSSDR(3,3)=DFSSDR(3,3)+STHE*FSS(1)+CTHE*FSS(2)
! compute d{position}/d{r,theta,phi}
      PRPS(1,1)=STHE*CPHI
      PRPS(1,2)=CTHE*CPHI
      PRPS(1,3)=-SPHI
      PRPS(2,1)=STHE*SPHI
      PRPS(2,2)=CTHE*SPHI
      PRPS(2,3)=CPHI
      PRPS(3,1)=CTHE
      PRPS(3,2)=-STHE
      PRPS(3,3)=0.D0
! compute d{r,theta,phi}/d{position}
      PSPR(1,1)=STHE*CPHI
      PSPR(1,2)=STHE*SPHI
      PSPR(1,3)=CTHE
      PSPR(2,1)=CTHE*CPHI/R
      PSPR(2,2)=CTHE*SPHI/R
      PSPR(2,3)=-STHE/R
      PSPR(3,1)=-SPHI/STHE/R
      PSPR(3,2)=CPHI/STHE/R
      PSPR(3,3)=0.D0
! transform acceleration and d{acceleration}/d{position} from spherical
! to body-fixed
      DO I=1,3
         FSB(I)=PRPS(I,1)*FSS(1)+PRPS(I,2)*FSS(2)+PRPS(I,3)*FSS(3)
         DO J=1,3
            DFSBDX(J)=PRPS(I,1)*DFSSDR(1,J)+                            &
     &                PRPS(I,2)*DFSSDR(2,J)+                            &
     &                PRPS(I,3)*DFSSDR(3,J)
         END DO
         DO J=1,3
            DFSBDR(I,J)=DFSBDX(1)*PSPR(1,J)+                            &
     &                  DFSBDX(2)*PSPR(2,J)+                            &
     &                  DFSBDX(3)*PSPR(3,J)
         END DO
      END DO
!...End TJS

      XDDOT(1)=VRARAY(1)*RMBI(1)+VRARAY(2)*RMBI(4)+VRARAY(3)*RMBI(7)
      XDDOT(2)=VRARAY(1)*RMBI(2)+VRARAY(2)*RMBI(5)+VRARAY(3)*RMBI(8)
      XDDOT(3)=VRARAY(1)*RMBI(3)+VRARAY(2)*RMBI(6)+VRARAY(3)*RMBI(9)
      VRARAY(1)=VRARAY(1)-GMR/VRARAY(4)
      RETURN
      END
