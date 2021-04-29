!$LGENDR
      SUBROUTINE LGENDR(LPOLON,COEFZ,COEFC,VRARAY,P,AORN,SINLAM,COSLAM, &
     &                  TANPSI,NMXDEG)
!********1*********2*********3*********4*********5*********6*********7**
! LGENDR           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   LPOLON        S    TRUE OF CALCULATION OF ANGULAR ARGUMENTS IS
!                      REQUESTED
!   COEFZ    I    A    SQRT(1+1/ZN) IS USED FOR SECTORAL RECURSION
!   COEFC    I    A    (*,1&2) ARE USED FOR ZONAL AND TESSERAL
!                      RECURSION; (*,3) IS NORMALIZATION FACTOR FOR N,M
!                      DIVIDED BY THAT OF N,M+1
!   VRARAY   I    A    (7)=(X**2 + Y**2)**1/2, 9=SAT X POSITION,
!                      10=SAT Y POSITION
!   P        O    A    NORMALIZED LEGENDRE POLYNOMIALS
!   AORN          A    ARRAY OF (GM/R)*(AE/R)**N
!   SINLAM   O    A    ARRAY OF SIN(M*SATELLITE LONGITUDE)
!   COSLAM   O    A    ARRAY OF COS(M*SATELLITE LONGITUDE)
!   TANPSI   O    A    ARRAY OF SUBSATELLITE LONGITUDE
!   NMXDEG        S    MAXIMUM DEGREE FOR LGENDRE POLYNOMIAL EVALUATION
!
!  COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!YBER      IMPLICIT REAL (A-H,O-Z),LOGICAL(L)
      COMMON/VRBLOK/SINPSI,COSPSI,XLAMDA,GMR,AOR
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      DIMENSION P(NPMAX),VRARAY(14),AORN(NPMAX),SINLAM(NPMAX),          &
     &   COSLAM(NPMAX),TANPSI(NPMAX),COEFZ(NPMAX),COEFC(NPMAX,*)
! %%%%%% COEFC(NPMAX,1) -> COEFC(NPMAX,*)
      DATA ZERO/0.D0/,ONE/1.D0/,HALF/0.5D0/,THREE/3.0D0/,               &
     &     SQTHR/1.732050807568877D0/
      DATA SQFIV/2.236067977499790D0/
      DATA XNORM6/1.290994448735806D0/
      DATA XNORM7/.6454972243679028D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      P(1)=SQTHR*SINPSI
      P(2)=SQTHR*COSPSI
      P(3)=ZERO
      P(4)=ZERO
      P(5)=SQFIV*((ONE+HALF)*SINPSI**2-HALF)
      CP3=THREE*COSPSI
      P(6)=CP3*SINPSI*XNORM6
      P(7)=CP3*COSPSI*XNORM7
      KRL1=5
      KRL2=1
      KR=10
      DO 2000 N=3,NMXDEG
      P(KR-1)=ZERO
      P(KR-2)=ZERO
      P(KR+N)=COSPSI*COEFZ(N)*P(KRL1+N-1)
      DO 1000 M=1,N
      P(KR-1+M)=SINPSI*COEFC(KR-1+M,1)*P(KRL1-1+M)                      &
     &         -COEFC(KR-1+M,2)*P(KRL2-1+M)
 1000 END DO
!YBERC     P(KR;N)=SINPSI*COEFC(KR,1;N)*P(KRL1;N)
!YBERC    .       -COEFC(KR,2;N)*P(KRL2;N)
!YBER      AORN(1;N)=COEFC(KR,1;N)*P(KRL1;N)
!YBER      P(KR;N)=COEFC(KR,2;N)*P(KRL2;N)
!YBER      P(KR;N)=SINPSI*AORN(1;N)-P(KR;N)
      KRL2=KRL1
      KRL1=KR
      KR=KR+N+3
 2000 END DO
      IF(LPOLON) RETURN
!
      TANPSI(1)=ZERO
      COSLAM(1)=ONE
      SINLAM(1)=ZERO
      SINLAM(2)=VRARAY(10)/VRARAY(7)
      COSLAM(2)=VRARAY(9 )/VRARAY(7)
      TANPSI(2)=SINPSI/COSPSI
      CL2=COSLAM(2)+COSLAM(2)
      SINLAM(3)=CL2*SINLAM(2)
      COSLAM(3)=CL2*COSLAM(2)-ONE
      AORN1=AOR*GMR
      AORN(1)=AORN1
      DO 3000 N=2,NMAX
      TANPSI(N+1)=TANPSI(N)+TANPSI(2)
      AORN1=AORN1*AOR
      AORN(N)=AORN1
 3000 END DO
      NMAXL1=NMAX-1
      DO 4000 N=2,NMAXL1
      N1=N+1
      N2=N+2
      SINLAM(N2)=CL2*SINLAM(N1)-SINLAM(N)
      COSLAM(N2)=CL2*COSLAM(N1)-COSLAM(N)
 4000 END DO
      RETURN
      END
