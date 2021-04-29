!$PDLGEN
      SUBROUTINE PDLGEN (COEFZ,COEFC,VRARAY,P,AORN,SINLAM,COSLAM,       &
     &                  TANPSI,NMXDEG,GMR,AOR)
!********1*********2*********3*********4*********5*********6*********7**
! PDLGEN           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
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
!   NMXDEG        S    MAXIMUM DEGREE FOR LEGENDRE POLYNOMIAL EVALUATION
!
!  COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

      parameter( ZERO = 0.D0 )
      parameter( ONE = 1.D0 )
      parameter( HALF = 0.5D0 )
      parameter( THREE = 3.0D0 )
      parameter( SQTHR = 1.732050807568877D0 )
      parameter( SQFIV = 2.236067977499790D0 )
      parameter( XNORM6 = 1.290994448735806D0 )
      parameter( XNORM7 = .6454972243679028D0 )

      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      DIMENSION P(22),VRARAY(14),AORN(4),SINLAM(5),                     &
     &   COSLAM(5),TANPSI(5),COEFZ(1),COEFC(NPMAX,*)
! %%%%%%%% COEFC(NPMAX,1) -> COEFC(NPMAX,*)

!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!     PRINT *,'PDLGEN**NMAX,NP,NPMAX=',NMAX,NP,NPMAX
      R=VRARAY(4)
      SINPSI=VRARAY(11)/R
      COSPSI=VRARAY(7)/R
!     write(6,*) 'PDLGEN: GMR, AOR,GM,AE ', GMR, AOR,GM,AE
!     write(6,*) 'PDLGEN: sinpsi, cospsi ', sinpsi, cospsi
!
!     NMAX=4
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
      DO 2000 N=3,4
      P(KR-1)=ZERO
      P(KR-2)=ZERO
      P(KR+N)=COSPSI*COEFZ(N)*P(KRL1+N-1)
      DO 1000 M=1,N
      P(KR-1+M)=SINPSI*COEFC(KR-1+M,1)*P(KRL1-1+M)                      &
     &         -COEFC(KR-1+M,2)*P(KRL2-1+M)
 1000 END DO
      KRL2=KRL1
      KRL1=KR
      KR=KR+N+3
 2000 END DO
!
      TANPSI(1)=ZERO
      COSLAM(1)=ONE
      SINLAM(1)=ZERO
      SINLAM(2)=VRARAY(10)/VRARAY(7)
      COSLAM(2)=VRARAY(9 )/VRARAY(7)
      write(6,*) 'PHLGEN: sinlam(2), coslam(2) ', sinlam(2), coslam(2)
      TANPSI(2)=SINPSI/COSPSI
      CL2=COSLAM(2)+COSLAM(2)
      SINLAM(3)=CL2*SINLAM(2)
      COSLAM(3)=CL2*COSLAM(2)-ONE
      AORN1=AOR*GMR
      AORN(1)=AORN1
      DO 3000 N=2,4
      TANPSI(N+1)=TANPSI(N)+TANPSI(2)
      AORN1=AORN1*AOR
      AORN(N)=AORN1
 3000 END DO
!...
!     NMAXL1=NMAX-1
      DO 4000 N=2,3
      N1=N+1
      N2=N+2
      SINLAM(N2)=CL2*SINLAM(N1)-SINLAM(N)
      COSLAM(N2)=CL2*COSLAM(N1)-COSLAM(N)
 4000 END DO
      RETURN
      END
