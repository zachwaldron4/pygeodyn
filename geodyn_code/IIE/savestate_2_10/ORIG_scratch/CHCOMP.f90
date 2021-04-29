!$CHCOMP
      SUBROUTINE CHCOMP(K1,K2,COFF,IORD,IGRPP,                          &
     &                  CHB1,CHB2,CHBV1,CHBV2,CHEB,N,IDIM)
!********1*********2*********3*********4*********5*********6*********7**
! CHCOMP           88/11/09            8901.0    PGMR - A. MARSHALL
!
! FUNCTION:  USE A LEAST SQUARES ALGORITHM TO COMPUTE A NEW
!            SET OF CHEBYCHEV COEFFICIENTS FOR THIS
!            MEASUREMENT BLOCK WHICH SPANS TWO PERIOD
!            GROUPS
!
!  I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   K1       I    S    POINTER FOR CHEBYCHEV COEFFICIENTS AT BEGINNING
!                      OF THE BLOCK
!   K2       I    S    POINTER FOR CHEBYCHEV COEFFICIENTS AT THE END
!                      OF THE BLOCK
!   COFF     O    A    OUTPUT CHEBYCHEV COEFFICIENTS
!   IORD     I    S    DEGREE OF CHEBYCHEV COEFFICIENTS
!   IGRPP    I    S    CELESTIAL BODY GROUP
!   CHB1     I    A
!   CHB2     I    A
!   CHBV1    I    A    CHEBYCHEV POLYNOMIAL
!   CHBV2    I    A    CHEBYCHEV POLYNOMIAL
!   CHEB     I    A    (ATA)-1 * AT MATRIX OF CHEB POLY
!   N        I    S    BODY NUMBER : 1=SA ,2=JUP,3=SUN,4=EM,5=MO,6=CB,7=
!                                    7=MAR
!
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/EPHM/FSECXY(1),XP(261),YP(261),A1UT(261),EP(261),EC(261),  &
     &            FSCFLX(1),FLUXS(36),AVGFLX(36),FLUXM(288),FSECEP(1),  &
     &            EPHP(816),EPHN(96),ELIB(120),BUFT(2700),FSECNP(4)
      COMMON/EPHMPT/NTOT,NTOTS,NNPDPR,NUTORD,IBODDG(8),IGRPDG(4),       &
     &              NENPD,INRCD(2),KPLAN(11),NINEP,NEPHMR,NEPHEM,       &
     &              IBHINT,NINTBH,NXEPHP
      COMMON/EPHSET/EMFACT,DTENPD,DTINPD,FSC1EN,FSCENP(2),FSCINP(4),    &
     &   FSDINP(4),XEPHST
      COMMON/IORCHB/ IORM,IORJ,IORSA,IORSUN,IOREM,IORMO,IORCB,IORTB,    &
     &               IGRP,ICHBOG(2,14),IORSCB(988),NXORCH
!
      DIMENSION COFF1(3,IDIM),COFF2(3,IDIM),COFF(3,IDIM)
      DIMENSION RANGE(200,3),CHEB(200,IDIM,7),CHB1(IDIM,50),            &
     &          CHB2(IDIM,50),CHBV1(IDIM,50),CHBV2(IDIM,50),            &
     &          VSEC(4),FNSEC(4)
!
      DATA ONE/1.D00/,ZERO/0.D00/,TWO/2.D00/
      DATA FNSEC/2764800.D0,1382400.D0,691200.D0,345600.D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!
      VSEC(1) = TWO/FNSEC(1)
      VSEC(2) = TWO/FNSEC(2)
      VSEC(3) = TWO/FNSEC(3)
      VSEC(4) = TWO/FNSEC(4)
!
! READ CHEBYSHEV COEFFICIENTS (FROM EPHP ARRAY USING INPUT POINTERS)
      K=K1
      DO 100 I=1,3
      DO 100 J=1,IORD
      COFF1(I,J)=EPHP(K)
  100 K=K+1
      K=K2
      DO 200 I=1,3
      DO 200 J=1,IORD
      COFF2(I,J)=EPHP(K)
  200 K=K+1
!
      DO 500 M=1,3
      DO 400 I=1,50
      RNG1=ZERO
      RNG2=ZERO
      V1  =ZERO
      V2  =ZERO
!
!     COMPUTE RANGE AND VELOCITY
      DO 300 J=1,IORD
      RNG1=RNG1+COFF1(M,J)*CHB1(J,I)
      RNG2=RNG2+COFF2(M,J)*CHB2(J,I)
      V1  =V1  +COFF1(M,J)*CHBV1(J,I)*VSEC(IGRPP)
      V2  =V2  +COFF2(M,J)*CHBV2(J,I)*VSEC(IGRPP)
  300 END DO
!
      RANGE(I,M) = RNG1
      RANGE(I+50,M) = RNG2
      RANGE(I+100,M)= V1
      RANGE(I+150,M)= V2
  400 END DO
  500 END DO
!
! COMPUTE COEFFICIENTS
      DO 600 M=1,3
      DO 600 I=1,IORD
      COFF(M,I)=ZERO
      DO 660 K=1,200
      C1=CHEB(K,I,N)*RANGE(K,M)
      COFF(M,I)=COFF(M,I)+C1
  660 END DO
  600 CONTINUE
!
      RETURN
      END
