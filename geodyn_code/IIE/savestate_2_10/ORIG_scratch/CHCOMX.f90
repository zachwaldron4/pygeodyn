!$CHCOMX
      SUBROUTINE CHCOMX(IBODY,COFF,BUFFER,IORD,IDIM,                    &
     &                  CHB1,CHB2,CHBV1,CHBV2,CHEB,VSEC,N,ICB)
!********1*********2*********3*********4*********5*********6*********7**
! CHCOMX
!
! FUNCTION:  USE A LEAST SQUARES ALGORITHM TO COMPUTE A NEW
!            SET OF CHEBYCHEV COEFFICIENTS FOR THIS
!            MEASUREMENT BLOCK WHICH SPANS TWO PERIOD
!            GROUPS
!            THIS CODE FOR SUPPLEMENTARY EPHEMERIS ONLY
!
!  I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
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
      DIMENSION COFF1(3,25),COFF2(3,25),COFF(3,25)
      DIMENSION RANGE(200,3),CHEB(200,25,ICB),CHB1(25,50),CHB2(25,50),  &
     &          CHBV1(25,50),CHBV2(25,50)
      DIMENSION BUFFER(1,IDIM,2)
!
      DATA ONE/1.D00/,ZERO/0.D00/,TWO/2.D00/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!
!
! READ CHEBYSHEV COEFFICIENTS (FROM EPHP ARRAY USING INPUT POINTERS)
      DO  J=1,IORD
      DO I=1,3
      IBK=(I-1)*IORD
      COFF1(I,J)=BUFFER(IBODY,J+IBK,1)
      COFF2(I,J)=BUFFER(IBODY,J+IBK,2)
      ENDDO
      ENDDO
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
      V1  =V1  +COFF1(M,J)*CHBV1(J,I)*VSEC
      V2  =V2  +COFF2(M,J)*CHBV2(J,I)*VSEC
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
