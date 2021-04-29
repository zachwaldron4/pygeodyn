!$BCCORD
      SUBROUTINE BCCORD(MJDSEC,FSEC,NM,PLANET,PLANV,AA,II)
!********1*********2*********3*********4*********5*********6*********7**
! BCCORD          12.06.90          0000.0  WRITTEN BY LUCIA TSAOUSSI
!
! FUNCTION:        THE COMPUTATION OF BARYCENTRIC POSITIONS AND
!                  VELOCITIES FOR THE VLBI ALGORITHM
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS IN ALPHABETICAL ORDER
!   ------  ---  ---   ------------------------------------------------
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBARYC/CBODY(6),TBODY(6),SUNRF(6)
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CHCOEF/ COEFM(3,15),COEFJ(3,15),COEFSA(3,15),COEFS(3,15),  &
     &               COEFEM(3,15),COEFMO(3,15),COEF(3,15)
      COMMON/EPHM/FSECXY(1),XP(261),YP(261),A1UT(261),EP(261),EC(261),  &
     &            FSCFLX(1),FLUXS(36),AVGFLX(36),FLUXM(288),FSECEP(1),  &
     &            EPHP(816),EPHN(96),ELIB(120),BUFT(2700),FSECNP(4)
      COMMON/EPHMPT/NTOT,NTOTS,NNPDPR,NUTORD,IBODDG(8),IGRPDG(4),       &
     &              NENPD,INRCD(2),KPLAN(11),NINEP,NEPHMR,NEPHEM,       &
     &              IBHINT,NINTBH,NXEPHP
      COMMON/EPHSET/EMFACT,DTENPD,DTINPD,FSC1EN,FSCENP(2),FSCINP(4),    &
     &   FSDINP(4),XEPHST
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/IORCHB/ IORM,IORJ,IORSA,IORSUN,IOREM,IORMO,IORCB,IORTB,    &
     &               IGRP,ICHBOG(2,14),IORSCB(988),NXORCH
      COMMON/PLNETI/IPLNET(999),IPLNIN(999),MPLNGD(999),MPLNGO(999),   &
     &              IPLNZ(999),NXPLNI
      DIMENSION EARBP(NM,3),EARBV(NM,3),SUNBP(NM,3)
      DIMENSION FSEC(NM)
      DIMENSION AA(1),II(1)
      DIMENSION PLANET(1,3,11),PLANV(1,3,11)
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!     CALL COORDT()    **** MAKE APPROX. OF TDB WITH TDT FOR NOW ******
!     CALLED FROM LITEDN & LITEUP
!
!
!  *** SUN AND EARTH POSITION COMPUTATION TWICE FOR CHECKING  ***
!  *** LOOP THROUGH PLANPO & ONE CALL TO BARYC                ***
!
!
      DO 20 I=1,NM
      FSECP=FSEC(I)
      CALL PLANPO(MJDSEC,FSECP,.TRUE.,.TRUE.,AA,II)
      DO 12 K=1,11
      DO 11 J=1,3
      PLANET(I,J,K)=BDSTAT(J,K)
      PLANV(I,J,K)=BDSTAT(J+3,K)
 11   CONTINUE
 12   CONTINUE
 20   CONTINUE
!
      RETURN
      END
