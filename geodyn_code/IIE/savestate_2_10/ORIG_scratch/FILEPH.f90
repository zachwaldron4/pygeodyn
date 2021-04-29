!$FILEPH
      SUBROUTINE FILEPH(INDX1,INDX2,HOLDEP)
!********1*********2*********3*********4*********5*********6*********7**
! FILEPH           00/00/00            0000.0    PGMR - ?
!                  91/04/02            9104.0    PGMR - SBL
!
!
! FUNCTION: TEMP ROUTINE TO FILL EPHM COMMON BLOCK
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   INDX1    I    S
!   INDX2    I    S
!   HOLDEP   I    A
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DYNPTR/KDEPHM,KDEPH2,KDAHDR(3,9),KDYNAP(3,9,2),KDNEXT(3),  &
     &NXDYNP
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
      DIMENSION EQUIV(5368),HOLDEP(NTOTS,1)
      DIMENSION NGRP(4)
      DIMENSION EPHNE(5000)
      DIMENSION EPHPE(5000)
      DIMENSION ELIBE(5000)
!
      EQUIVALENCE (FSECXY(1),EQUIV(1))
      EQUIVALENCE (EPHN(1),EPHNE(1))
      EQUIVALENCE (EPHP(1),EPHPE(1))
      EQUIVALENCE (ELIB(1),ELIBE(1))
      DATA NGRP/1,2,4,8/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      NDIMP=KPLAN(8)+NGRP(ICHBOG(2,11))*ICHBOG(1,11)*3-1
      NNUT=2*ICHBOG(1,12)*NGRP(ICHBOG(2,12))
      NOTHER=3*ICHBOG(1,13)*NGRP(ICHBOG(2,13))
!c                WRITE(6,*) 'FILEPH: INDX1, INDX2, NEPHMR ',
!c     1                              INDX1, INDX2, NEPHMR
!
!     ....TAKE ERROR EXIT IF NUMBER OF REQUESTED RECORD >
!     ....TOTAL NUMBER OF EPHEMERIS RECORDS
      IF(INDX2.GT.NEPHMR) GO TO 600
!
      INDX22=INDX1+1
      J=-NTOT
!c                WRITE(6,*) 'FILEPH: INDX1, INDX22, NTOT, J ',
!c     1   INDX1, INDX22, NTOT, J
!
       KE2=NNUT
       KE1=NDIMP+NNUT
      DO 120 I=INDX1,INDX22
         J=J+NTOT
         DO 100 II=1,NTOTS
            EQUIV(II+J)=HOLDEP(II,I)
  100 END DO
       IF(NDIMP.LT.816) THEN
            DO 105 II=1,NNUT
            EPHNE(KE2+1-II+J)=EPHPE(KE1+1-II+J)
  105      CONTINUE
       ENDIF
      NDIMN=2*NUTORD*NNPDPR
      IF(NDIMN.EQ.96) GO TO 120
        NDIML=120
        DO 115 II=1,NDIML
           ELIBE(NDIML+1-II+J)=EPHNE(NDIMN+1+NDIML-II+J)
  115   CONTINUE
  120   CONTINUE
      RETURN
!
!     ....ERROR EXIT
!
  600 JLINE6=ILINE6+3
      IF(JLINE6.LE.MLINE6) GO TO 1000
      IPAGE6=IPAGE6+1
      JLINE6=4
      WRITE(IOUT6,10000) IPAGE6
 1000 CONTINUE
      WRITE(IOUT6,20000)
      WRITE(IOUT6,30000) FSC1EN,INDX2
      ILINE6=JLINE6
      STOP 16
!
10000 FORMAT('1',109X,'UNIT  6 PAGE NO.',I6)
20000 FORMAT(/'0EPHEMERIS PROBLEM IN G2E SUBROUTINE FILEPH '/           &
     &     '  INDX2 IS GREATER THAN NEPHMR  '/                          &
     &     '  FSC1EN AND INDX2 PRINTED: '/1X )
30000 FORMAT(' ',D25.16,I35)
      END
