!$INTPRX
      SUBROUTINE INTPRX(XSTT1,XSTT2,XSTR1,XSTR2,XS1,XS2,                &
     &                  COEFEM,COEFMO,COEFM,                            &
     &                  IGRPX,IORCBX,IORTBX,IORMOX,COUNT,LTWO,NM,       &
     &                  RANGET,RANGER,RANGE2,RANGE4,RRT,RRR,            &
     &                  CHEBS1,CHEBS2,CHEBT1,CHEBT2,CHEBR1,             &
     &                  CHEBR2,CBT1MO,CBT2MO,CBR1MO,CBR2MO,             &
     &                  XET1,XER1,XM1,DXET,DXER,DXM,                    &
     &                  DXTMO,DXRMO,DSTT,DSTR,DS,                       &
     &                  VECT,VECR,DVECT,DVECR,DVECT2,DVECR2,            &
     &                  BETAT,BETAR,AT,AR,SUMT,SUMR,FT,FR,              &
     &                  DELTT,DELTR,DELTS,DELMOT,DELMOR,DELEMT,         &
     &                  DELEMR,AA)
!********1*********2*********3*********4*********5*********6*********7**
! INTPRR           88/08/24            8901.0    PGMR - A. MARSHALL
!
! FUNCTION:  COMPUTE THE ONE OR TWO-WAY RANGE-RATE MEASUREMENT
!            FOR AN INTERPLANETARY SATELLITE USING CHEBYSHEV
!            POLYNOMIAL DIFFERENCING - ONLY FOR SUPPLEMENTARY EPHEMERIS
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSTT1    I    A    TRANSMITTING STATION COOR W.R.T. EARTH CENTER
!                      IN THE TRUE OF REFERENCE BARYCENTRIC FRAME AT
!                      POINT1
!   XSTT2    I    A    TRANSMITTING STATION COOR W.R.T. EARTH CENTER
!                      IN THE TRUE OF REFERENCE BARYCENTRIC FRAME AT
!                      POINT2
!   XSTR1    I    A    RECEIVING STATION COOR W.R.T. EARTH CENTER IN
!                      THE TRUE OF REFERENCE BARYCENTRIC FRAME AT
!                      POINT1
!   XSTR2    I    A    RECEIVING STATION COOR W.R.T. EARTH CENTER IN
!                      THE TRUE OF REFERENCE BARYCENTRIC FRAME AT
!                      POINT2
!    XS1     I    A    SATELLITE COOR W.R.T. CB CENTER IN THE TRUE
!                      OF REFERENCE BARYCENTRIC FRAME AT POINT1
!    XS2     I    A    SATELLITE COOR W.R.T. CB CENTER IN THE TRUE
!                      OF REFERENCE BARYCENTRIC FRAME AT POINT2
!    COEFEM  I    A    CHEBYSHEV POLYNOMIAL COEFFICIENTS FOR
!                      EARTH MOON(TRACKING BODY)
!    COEFMO  I    A    CHEBYSHEV POLYNOMIAL COEFFICIENTS FOR MOON
!    COEFM   I    A    CHEBYSHEV POLYNOMIAL COEFFICIENTS FOR CBODY
!    IGRP    I    S    CENTRAL BODY GROUP (MERCURY=3, OTHERWISE=1)
!    IORCB   I    S    ORDER OF CENTRAL BODY CHEBYSHEV POLYNOMIALS
!    IORTB   I    S    ORDER OF TRACKING BODY (EARTH) CHEB POLY.
!    IORMO   I    S    ORDER OF MOON CHEBYSHEV POLYNOMIALS
!    COUNT   I    S    COUNT INTERVAL
!    LTWO    I    S    SWITCH FOR 1 OR 2-WAY RANGE (.TRUE. = 2-WAY)
!    NM      I    S    NUMBER OF MEASUREMENTS IN BLOCK
!    RANGET  I    A    RANGE MEASUREMENT AT BEGINNING OF COUNT FROM
!                      TRANSMITTING STATION TO SATELLITE
!    RANGER  I    A    RANGE MEASUREMENT AT BEGINNING OF COUNT FROM
!                      RECEIVING STATION TO SATELLITE
!    RANGE2 I/O   A    RANGE**2
!    RANGE4 I/O   A    RANGE**4
!    RRT    I/O   A    SCRATCH RANGE RATE
!    RRR     O    A    RANGE-RATE
!    CHEBS1  I    A    CHEBYSHEV POLYNOMIALS AT SATELLITE POINT 1
!    CHEBS2  I    A    CHEBYSHEV POLYNOMIALS AT SATELLITE POINT 2
!    CHEBT1  I    A    CHEBYSHEV POLYNOMIALS AT TRANSMITTING POINT 1
!    CHEBT2  I    A    CHEBYSHEV POLYNOMIALS AT TRANSMITTING POINT 2
!    CHEBR1  I    A    CHEBYSHEV POLYNOMIALS AT RECEIVING POINT 1
!    CHEBR2  I    A    CHEBYSHEV POLYNOMIALS AT RECEIVING POINT 2
!    CBT1MO  I    A    CHEBYSHEV POLYNOMIALS AT TRANSMITTING
!                      POINT1 (MO)
!    CBT2MO  I    A    CHEBYSHEV POLYNOMIALS AT TRANSMITTING
!                      POINT 2 (MO)
!    CBR1MO  I    A    CHEBYSHEV POLYNOMIALS AT RECEIVING POINT 1
!                      MOON
!    CBR2MO  I    A    CHEBYSHEV POLYNOMIALS AT RECEIVING POINT 2
!                      MOON
!    XET1    I    A    EARTH POSITION AT TRANSMITTING POINT 1
!    XER1    I    A    EARTH POSITION AT RECEIVING POINT 1
!    XM1     I    A    CB POSITION AT POINT 1
!    DXET   I/O   A    CHANGE IN EARTH POISTION BETWEEN TRANSMIT
!                      POINTS 1&2
!    DXER   I/O   A    CHANGE IN EARTH POISTION BETWEEN RECEIVING
!                      POINTS 1&2
!    DXM    I/O   A    CHANGE IN CB POSITION BETWEEN POINTS 1&2
!    DXTMO  I/O   A    CHANGE IN MOON POISTION BETWEEN TRANSMITTING
!                      POINTS 1&2
!    DXRMO  I/O   A    CHANGE IN MOON POISTION BETWEEN RECEIVING
!                      POINTS 1&2
!    DSTT   I/O   A    CHANGE IN TRANSMITTNG STATION POSITION
!                      BETWEEN POINTS 1&2
!    DSTR   I/O   A    CHANGE IN RECEIVING STATION POSITION BEWTEEN
!                      POINTS 1&2
!    DS     I/O   A    CHANGE IN SATELLITE POSITION BETWEEN POINTS
!                      1&2
!    VECT   I/O   A    RELATIVE VECTOR BETWEEN TRANSMITTING STATION
!                      AND SATELLITE AT POINT 1
!    VECR   I/O   A    RELATIVE VECTOR BETWEEN RECEIVING STATION
!                      AND SATELLITE AT POINT 1
!    DVECT  I/O   A    RELATIVE CHANGE VECTOR BETWEEN TRANSMITTING
!                      STATION AND SATELLITE AT POINT 1
!    DVECR  I/O   A    RELATIVE CHANGE VECTOR BETWEEN RECEIVING
!                      STATION AND SATELLITE AT POINT 1
!    DVECT2 I/O   A    SQUARE OF DVECT
!    DVECR2 I/O   A    SQUARE OF DVECR
!    BETAT  I/O   A    RELATIVE TRANSMITTING CHANGE VECTOR LENGTH
!    BETAR  I/O   A    RELATIVE RECEIVING CHANGE VECTOR LENGTH
!    AT     I/O   A    UNIT TRANSMITTING CHANGE VECTOR
!    AR     I/O   A    UNIT RECEIVING CHANGE VECTOR
!    SUMT   I/O   A    SCRATCH SUM
!    SUMR   I/O   A    SCRATCH SUM
!    FT     I/O   A    TRANSMITTING DIRECTIONAL DERIVATIVES
!    FR     I/O   A    RECEIVING DIRECTIONAL DERIVATIVES
!    DELTT  I/O   A    TRANSMIT TIME DIFFERENCE
!    DELTR  I/O   A    RECEIVE  TIME DIFFERENCE
!    DELTS  I/O   A    SATELLITE NORMALIZED TIME DIFFERENCE
!    DELMOT I/O   A    MOON TRANSMIT NORMALIZED TIME DIFFERENCE
!    DELMOR I/O   A    MOON RECEIVE  NORMALIZED TIME DIFFERENCE
!    DELEMT I/O   A    EM TRANSMIT NORMALIZED TIME DIFFERENCE
!    DELEMR I/O   A    EM RECEIVE  NORMALIZED TIME DIFFERENCE
!    AA     I/O   A
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CHCOFX/ COEFX(3,25),COEFXX(3,25,988),RLVAL(988)
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
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
      COMMON/PLNTRR/JCHEMR,JCHMOR,JEPOSR,JCHEMT,JCHMOT,JEPOST,JCHCBS,   &
     &              JCPOSS,JXSTT,JXSTR,JXSS,JFSCN1,JFSCN2,JFSCK1,       &
     &              JFSCK2,JFSCM1,JFSCM2
!*DEBUG*******
!       COMMON/LANDY/KKOUNT
!*************
      DIMENSION XSTT1(MINTIM,3),XSTT2(MINTIM,3),XSTR1(MINTIM,3),        &
     &          XSTR2(MINTIM,3),XS1(MINTIM,3),XS2(MINTIM,3),            &
     &          COEFM(3,IORCB),COEFEM(3,IORTB),COEFMO(3,IORMO),         &
     &          RANGET(NM),RANGER(NM),RRT(NM),RRR(NM)
      DIMENSION CHEBS1(NM,IORCB),CHEBS2(NM,IORCB),                      &
     &          CHEBT1(NM,IORTB),CHEBT2(NM,IORTB),                      &
     &          CHEBR1(NM,IORTB),CHEBR2(NM,IORTB),                      &
     &          CBT1MO(NM,IORMO),CBT2MO(NM,IORMO),CBR1MO(NM,IORMO),     &
     &          CBR2MO(NM,IORMO),FNSEC(4)
      DIMENSION XET1(NM,3),XER1(NM,3),XM1(NM,3),                        &
     &          DXET(NM,3),DXER(NM,3),DXM(NM,3),DXTMO(NM,3),DXRMO(NM,3),&
     &          DSTT(NM,3),DSTR(NM,3),DS(NM,3),                         &
     &          VECT(NM,3),VECR(NM,3),                                  &
     &          DVECT(NM,3),DVECR(NM,3),DVECT2(NM),DVECR2(NM),          &
     &          BETAT(NM),BETAR(NM),AT(NM,3),AR(NM,3),                  &
     &          SUMT(NM),SUMR(NM),FT(NM,3),FR(NM,3) ,                   &
     &          RANGE2(NM),RANGE4(NM)
      DIMENSION DELTT(NM),DELTR(NM),DELTS(NM),DELMOT(NM),               &
     &          DELMOR(NM),DELEMT(NM),DELEMR(NM),AA(1)
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,THREE/3.0D00/
      DATA FNSEC/2764800.D0,1382400.D0,691200.D0,345600.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!**************DEBUG*******
!         WRITE(6,*) 'INSIDE INTPRR'
!****************************
!
!
! INITIALIZE
      NM3 = NM*3
      IF(LTWO) THEN
      DO 50 I=1,NM
      DVECT2(I) = ZERO
      SUMT(I)   = ZERO
      DO 50 J=1,3
      DXET(I,J)  = ZERO
      DXTMO(I,J) = ZERO
   50 CONTINUE
      ENDIF
      DO 75 I=1,NM
      DVECR2(I) = ZERO
      SUMR(I)   = ZERO
      DO 75 J=1,3
      DXER(I,J) = ZERO
      DXRMO(I,J)= ZERO
      DXM(I,J)   = ZERO
   75 CONTINUE
!
      VSEC1=TWO/RLVAL(ICBDGM-11)
      VSEC2=TWO/FNSEC(ICHBOG(2,3))
      VSEC4=TWO/FNSEC(ICHBOG(2,10))
! COMPUTE TIME DIFFERENCES
      IF(LTWO) THEN
      DO 80 I=1,NM
      DELTT(I)=AA(JFSCK2+I-1)-AA(JFSCK1+I-1)
      DELMOT(I)=DELTT(I)*VSEC4
      DELEMT(I)=DELTT(I)*VSEC2
   80 END DO
      ENDIF
      DO 90 I=1,NM
      DELTR(I)=AA(JFSCN2+I-1)-AA(JFSCN1+I-1)
      DELTS(I)=(AA(JFSCM2+I-1)-AA(JFSCM1+I-1))*VSEC1
      DELMOR(I)=DELTR(I)*VSEC4
      DELEMR(I)=DELTR(I)*VSEC2
   90 END DO
!*********DEBUG*******************
!     WRITE(6,*) ' DELTR ',DELTR(1)
!     WRITE(6,*) ' DELTT ',DELTT(1)
!     WRITE(6,*) ' DELTS ',DELTS(1)
!***********************************
!
! GET CHEB DIFFERENCES
      IF(LTWO) THEN
      CALL CHEBDF(CBT1MO,CBT2MO,DELMOT,NM,IORMO)
      CALL CHEBDF(CHEBT1,CHEBT2,DELEMT,NM,IORTB)
      ENDIF
      CALL CHEBDF(CBR1MO,CBR2MO,DELMOR,NM,IORMO)
      CALL CHEBDF(CHEBR1,CHEBR2,DELEMR,NM,IORTB)
      CALL CHEBDF(CHEBS1,CHEBS2,DELTS, NM,IORCB)
!*********DEBUG*******************
!     WRITE(6,*) ' CBT2MO',CBT2MO(1,3)
!     WRITE(6,*) ' CBR2MO',CBR2MO(1,3)
!     WRITE(6,*) ' CHEBT2',CHEBT2(1,3)
!     WRITE(6,*) ' CHEBR2',CHEBR2(1,3)
!     WRITE(6,*) ' CHEBS2',CHEBS2(1,3)
!***********************************
!*******************************************************
!
! NOTE: If the rrr is not correct in INTPRX, chances are that COEFM whic
!       contains the coefficients for central body are not correct or XM
!       (CPOSS in MTSTST) is not correct. COEFM is computed in COEFFX an
!       CPOSS in BARYCX
!     DO I=1,IORCB
!     WRITE(6,*)' dbg INTPRX COEFM ',COEFM(1,I),COEFM(2,I),COEFM(3,I)
!     ENDDO
! COMPUTE PLANET RANGE DIFFRENCES BETWEEN POINT 1 AND POINT 2
      DO 460 I=1,3
      DO 200 J=2,IORMO
      JJ=IORMO+2-J
      DO 250 K=1,NM
      DXRMO(K,I) = DXRMO(K,I)+COEFMO(I,JJ)*CBR2MO(K,JJ)
      IF(LTWO) THEN
      DXTMO(K,I) = DXTMO(K,I)+COEFMO(I,JJ)*CBT2MO(K,JJ)
      ENDIF
  250 END DO
  200 END DO
!
      DO 300 J=2,IORTB
      JJ=IORTB+2-J
      DO 325 K=1,NM
      DXER(K,I) = DXER(K,I)+COEFEM(I,JJ)*CHEBR2(K,JJ)
      IF(LTWO) THEN
      DXET(K,I) = DXET(K,I)+COEFEM(I,JJ)*CHEBT2(K,JJ)
      ENDIF
  325 END DO
  300 END DO
      DO 350 K=1,NM
      IF(LTWO) THEN
      DXET(K,I) = DXET(K,I)-DXTMO(K,I)
      ENDIF
      DXER(K,I) = DXER(K,I)-DXRMO(K,I)
  350 END DO
!
      DO 400 J=2,IORCB
      JJ=IORCB+2-J
      DO 400 K=1,NM
      DXM(K,I)  = DXM(K,I) +COEFM(I,JJ)*CHEBS2(K,JJ)
  400 CONTINUE
!
!**********DEBUG*********
!     WRITE(6,*) ' XSTT',XSTT1(1,1),XSTT2(1,1)
!     WRITE(6,*) ' XSTR',XSTR1(1,1),XSTR2(1,1)
!     WRITE(6,*) ' XS',XS1(1,1),XS2(1,1)
!     WRITE(6,*) ' XET1',XET1(1,1)
!     WRITE(6,*) ' XM1',XM1(1,1)
!     STOP
!*****************
! COMPUTE SATELLITE AND STATION MOVEMENTS BETWEEN POINTS 1 AND 2
      DO 450 K=1,NM
      IF(LTWO) THEN
      DSTT(K,I) = XSTT2(K,I)-XSTT1(K,I)
      ENDIF
      DS(K,I)   = XS2(K,I)-XS1(K,I)
      DSTR(K,I) = XSTR2(K,I)-XSTR1(K,I)
!
! COMPUTE RANGE VECTOR AT POINT 1
       IF(LTWO) THEN
      VECT(K,I) =-XET1(K,I)-XSTT1(K,I)+XM1(K,I)                         &
     &              +XS1(K,I)
      ENDIF
      VECR(K,I) =-XER1(K,I)-XSTR1(K,I)+XM1(K,I)                         &
     &              +XS1(K,I)
!
! COMPUTE RELATIVE VECTOR FROM POINT 1 TO POINT 2
      IF(LTWO) THEN
      DVECT(K,I) =-DXET(K,I)-DSTT(K,I)+DXM(K,I)                         &
     &               +DS(K,I)
      DVECT2(K)  = DVECT2(K)+DVECT(K,I)*DVECT(K,I)
      ENDIF
      DVECR(K,I) =-DXER(K,I)-DSTR(K,I)+DXM(K,I)                         &
     &               +DS(K,I)
      DVECR2(K)  = DVECR2(K)+DVECR(K,I)*DVECR(K,I)
  450 END DO
!
  460 END DO
!
! COMPUTE RELATIVE VECTOR LENGTH
      DO 475 K=1,NM
      IF(LTWO) THEN
      BETAT(K)= SQRT(DVECT2(K))
      ENDIF
      BETAR(K)= SQRT(DVECR2(K))
  475 END DO
!
! COMPUTE UNIT VECTOR
      DO 500 I=1,3
      DO 500 K=1,NM
      IF(LTWO) THEN
      AT(K,I) = DVECT(K,I)/BETAT(K)
      SUMT(K) = SUMT(K)+AT(K,I)*VECT(K,I)
      ENDIF
      AR(K,I) = DVECR(K,I)/BETAR(K)
      SUMR(K) = SUMR(K)+AR(K,I)*VECR(K,I)
  500 CONTINUE
!
! COMPUTE DIRECTIONAL DERIVATIVES
      DO 600 K=1,NM
      IF(LTWO) THEN
      RANGE2(K) = RANGET(K)*RANGET(K)
      RANGE4(K) = RANGE2(K)*RANGE2(K)
      FT(K,1) = SUMT(K)/RANGET(K)
      FT(K,2) = (RANGET(K)-SUMT(K)*SUMT(K)/RANGET(K))                   &
     &             /RANGE2(K)
      FT(K,3) = ((-ONE-TWO*(AT(K,1)+AT(K,2)+AT(K,3)))*                  &
     &             RANGET(K)*SUMT(K)+THREE                              &
     &             *SUMT(K)*SUMT(K)*SUMT(K)/RANGET(K))                  &
     &             /RANGE4(K)
      ENDIF
      RANGE2(K) = RANGER(K)*RANGER(K)
      RANGE4(K) = RANGE2(K)*RANGE2(K)
      FR(K,1) = SUMR(K)/RANGER(K)
      FR(K,2) = (RANGER(K)-SUMR(K)*SUMR(K)/RANGER(K))                   &
     &             /RANGE2(K)
      FR(K,3) = ((-ONE-TWO*(AR(K,1)+AR(K,2)+AR(K,3)))*                  &
     &             RANGER(K)*SUMR(K)+THREE*SUMR(K)*                     &
     &             SUMR(K)*SUMR(K)/RANGER(K))/RANGE4(K)
!
!*************DEBUG********
!     WRITE(6,*) 'IN INTPRR'
!     WRITE(6,*) ' NM ',NM
!     PRINT 771, NM
! 771 FORMAT(' NM ',I5)
!     KKOUNT=KKOUNT+1
!     IF(KKOUNT .GE. 190.AND.KKOUNT.LE.192)THEN
!     DO 777 M=1,NM
!     WRITE(6,*) ' FT ',FT(1,1),FT(1,2),FT(1,3)
!     WRITE(6,*) ' FR ',FR(1,1),FR(1,2),FR(1,3)
!     WRITE(6,*) ' BETAT&R ',BETAT(1),BETAR(1)
! 777 CONTINUE
!     ENDIF
!********************
! COMPUTE RANGE RATE
      RRR(K) = FR(K,2)+FR(K,3)*BETAR(K)/THREE
      RRR(K) = FR(K,1)+BETAR(K)*RRR(K)/TWO
      RRR(K) = -RRR(K)*BETAR(K)
      IF(LTWO) THEN
      RRT(K) = FT(K,2)+FT(K,3)*BETAT(K)/THREE
      RRT(K) = FT(K,1)+BETAT(K)*RRT(K)/TWO
      RRT(K) = RRR(K)-RRT(K)*BETAT(K)
      RRR(K) = RRT(K)/TWO
      ENDIF
  600 END DO
!
      RETURN
      END
