!$AANDBO
      SUBROUTINE AANDBO(AAA,BBB,NNN,ICENTR,COSEPH,SINEPH,THGPI2)
!********1*********2*********3*********4*********5*********6*********7**
! AANDBO           89/05/23            0000.0    PGMR - DAVID MORSBERGER
!                                      0000.0    PGMR - A. MARSHALL
!
! FUNCTION:  EVALUATE THE TIDAL AMPLITUDE AND ANGULAR ARG.
!            FOR THE SIDE BAND OTIDE CONSTITUENTS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AAA     O          GROUP TIDAL AMPLITUDE
!   BBB     O          GROUP TIDAL AMPLITUDE
!   NNN     I          TIDAL DEGREE IN HARMONIC EXPANSION
!   ICENTR  I          POINTER TO THE GROUP OF SIDE BANDS
!   COSEPH  I          COSINE OF LONGITUDE
!   SINEPH  I          SINE OF LONGITUDE
!   THGPI2  I          THETG + PI/2
!
!  COMMENTS:
!            ROUTINE IS CALLED FOR EACH MAIN LINE CONSTITUENT
!
!  REFERENCES:
!             NASA TECH MEMO 86180, "ALTIMETRY, ORBITS AND TIDES",
!             OSCAR L. COLOMBO
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      INTEGER HHH,SIGN1
!
! NUMOT IS NUMBER OF DATA INITIALIZED TIDES.  THIS INCLUDES MAIN LINES
! AND SIDEBANDS. NUMGRP IS NUMBER OF TIDAL GROUPS.
! NUMET  MUST BE INCREASED IF NEW TIDES ARE ADDED TO LIST
! NUMGRP & NUMET MUST BE INCREASED IF NEW GROUPS ARE ADDED TO LIST
! DIMENSIONS MUST ALSO BE INCREASED IN SUBROUTINE PRESET
      PARAMETER (NUMOT=87,NUMGRP=18)
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/ENDPTS/IPTEND(18)
      COMMON/STARTT/ESSTRT,FSSTRT
      COMMON/TDELEM/XMEAN(2),XNODE(2),OMEGA(2),A(2),XINCL(2),E(2),      &
     &   EPSLON,RMASS,XLONG,TANPSI,THETG,RHO3,CC(3,3),B(2)
      COMMON/VRBLOK/SINPSI,COSPSI,XLAMDA,GMR,AOR
      DIMENSION MMM(NUMOT),KKK(NUMOT),HHH(NUMOT),JJJ(NUMOT),            &
     &     JJBDY(NUMOT),SIGN1(NUMOT)
      DIMENSION XMMM(NUMOT),XKKK(NUMOT),XHHH(NUMOT),XJJJ(NUMOT),        &
     &     FII(2,NUMOT),GEE(2,NUMOT),DOOD(NUMOT),                       &
     &     SRC(NUMOT),XSIGN1(NUMOT),SCRTCH(NUMOT,3)
      DIMENSION DODI(NUMGRP),IPTBEG(NUMGRP)
!
! INITIALIZE THE DEMOS NUMBER FOR THE TIDE BANDS WITH THE
! CENTRAL FREQUENCIES FIRST
      DATA SIGN1/                                                       &
     &           -1,1,                                                  &
     &           1,-1,-1,1,1,-1,-1,1,1,                                 &
     &           -1,-1,-1,1,1,1,-1,-1,1,                                &
     &           1,-1,1,1,-1,1,1,1,                                     &
     &           1,1,                                                   &
     &           1,-1,-1,1,1,                                           &
     &           5*1,-1,-1,1,                                           &
     &           1,1,                                                   &
     &           1,-1,                                                  &
     &           -1,1,-1,1,                                             &
     &           1,1,1,1,                                               &
     &           1,1,1,-1,1,1,                                          &
     &           1,-1,1,1,-1,-1,                                        &
     &           -1,1,1,-1,1,1,                                         &
     &           1,1,                                                   &
     &           1,1,                                                   &
     &           1,1,-1,1,1,1,                                          &
     &           1,-1,1,1/
      DATA MMM/                                                         &
     &         2*0,                                                     &
     &         9*0,                                                     &
     &         9*0,                                                     &
     &         8*1,                                                     &
     &         2*1,                                                     &
     &         5*1,                                                     &
     &         8*2,                                                     &
     &         2*2,                                                     &
     &          2*0,                                                    &
     &         4*0,                                                     &
     &         4*1,                                                     &
     &         6*1,                                                     &
     &         6*1,                                                     &
     &         6*1,                                                     &
     &         2*2,                                                     &
     &         2*2,                                                     &
     &         6*2,                                                     &
     &         4*2/
      DATA KKK/                                                         &
     &         2,0,                                                     &
     &         0,2,1,1,2,2,1,0,1,                                       &
     &         2,2,1,0,1,2,1,0,1,                                       &
     &         2,1,0,1,1,0,1,2,                                         &
     &         2,0,                                                     &
     &         0,2,1,1,2,                                               &
     &         2,0,1,1,0,1,1,2,                                         &
     &         2,0,                                                     &
     &           0,2,                                                   &
     &           2,0,1,0,                                               &
     &           2,0,1,0,                                               &
     &           0,1,2,1,1,2,                                           &
     &           0,1,1,2,2,1,                                           &
     &           2,0,1,1,0,1,                                           &
     &           2,1,                                                   &
     &           2,1,                                                   &
     &           2,1,1,0,1,2,                                           &
     &           0,1,1,2/
      DATA HHH/                                                         &
     &         0,1,                                                     &
     &         1,1,1,1,1,0,0,2,2,                                       &
     &         0,1,1,1,1,1,0,0,2,                                       &
     &         0,2,0,0,1,1,1,1,                                         &
     &         0,1,                                                     &
     &         1,1,1,1,1,                                               &
     &         0,1,0,1,0,2,1,1,                                         &
     &         0,1,                                                     &
     &           1,0,                                                   &
     &           0,1,0,2,                                               &
     &           0,0,0,1,                                               &
     &           1,0,0,1,1,1,                                           &
     &           1,1,1,1,0,0,                                           &
     &           0,1,1,0,2,2,                                           &
     &           0,0,                                                   &
     &           0,0,                                                   &
     &           0,0,1,1,1,1,                                           &
     &           1,1,1,1/
      DATA JJJ/                                                         &
     &         0,-2,                                                    &
     &         -1,1,1,-1,-1,-1,-1,1,1,                                  &
     &         0,2,2,-2,-2,-2,0,0,0,                                    &
     &         0,0,0,0,-2,2,2,2,                                        &
     &         0,2,                                                     &
     &         0,0,0,0,0,                                               &
     &         0,2,0,2,0,0,-2,2,                                        &
     &         0,2,                                                     &
     &           -1,-1,                                                 &
     &           1,-3,1,-1,                                             &
     &           1,1,1,3,                                               &
     &           1,-1,-1,-1,1,1,                                        &
     &           -1,1,-1,-1,-1,-1,                                      &
     &           0,-2,-2,0,0,0,                                         &
     &           2,2,                                                   &
     &           1,1,                                                   &
     &           -1,-1,-1,1,1,1,                                        &
     &           0,0,0,0/
      DATA JJBDY/                                                       &
     &           2,2,                                                   &
     &           1,1,1,1,1,1,1,1,1,                                     &
     &           1,1,1,1,1,1,1,1,1,                                     &
     &           1,1,1,1,1,1,1,1,                                       &
     &           2,2,                                                   &
     &           1,1,1,1,1,                                             &
     &           1,1,1,1,1,1,1,1,                                       &
     &           2,2,                                                   &
     &           2,2,                                                   &
     &           1,1,1,1,                                               &
     &           1,1,1,1,                                               &
     &           1,1,1,1,1,1,                                           &
     &           1,1,1,1,1,1,                                           &
     &           1,1,1,1,1,1,                                           &
     &           1,1,                                                   &
     &           1,1,                                                   &
     &           1,1,1,1,1,1,                                           &
     &           1,1,1,1/
!
! IPTBEG = POINTERS TO BEGINNING OF EACH TIDAL GROUP
! DODI   = INVERSE DOODSON COEFFICIENT
! GDINV  = INVERSE DOODSON CONSTANT
!
      DATA IPTBEG/1, 3,12,21,29,31,36,44,46,48,52,56,62,68,74,76,78,84/
! NOTE: DODI ASSUMES ALL SYMMETRIES EXCEPT K1, K2 FOR H=1,J=0,K=0
      DATA DODI/13.726875507720D0,12.8348308129255D0,                   &
     &           6.387551573886D0, 2.6505852418737D0,                   &
     &           5.696118961535D0,-0.94342473363535D0,                  &
     &           1.099889690621D0, 2.3636676245561D0,                   &
     &          87.495732607770D0,35.3490010027100D0,                   &
     &          14.668459313239D0,-35.754593357858D0,                   &
     &         -35.754593357858D0,-61.572537943856D0,                   &
     &          48.592859781030D0, 6.0868395858564D0,                   &
     &         -42.373248787448D0, 4.3513094893125D0/
      DATA GDINV/1.1450630305643D0/
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
! GET THE POINTERS TO TIDE BAND ARRAYS
      IBEG=IPTBEG(ICENTR)
      IEND=IPTEND(ICENTR)
      DODINV=DODI(ICENTR)
      NUMTID=IEND-IBEG+1
! SWITCH BAND VALUES INTO REAL ARRAYS
      DO 100 J=1,NUMTID
      JJ=J+IBEG-1
      XSIGN1(J)=DBLE(SIGN1(JJ))
      XMMM(J)=DBLE(MMM(JJ))
      XKKK(J)=DBLE(KKK(JJ))
      XHHH(J)=DBLE(HHH(JJ))
      XJJJ(J)=DBLE(JJJ(JJ))
  100 END DO
! SET TEMP PARAMETERS FOR FINCL AND GEXC
      IT1=NET
      IT2=NOTADJ
      IT3=NETADJ
      NETADJ=0
      NET=NUMTID
      NOTADJ=0
! COMPUTE INCLINATION FUNCTION
      CALL FINCL(2,KKK(IBEG),HHH(IBEG),FII(1,1))
! COMPUTE ECCENTRICITY FUNCTION
      CALL GEXC(2,HHH(IBEG),JJJ(IBEG),GEE(1,1))
! RESTORE OLD PARAMETERS
      NET=IT1
      NOTADJ=IT2
      NETADJ=IT3
      AAA=ZERO
      BBB=ZERO
      DO 200 J=1,NUMTID
       JJ=J+IBEG-1
! COMPUTE DOODSON COEF FOR SIDE BANDS
       DOOD(J)=ADOODO(COSEPH,SINEPH,NNN,MMM(JJ),KKK(JJ),HHH(JJ),JJJ(JJ),&
     &     JJBDY(JJ),SIGN1(JJ),FII(1,J),GEE(1,J))
  200 END DO
! LOAD KEPLERIAN ELEMENTS
      DO 300 J=1,NUMTID
       JJ=J+IBEG-1
       SCRTCH(J,1)=OMEGA(JJBDY(JJ))
       SCRTCH(J,2)=XMEAN(JJBDY(JJ))
  300  SCRTCH(J,3)=XNODE(JJBDY(JJ))
!
! COMPUTE ARGUMENT FOR SIDE BANDS
      DO 400 J=1,NUMTID
       SRC(J)=-((TWO-TWO*XHHH(J))*SCRTCH(J,1)+                          &
     &          (TWO-TWO*XHHH(J)+XJJJ(J))*SCRTCH(J,2)+                  &
     &           XKKK(J)*SCRTCH(J,3))*XSIGN1(J)+                        &
     &           XMMM(J)*THGPI2
  400 END DO
! COMPUTE INVERSE DOODSON * DOODSON OF SIDEBAND
      DO 500 J=1,NUMTID
       SCRTCH(J,1)=ABS(DOOD(J)*DODINV*GDINV)
! COMPUTE DIFFERENCE OF SIDE BAND AND DOMINANT ARGUMENTS
       SCRTCH(J,2)=COS(SRC(J)-SRC(1))
       SCRTCH(J,3)=SIN(SRC(J)-SRC(1))
! COMPUTE A AND B
       AAA= SCRTCH(J,1)*SCRTCH(J,2)+AAA
       BBB=-SCRTCH(J,1)*SCRTCH(J,3)+BBB
  500 END DO
      RETURN
      END
