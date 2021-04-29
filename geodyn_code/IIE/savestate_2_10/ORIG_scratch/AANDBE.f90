!$AANDBE
      SUBROUTINE AANDBE(AAA,BBB,NNN,ICENTR,COSEPH,SINEPH,THGPI2)
!********1*********2*********3*********4*********5*********6*********7**
! AANDBE           89/05/23            0000.0    PGMR - DAVID MORSBERGER
!                                      0000.0    PGMR - A MARSHALL
!
!  FUNCTION: EVALUATE THE TIDAL AMPLITUDE AND ANGULAR ARG.
!            FOR THE SIDE BAND ETIDE CONSTITUENTS.
!
!  I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AAA     I/O        DOODSON COEFFICIENT OF CENTRAL BAND
!                      AMPLITUDE FOR TIDAL GROUP
!   BBB     I/O        ZERO
!                      AMPLITUDE FOR TIDAL GROUP
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
! NUMET IS NUMBER OF DATA INITIALIZED TIDES.  THIS INCLUDES MAIN LINES
! AND SIDEBANDS. NUMGRP IS NUMBER OF TIDAL GROUPS.
! NUMET  MUST BE INCREASED IF NEW TIDES ARE ADDED TO LIST
! NUMGRP & NUMET MUST BE INCREASED IF NEW GROUPS ARE ADDED TO LIST
! DIMENSIONS MUST ALSO BE INCREASED IN SUBROUTINE PRESET
      PARAMETER (NUMET=20,NUMGRP=4)
!
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/TDELEM/XMEAN(2),XNODE(2),OMEGA(2),A(2),XINCL(2),E(2),      &
     &   EPSLON,RMASS,XLONG,TANPSI,THETG,RHO3,CC(3,3),B(2)
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/VRBLOK/SINPSI,COSPSI,XLAMDA,GMR,AOR
      DIMENSION MMM(NUMET),KKK(NUMET),HHH(NUMET),JJJ(NUMET),            &
     &     JJBDY(NUMET),SIGN1(NUMET)
      DIMENSION XMMM(NUMET),XKKK(NUMET),XHHH(NUMET),XJJJ(NUMET),        &
     &     FII(2,NUMET),GEE(2,NUMET),DOOD(NUMET),                       &
     &     SRC(NUMET),XSIGN1(NUMET),SCRTCH(NUMET,3)
      DIMENSION IPTBEG(NUMGRP),IPTEND(NUMGRP)
!
! INITIALIZE THE DEMOS NUMBER FOR THE TIDE BANDS WITH THE
! CENTRAL FREQUENCIES FIRST
      DATA SIGN1/                                                       &
     &           1,-1,1,1,-1,1,1,1,                                     &
     &           1,1,                                                   &
     &           5*1,-1,-1,1,                                           &
     &           1,1/
      DATA MMM/                                                         &
     &         8*1,                                                     &
     &         2*1,                                                     &
     &         8*2,                                                     &
     &         2*2/
      DATA KKK/                                                         &
     &         2,1,0,1,1,0,1,2,                                         &
     &         2,0,                                                     &
     &         2,0,1,1,0,1,1,2,                                         &
     &         2,0/
      DATA HHH/                                                         &
     &         0,2,0,0,1,1,1,1,                                         &
     &         0,1,                                                     &
     &         0,1,0,1,0,2,1,1,                                         &
     &         0,1/
      DATA JJJ/                                                         &
     &         0,0,0,0,-2,2,2,2,                                        &
     &         0,2,                                                     &
     &         0,2,0,2,0,0,-2,2,                                        &
     &         0,2/
      DATA JJBDY/                                                       &
     &           1,1,1,1,1,1,1,1,                                       &
     &           2,2,                                                   &
     &           1,1,1,1,1,1,1,1,                                       &
     &           2,2/
!
! IPTBEG = POINTERS TO BEGINNING OF EACH TIDAL GROUP
! IPTEND = POINTERS TO END OF EACH TIDAL GROUP
      DATA IPTBEG/1, 9,11,19/
      DATA IPTEND/8,10,18,20/
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
! IDENTITFY POINTERS FOR BANDS
      IBEG=IPTBEG(ICENTR)
      IEND=IPTEND(ICENTR)
      NUMTID=IEND-IBEG+1
!
! STORE REAL VALUES OF DEMOS NUMBER
      DO 100 J=1,NUMTID
      JJ=J+IBEG-1
      XSIGN1(J)=DBLE(SIGN1(JJ))
      XMMM(J)  =DBLE(MMM(JJ))
      XKKK(J)  =DBLE(KKK(JJ))
      XHHH(J)  =DBLE(HHH(JJ))
      XJJJ(J)  =DBLE(JJJ(JJ))
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
!
      DO 200 J=2,NUMTID
      JJ=J+IBEG-1
! COMPUTE DOODSON COEF FOR SIDE BANDS
      DOOD(J)=ADOOD(COSEPH,SINEPH,NNN,MMM(JJ),KKK(JJ),HHH(JJ),JJJ(JJ),  &
     &               JJBDY(JJ),SIGN1(JJ),FII(1,J),GEE(1,J))
  200 END DO
!
! LOAD KEPLERIAN ELEMENTS
      DO 300 J=1,NUMTID
       JJ=J+IBEG-1
       SCRTCH(J,1)=OMEGA(JJBDY(JJ))
       SCRTCH(J,2)=XMEAN(JJBDY(JJ))
       SCRTCH(J,3)=XNODE(JJBDY(JJ))
  300 END DO
! COMPUTE ANGULAR ARGUMENT FOR REFERENCE LINE
       SRC1=-((TWO-TWO*XHHH(1))*SCRTCH(1,1)+                            &
     &        (TWO-TWO*XHHH(1)+XJJJ(1))*SCRTCH(1,2)+                    &
     &         XKKK(1)*SCRTCH(1,3))*XSIGN1(1)+                          &
     &         XMMM(1)*THGPI2
! COMPUTE ANGULAR ARGUMENT FOR SIDE BANDS
       DO 400 J=2,NUMTID
       SRC(J)=-((TWO-TWO*XHHH(J))*SCRTCH(J,1)+                          &
     &     (TWO-TWO*XHHH(J)+XJJJ(J))*SCRTCH(J,2)+                       &
     &             XKKK(J)*SCRTCH(J,3))*XSIGN1(J)+                      &
     &             XMMM(J)*THGPI2
  400 END DO
! COMPUTE DIFFERENCE OF SIDE BAND AND DOMINANT ARGUMENTS
       DO 500 J=2,NUMTID
       SCRTCH(J,2)=COS(SRC(J)-SRC1)
       SCRTCH(J,3)=SIN(SRC(J)-SRC1)
! COMPUTE A AND B
       AAA=DOOD(J)*SCRTCH(J,2)+AAA
       BBB=-DOOD(J)*SCRTCH(J,3)+BBB
  500  CONTINUE
      RETURN
      END
