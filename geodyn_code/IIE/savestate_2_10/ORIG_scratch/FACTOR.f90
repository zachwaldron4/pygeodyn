!$FACTOR
      FUNCTION FACTOR(KF,RI,RNIU,RP)
!********1*********2*********3*********4*********5*********6*********7**
! FACTOR           07/08/94            9408.0    PGMR - N. PAVLIS
!
!  FUNCTION  :
!  FUNCTION S/R TO EVALUATE THE NODAL FACTORS FOR THE LUNAR TIDES.
!
!  THE NODAL FACTORS ARE COMPUTED ACCORDING TO THE FORMULATION IN:
!  'MANUAL OF HARMONIC ANALYSIS AND PREDICTION OF TIDES'
!  BY P. SCHUREMAN (U.S. COAST AND GEODETIC SURVEY SPECIAL PUBLICATION
!
! I/O PARAMETERS:
!
!     INPUT :   KF : INDEX IDENTIFYING THE EQUATION TO BE USED
!     -----          (KF DEPENDS ON THE CONSTITUENT IN QUESTION)
!               RI,RNIU,RP (FROM S/R DOODSN)..RADIANS
!
!    OUTPUT :   FACTOR : NODAL FACTOR
!    ------
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   KF       I    S    INDEX IDENTIFYING THE EQUATION TO BE USED
!                      (KF DEPENDS ON THE CONSTITUENT INQUESTION)
!                       RI,RNIU,RP (FROM S/R DOODSN)..RADIANS
!   FACTOR   O    S     NODAL FACTOR
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      X05=COS(2.D0*RP)
      X07=SIN(RI)
      X27=X07*X07
      X08=COS(RI)
      X09=2.D0*X07*X08
      X11=COS(RNIU)
      X12=(1.D0+X08)/X07
      X14=2.D0*X11*X11-1.D0
      X15=(1.D0-X08)/2.D0
      X16=(1.D0+X08)/2.D0
!-----------------------------------------------------------------------
!  FIND '1/QA' AND '1/RA' (EQS. 197 AND 213)
!-----------------------------------------------------------------------
      RECQA=SQRT(2.310D0+1.435D0*X05)
      RECRA=SQRT(1.D0-12.D0/(X12*X12)*X05+36.D0/(X12*X12*X12*X12))
!-----------------------------------------------------------------------
!  FIND NODE FACTOR
!-----------------------------------------------------------------------
      IF(KF.EQ. 0) FACTOR=1.D0
      IF(KF.EQ. 1) FACTOR=(2.D0/3.D0-X27)/0.5021D0
      IF(KF.EQ. 2) FACTOR=X27/0.1578D0
      IF(KF.EQ. 3) FACTOR=X07*X16/0.3800D0
      IF(KF.EQ. 4) FACTOR=X09/0.7214D0
      IF(KF.EQ. 5) FACTOR=X07*X15/0.0164D0
      IF(KF.EQ. 6) FACTOR=X16*X16/0.9154D0
      IF(KF.EQ. 7) FACTOR=X27/0.1565D0
      IF(KF.EQ. 8) FACTOR=X15*X15/0.0017D0
      IF(KF.EQ. 9) FACTOR=X07*X16/0.3800D0*RECQA
      IF(KF.EQ.10) FACTOR=SQRT(0.8965D0*X09*X09+0.6001D0*X09*X11+      &
     &                          0.1006D0)
      IF(KF.EQ.11) FACTOR=X16*X16/0.9145D0*RECRA
      IF(KF.EQ.12) FACTOR=SQRT(19.0444D0*X27*X27+2.7702D0*X27*X14+     &
     &                           0.0981D0)
!-----------------------------------------------------------------------
      RETURN
      END
