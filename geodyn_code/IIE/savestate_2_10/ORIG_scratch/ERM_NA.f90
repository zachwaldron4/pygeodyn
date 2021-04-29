      SUBROUTINE     ERM_NA ( MJD, TAI, &
     &                        TRS_TO_CRS, TRS_TO_CRS_DER1, &
     &                        TRS_TO_CRS_DER2, DTRS_TO_CRS_DEOP,  &
     &                        RC2Keps,RC2Kpsi,IUER )
! ************************************************************************
! *                                                                      *
! *   Routine     ERM_NA computes the matrix of transformation from the  *
! *   terrestrial coordinate system to the celestial coordinate system   *
! *   as well as its first and second time derivatives using             *
! *   Newcomb-Andoyer formalism.                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MJD ( INTEGER*4 ) -- Modified Julian data on the midnight.     *
! *                            Units: days.                              *
! *       TAI ( REAL*8    ) -- Moment of time. Units: sec.               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * TRS_TO_CRS      ( REAL*8    ) -- The matrix of transformation from   *
! *                                  the terrestrial reference system to *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI.         *
! *                                  Dimension: 3,3.                     *
! * TRS_TO_CRS_DER1 ( REAL*8    ) -- First time derivative of the        *
! *                                  transformation matrix from the      *
! *                                  terrestrial reference system to     *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI.         *
! *                                  Dimension: 3,3.                     *
! * TRS_TO_CRS_DER2 ( REAL*8    ) -- Second time derivative of the       *
! *                                  transformation matrix from the      *
! *                                  terrestrial reference system to     *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI.         *
! *                                  Dimension: 3,3.                     *
! * DTRS_TO_CRS_DEOP ( REAL*8   ) -- Arrays of time derivatives of the   *
! *                                  transformation matrix from the      *
! *                                  terrestrial reference system to     *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI with     *
! *                                  respect to the small vector of      *
! *                                  rotation with Euler angles E1, E2,  *
! *                                  end E3. Dimension: 3,3,3. The last  *
! *                                  dimension runs over E1, E2 and E3   *
! *                                  Euler angles.                       *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 08-DEC-2003   VTD_ERM_NA   v2.1 (c) L. Petrov  21-SEP-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  MJD, IVRB,  IUER, IJ
      DOUBLE PRECISION TAI,   &
     &       TRS_TO_CRS(3,3), TRS_TO_CRS_DER1(3,3),  &
     &       TRS_TO_CRS_DER2(3,3), &
     &        DTRS_TO_CRS_DEOP(3,3,3)

!  PRECS(3,3,2)-THE PRECESSION PORTION OF THE COMPLETE TRS_TO_CRS
!               ROTATION MATRIX AND ITS CT TIME DERIVATIVE. (UNITLESS, 1/SEC)
      DOUBLE PRECISION PRECS(3,3,2),NUTAT(3,3,2),RW2K(3,3,2)
      DOUBLE PRECISION RNCeps(3,3,2),RN1eps(3,3),RC2Keps(3,3,2)
      DOUBLE PRECISION RSCpsi(3,3,3)
      DOUBLE PRECISION RNCpsi(3,3,2),RN2psi(3,3),RC2Kpsi(3,3,2)
      DOUBLE PRECISION RC2K1(3,3),RC2K2(3,3)
      DOUBLE PRECISION CEPSA,SEPSA
      DOUBLE PRECISION  DPRDT1(3,3),DPRDT2(3,3),DPRDT3(3,3)
      DOUBLE PRECISION  DNUDT1(3,3),DNUDT2(3,3),DNUDT3(3,3)
      DOUBLE PRECISION  DPMDT1(3,3),DPMDT2(3,3),DPMDT3(3,3)
      DOUBLE PRECISION R2001e(3,3), R2002e(3,3), R2003e(3,3), R2004e(3,3)
      DOUBLE PRECISION R2p1a(3,3),R2p1b(3,3),R2p1(3,3),R2p2a(3,3), R2p2b(3,3), &
     &       R2p2(3,3),R2p3a(3,3),R2p3b(3,3),R2p3(3,3),  R2p4a(3,3), &
     &       R2p4b(3,3), R2p4(3,3)
      DOUBLE PRECISION xparcsec,yparcsec
      CHARACTER  STR*32
      INCLUDE 'COMMON_DECL.inc'
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/EOPRAT/ETAG,THTAG,ZTAG,EPSMG,DPSIG,EPSTG,TETAG,XPOLE,   &
     &     YPOLE,ETAR,THTAR,ZTAR,EPSMR,DPSIR,EPSTR,THETAGR,XPOLER,   &
     &     YPOLER,SP2,DSP2
!
      DOUBLE PRECISION  DZETA, TETA, ZA, EPS_0, &
     &       DZETA__ARG(0:3), TETA__ARG(0:3), ZA__ARG(0:3), &
     &       EPS__ARG(0:3), OMEGA__ARG(0:3),    &
     &       TARG_TAI, TARG_TDB, TDB, S_ANG, DEPS, E1, E2, &
     &       DPSI_RATE, DEPS_RATE, E1_RATE, E2_RATE, &
     &       DZETA_RATE, TETA_RATE, ZA_RATE, EPS_0_RATE, OMEGA_RATE, &
     &       S_ANG_RATE
      DOUBLE PRECISION RTM_YWOB(3,3),  RTM_XWOB(3,3),  RTM_DIU(3,3),    &
     &       RTM_N1(3,3),    RTM_N2(3,3),    RTM_N3(3,3),     &
     &       RTM_P1(3,3),    RTM_P2(3,3),    RTM_P3(3,3),     &
     &       RTM_TMP(3,3),   RTM_E1(3,3),    RTM_E2(3,3),     &
     &       DRTM_YWOB(3,3), DRTM_XWOB(3,3), DRTM_DIU(3,3),   &
     &       DRTM_N1(3,3),   DRTM_N2(3,3),   DRTM_N3(3,3),    &
     &       DRTM_P1(3,3),   DRTM_P2(3,3),   DRTM_P3(3,3),    &
     &       DRTM_1(3,3),    DRTM_2(3,3),    DRTM_3(3,3),     &
     &       DRTM_4(3,3),    DRTM_5(3,3),    DRTM_6(3,3),     &
     &       DRTM_7(3,3),    DRTM_8(3,3),    DRTM_9(3,3),     &
     &       SRTM_DIU(3,3),  SRTM_1(3,3),    SRTM_2(3,3),     &
     &       SRTM_3(3,3),    SRTM_4(3,3),    SRTM_5(3,3),     &
     &       SRTM_6(3,3),    SRTM_7(3,3),    SRTM_8(3,3),     &
     &       RTM_S(3,3),     DRTM_S(3,3),    PRTM_s(3,3)
      DOUBLE PRECISION PRTM_DIU(3,3),PRTM_XWOB(3,3),PRTM_YWOB(3,3),PRTM_SP2(3,3)
!     REAL*8 HEO_VEC(3),   HEO_VEC_DER1(3),   HEO_VEC_DER2(3)
!     REAL*8 HEO_MAT(3,3), HEO_MAT_DER1(3,3), HEO_MAT_DER2(3,3)
      DOUBLE PRECISION RTM_1(3,3)
!     REAL*8 E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS
      INTEGER  IER
      INTEGER    J2000__MJD
      PARAMETER  ( J2000__MJD  = 51544       ) ! 2000.01.01_00:00:00

!
      CALL CLEARA(RC2Keps,18)
      CALL CLEARA(RC2Kpsi,18)
!     WRITE(6,*)' DBG EOP FROM GEODYN '
!     WRITE(6,*)' ETAG: ',ETAG
!     WRITE(6,*)' THTAG: ',THTAG
!     WRITE(6,*)' ZTAG: ',ZTAG
!     WRITE(6,*)' EPSMG: ',EPSMG
!     WRITE(6,*)' DPSIG: ',DPSIG
!     WRITE(6,*)' EPSTG: ',EPSTG
!     WRITE(6,*)' TETAG: ',TETAG
!     WRITE(6,*)' XPOLE: ',XPOLE
!     xparcsec=xpole*(180.d0*3600.d0*1000.d0)/pi
!     write(6,*)' dbg xparcsec ',xparcsec
!     WRITE(6,*)' YPOLE: ',YPOLE
!     yparcsec=ypole*(180.d0*3600.d0*1000.d0)/pi
!     write(6,*)' dbg yparcsec ',yparcsec
!     WRITE(6,*)' ETAR: ',ETAR
!     WRITE(6,*)' THTAR: ',THTAR
!     WRITE(6,*)' ZTAR: ',ZTAR
!     WRITE(6,*)' EPSMR: ',EPSMR
!     WRITE(6,*)' DPSIR: ',DPSIR
!     WRITE(6,*)' EPSTR: ',EPSTR
!     WRITE(6,*)' THETAGR: ',THETAGR
!     WRITE(6,*)' XPOLER: ',XPOLER
!     WRITE(6,*)' YPOLER: ',YPOLER
!     WRITE(6,*)'                   '
!
      CALL CLEARA(TRS_TO_CRS_DER1,9)
      CALL CLEARA(TRS_TO_CRS_DER2,9)
      CALL CLEARA(DTRS_TO_CRS_DEOP,9)
!
! --- Compute argument TDB at moment of time MJD/TAI
!
      CALL TAI_TO_TDB  ( MJD, TAI, TDB )
      TARG_TAI = (MJD - J2000__MJD - 0.5D0)*86400.0D0 + TAI
      TARG_TDB = (MJD - J2000__MJD - 0.5D0)*86400.0D0 + TDB
!
! The values below from GEODYN NPNCON
!
           DZETA__ARG(1)=3.542482751561867D-12
           DZETA__ARG(2)=1.314561901364292D-25
           DZETA__ARG(3)=2.776433098255333D-36
!
           TETA__ARG(1)  = 3.079892623955878D-12
           TETA__ARG(2)  =-1.716636502556173D-25
           TETA__ARG(3)  =-6.453301800161981D-36
!
           ZA__ARG(1)    = 3.541046232608779D-12
           ZA__ARG(2)    = 5.172291724451325D-25
           ZA__ARG(3)    = 2.808057100096779D-36
!
           EPS__ARG(1)   = -7.191705826116799D-14
           EPS__ARG(2)   = -1.849047160369022D-27
           EPS__ARG(3)   =  2.796795870172751D-37
!
! -------- From GEODYN via COMMON EOPRAT
!
           DZETA=ETAG
           TETA = THTAG
           ZA=ZTAG
!
! -------- Compute values of time derivatives of Newcomb-Andoyer variables
!
           DZETA_RATE = (3.0D0*DZETA__ARG(3) *TARG_TDB + &
     &                   2.0D0*DZETA__ARG(2))*TARG_TDB + &
     &                   DZETA__ARG(1)
           TETA_RATE  = (3.0D0*TETA__ARG(3) *TARG_TDB  + &
     &                   2.0D0*TETA__ARG(2))*TARG_TDB  + &
     &                   TETA__ARG(1)
           ZA_RATE    = (3.0D0*ZA__ARG(3) *TARG_TDB    + &
     &                   2.0D0*ZA__ARG(2))*TARG_TDB    + &
     &                   ZA__ARG(1)
!
! -------- Compute the angle of mean inclination of ecliptic to the equator
!
           EPS_0 = EPSMG
           EPS_0_RATE = (3.0D0*EPS__ARG(3)*TARG_TDB    + &
     &                   2.0D0* EPS__ARG(2))*TARG_TDB  + &
     &                  EPS__ARG(1)
!
!
! -------- ... And its rate of change
!
!
! -------- Compute rotation matrices
!
!
! in vtd_erm_na.f in directory iie
           CALL VTD_ROTMAT ( 3,  DZETA,        RTM_P1   )
           CALL VTD_ROTMAT ( 2, -TETA,         RTM_P2   )
           CALL VTD_ROTMAT ( 3,  ZA,           RTM_P3   )
           CALL VTD_ROTMAT ( 1, -EPS_0,        RTM_N1   )
           CALL VTD_ROTMAT ( 3,  DPSIG,         RTM_N2   )
!          CALL VTD_ROTMAT ( 1,  EPS_0 + DEPS, RTM_N3   )
           CALL VTD_ROTMAT ( 1,  EPSTG       , RTM_N3   )
           CALL VTD_ROTMAT ( 3, -TETAG,        RTM_DIU  )
           CALL VTD_ROTMAT ( 2,  XPOLE,         RTM_XWOB )
           CALL VTD_ROTMAT ( 1,  YPOLE,         RTM_YWOB )
           IF(SP2.GT.0.D0) THEN
           CALL VTD_ROTMAT ( 3,  SP2,           RTM_S )
           ELSE
           CALL CLEARA(RTM_S,9)
           DO IJ=1,3
           RTM_S(IJ,IJ)=1.D0
           ENDDO
           ENDIF

!      CALL MULTI_MUL_3 (  9, TRS_TO_CRS, RTM_P1,  RTM_P2,   RTM_P3,  &
!    &                                    RTM_N1,  RTM_N2,   RTM_N3,  &
!    &                                    RTM_DIU, RTM_XWOB, RTM_YWOB )
!
      GOTO 220
           WRITE ( 6, 210 ) TRS_TO_CRS(1,1), &
     &                      TRS_TO_CRS(2,1), &
     &                      TRS_TO_CRS(3,1), &
     &                      TRS_TO_CRS(1,2), &
     &                      TRS_TO_CRS(2,2), &
     &                      TRS_TO_CRS(3,2), &
     &                      TRS_TO_CRS(1,3), &
     &                      TRS_TO_CRS(2,3), &
     &                      TRS_TO_CRS(3,3)
 210     FORMAT ( 3('TRS_TO_CRS = ', 3(F15.12,2X)/) )
 220     CONTINUE
!
! -------- Compute derivatices of the rotation matrices
!
           CALL VTD_ROTMAT_DER ( 3,  DZETA, DZETA_RATE, DRTM_P1   )
           CALL VTD_ROTMAT_DER ( 2, -TETA,  TETA_RATE,  DRTM_P2   )
           CALL VTD_ROTMAT_DER ( 3,  ZA,    ZA_RATE,    DRTM_P3   )
           CALL VTD_ROTMAT_DER ( 1, -EPS_0, EPS_0_RATE, DRTM_N1   )
           CALL VTD_ROTMAT_DER ( 3,  DPSIG, DPSIR,  DRTM_N2   )
           CALL VTD_ROTMAT_DER ( 1,  EPSTG, EPSTR,  DRTM_N3   )

! -------- NUTATION PARTIALS --------------------------------------

! -------- Compute intermediate rotation matrices needed for nutation partials
!
!  Precession Part of the total rotation matrix TRS_TO_CRS
      CALL MULTI_MUL_3 (  3, PRECS(1,1,1), RTM_P1,  RTM_P2,   RTM_P3)

!  Nutation Part of the total rotation matrix TRS_TO_CRS
      CALL MULTI_MUL_3 (  3, NUTAT(1,1,1), RTM_N1,  RTM_N2,   RTM_N3)

!   RW2K(3,3,2)-The wobble portion of the total rotation matrix TRS_TO_CRS
      CALL MULTI_MUL_3 (3, RW2K(1,1,1),RTM_S,RTM_XWOB,RTM_YWOB)

! end----- Compute intermediate rotation matrices needed for nutation partials


!  FORM THE MATRIX PC2Keps (partials of TRS_TO_CRS/ wrt epst) needed for
!  nutation partials.

      CALL VTD_ROTMAT_DER ( 1,  EPSTG, 1.D0,  RN1eps   )
      CALL MULTI_MUL_3 (3, RNCeps(1,1,1),RTM_N1,RTM_N2,RN1eps(1,1))

! -------- Partial derivative of TRS_TO_CRS w.r.t. obliquity ----------

      CALL MULTI_MUL_3(4,RC2Keps(1,1,1),PRECS(1,1,1),RNCeps(1,1,1), &
     &               RTM_DIU, RW2K(1,1,1))
! ---------------------------------------------------------------------

!  FORM THE MATRIX PC2Kpsi (partials of TRS_TO_CRS/ wrt dpsi) needed for
!  nutation partials.

      CALL VTD_ROTMAT_DER ( 3,  DPSIG, 1.D0,  RN2psi   )
      CALL MULTI_MUL_3 (3, RNCpsi(1,1,1),RTM_N1,RN2psi(1,1),RTM_N3)

!   Compute partial derivatives of the IERS2000 classical diurnal
!   spin matrix, RTM_DIU, and its first two CT time derivatives w.r.t.
!   DPSI2K(2).

!    RSCpsi(3,3,3) - Partial derivative of RTM_DIU (classical diurnal
!                    spin matrix) w.r.t. longitude (psi).

        CEPSA=COS(EPS_0)
        SEPSA=SIN(EPS_0)

           CALL VTD_ROTMAT_DER ( 3,  -TETAG, -CEPSA,  RSCpsi(1,1,1)   )

      CALL MULTI_MUL_3(4,RC2K1,PRECS(1,1,1),RNCpsi(1,1,1),RTM_DIU,  &
     &                 RW2K(1,1,1))
      CALL MULTI_MUL_3(4,RC2K2,PRECS(1,1,1),NUTAT(1,1,1),RNCpsi(1,1,1),&
     &                 RSCpsi(1,1,1),RW2K(1,1,1))

! ------------ Partial derivative of TRS_TO_CRS  w.r.t. longitude -----

      CALL MULTI_ADD_3 (2, RC2Kpsi(1,1,1) , 1.D0,RC2K1, 1.D0,RC2K2)
! ----------------------------------------------------------------------

!----------------------------------------------------------------------------

           CALL VTD_ROTMAT_DER ( 3, -TETAG,        1.0D0, PRTM_DIU  )
           CALL VTD_ROTMAT_DER ( 2,  XPOLE,         1.0D0, PRTM_XWOB )
           CALL VTD_ROTMAT_DER ( 1,  YPOLE,         1.0D0, PRTM_YWOB )

           IF(SP2.GT.0.D0) THEN
           CALL VTD_ROTMAT_DER ( 1,    SP2,         1.0D0, PRTM_S )
           ELSE
           CALL CLEARA(PRTM_S,9)
           DO IJ=1,3
           PRTM_S(IJ,IJ)=1.D0
           ENDDO
           ENDIF

!
           CALL VEC_MULT_CONSTANT ( PRTM_DIU,  9, THETAGR, DRTM_DIU  )
           CALL VEC_MULT_CONSTANT ( PRTM_XWOB, 9, XPOLER,  DRTM_XWOB )
           CALL VEC_MULT_CONSTANT ( PRTM_YWOB, 9, YPOLER,  DRTM_YWOB )

           IF(SP2.GT.0.D0) THEN
           CALL VEC_MULT_CONSTANT ( PRTM_S , 9, DSP2,    DRTM_S  )
           ELSE
           CALL CLEARA(DRTM_S,9)
           DO IJ=1,3
           DRTM_S(IJ,IJ)=1.D0
           ENDDO
           ENDIF
!
!               WRITE ( 6, * ) ' EPS_0_RATE = ', EPS_0_RATE
!               WRITE ( 6, * ) ' DPSIR  = ', DPSIR
!               WRITE ( 6, * ) ' DEPS_RATE  = ', DEPS_RATE
!               WRITE ( 6, * ) ' THETAGR = ', THETAGR
!               WRITE ( 6, * ) ' XPOL_RATE  = ', XPOLER
!               WRITE ( 6, * ) ' YPOL_RATE  = ', YPOLER
!
! -------- Now compute first time derivative of TRS_TO_CRS. It is done in
! -------- two steps: first comnpute 9 terms, each being a product of nine
! -------- matrices, 8 rotation matrices and the 9-th first derivative
!
           CALL MULTI_MUL_3 ( 9, DRTM_1, DRTM_P1,  RTM_P2,  RTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_2,  RTM_P1, DRTM_P2,  RTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_3,  RTM_P1,  RTM_P2, DRTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_4,  RTM_P1,  RTM_P2, RTM_P3, &
     &       DRTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_5,  RTM_P1,  RTM_P2,  RTM_P3, &
     &        RTM_N1, DRTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_6,  RTM_P1,  RTM_P2,  RTM_P3, &
     &        RTM_N1,  RTM_N2, DRTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_7,  RTM_P1,  RTM_P2,  RTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_8,  RTM_P1,  RTM_P2,  RTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU, DRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_9,  RTM_P1,  RTM_P2,  RTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB, DRTM_YWOB )
!
! -------- ... and at the second step we summ up the terms
!
           CALL MULTI_ADD_3 ( 9, TRS_TO_CRS_DER1, &
     &                           1.0D0, DRTM_1, &
     &                          -1.0D0, DRTM_2, &
     &                           1.0D0, DRTM_3, &
     &                          -1.0D0, DRTM_4, &
     &                           1.0D0, DRTM_5, &
     &                           1.0D0, DRTM_6, &
     &                          -1.0D0, DRTM_7, &
     &                           1.0D0, DRTM_8, &
     &                           1.0D0, DRTM_9  )
!
! -------- Compute derivative of TRS_TO_CRS matrix with respect to Eurler
! -------- angles.
!
        CALL MULTI_MUL_3 ( 9, DTRS_TO_CRS_DEOP(1,1,1), RTM_P1, RTM_P2, &
     &                        RTM_P3, RTM_N1, RTM_N2, RTM_N3, &
     &                        RTM_DIU, RTM_XWOB, PRTM_YWOB )
        CALL MULTI_MUL_3 ( 9, DTRS_TO_CRS_DEOP(1,1,2), RTM_P1, RTM_P2, &
     &                        RTM_P3, RTM_N1, RTM_N2, RTM_N3, &
     &                        RTM_DIU, PRTM_XWOB, RTM_YWOB )
        CALL MULTI_MUL_3 ( 9, DTRS_TO_CRS_DEOP(1,1,3), RTM_P1, RTM_P2, &
     &                        RTM_P3, RTM_N1, RTM_N2, RTM_N3, &
     &                        PRTM_DIU, RTM_XWOB, RTM_YWOB )
!
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(1,1,1)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(1,2,1)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(1,3,1)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(2,1,1)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(2,2,1)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(2,3,1)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(3,1,1)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(3,2,1)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(3,3,1)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(1,1,2)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(1,2,2)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(1,3,2)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(2,1,2)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(2,2,2)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(2,3,2)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(3,1,2)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(3,2,2)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(3,3,2)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(1,1,3)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(1,2,3)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(1,3,3)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(2,1,3)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(2,2,3)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(2,3,3)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(3,1,3)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(3,2,3)
!     write(6,*)' dbg deop ',DTRS_TO_CRS_DEOP(3,3,3)
!
! ----- Now compute time derivatives of TRS_TO_CRS wrt to DPSI and
! ----- rates.
!
! * DEP *  COMPUTE MATRICES REQUIRED FOR NUTATION PARTIALS
!
!   partials of time derivative of the precession matrix

      CALL MULTI_MUL_3 (  3,DPRDT1,DRTM_P1, RTM_P2, RTM_P3)
      CALL MULTI_MUL_3 (  3,DPRDT2, RTM_P1,DRTM_P2, RTM_P3)
      CALL MULTI_MUL_3 (  3,DPRDT3, RTM_P1, RTM_P2,DRTM_P3)

!   Complete the construction of the time derivative of the precession
!   matrix.

      CALL MULTI_ADD_3 (3,PRECS(1,1,2),        &
     &                    1.D0,DPRDT1,         &
     &                    1.D0,DPRDT2,         &
     &                    1.D0,DPRDT3)

!   partials of time  derivative of the nutation matrix
!
      CALL MULTI_MUL_3 (  3,DNUDT1,DRTM_N1, RTM_N2, RTM_N3)
      CALL MULTI_MUL_3 (  3,DNUDT2, RTM_N1,DRTM_N2, RTM_N3)
      CALL MULTI_MUL_3 (  3,DNUDT3, RTM_N1, RTM_N2,DRTM_N3)
!
!   Complete the construction of the time derivative of the nutation
!   matrix.

      CALL MULTI_ADD_3 (3,NUTAT(1,1,2),        &
     &                    1.D0,DNUDT1,         &
     &                    1.D0,DNUDT2,         &
     &                    1.D0,DNUDT3)

!   partials of time  derivative of the RTM_DIU ( DRTM_DIU )
!

      RSCpsi(1,1,2) = -COS(TETAG) * THETAGR * CEPSA  + &
     &                 SIN(TETAG) * SEPSA*EPS_0_RATE
      RSCpsi(1,2,2) =  SIN(TETAG) * THETAGR * CEPSA  + &
     &                 COS(TETAG) * SEPSA*EPS_0_RATE
      RSCpsi(1,3,2) =  0.D0
      RSCpsi(2,1,2) = -RSCpsi(1,2,2)
      RSCpsi(2,2,2) =  RSCpsi(1,1,2)
      RSCpsi(2,3,2) =  0.D0
      RSCpsi(3,1,2) =  0.D0
      RSCpsi(3,2,2) =  0.D0
      RSCpsi(3,3,2) =  0.D0

!  Compute the time derivative of the polar motion  rotation matrix.

      CALL MULTI_MUL_3 (  3,DPMDT1,DRTM_S, RTM_YWOB, RTM_XWOB)
      CALL MULTI_MUL_3 (  3,DPMDT2, RTM_S,DRTM_YWOB, RTM_XWOB)
      CALL MULTI_MUL_3 (  3,DPMDT3, RTM_S, RTM_YWOB,DRTM_XWOB)
!   Complete the construction of the time derivative of the polar motion
!   matrix.

      CALL MULTI_ADD_3 (3,RW2K(1,1,2), 1.D0, DPMDT1,     &
     &                                 1.D0, DPMDT2,     &
     &                                 1.D0, DPMDT3)
!
!
!     COMPLETE THE FIRST DERIVATIVES OF TRS_TO_CRS WRT DPSI AND EPST
!
!  second derivative of trs_to_crs wrt epst
!
      CALL MULTI_MUL_3 (4,R2001e,PRECS(1,1,2),RNCeps(1,1,1),RTM_DIU,   &
     &                  RW2K(1,1,1))
      CALL MULTI_MUL_3 (4,R2002e,PRECS(1,1,1),RNCeps(1,1,2),RTM_DIU,   &
     &                  RW2K(1,1,1))
      CALL MULTI_MUL_3 (4,R2003e,PRECS(1,1,1),RNCeps(1,1,1),DRTM_DIU,  &
     &                  RW2K(1,1,1))
      CALL MULTI_MUL_3 (4,R2004e,PRECS(1,1,1),RNCeps(1,1,1), RTM_DIU,  &
     &                  RW2K(1,1,2))
      CALL MULTI_ADD_3 (4,RC2Keps(1,1,2),     1.D0, R2001e,            &
     &                                        1.D0, R2002e,            &
     &                                        1.D0, R2003e,            &
     &                                        1.D0, R2004e)

!  second derivative of trs_to_crs wrt dpsi

      CALL MULTI_MUL_3 (4,R2p1a,PRECS(1,1,2),RNCpsi(1,1,1),RTM_DIU,     &
     &                  RW2K(1,1,1))
      CALL MULTI_MUL_3 (4,R2p1b,PRECS(1,1,2),NUTAT(1,1,1),RSCpsi(1,1,1),&
     &                  RW2K(1,1,1))
      CALL MULTI_ADD_3 (2,R2p1a, 1.D0, R2p1b, 1.D0, R2p1)
! ----
      CALL MULTI_MUL_3 (4,R2p2a,PRECS(1,1,1),RNCpsi(1,1,2),RTM_DIU,     &
     &                  RW2K(1,1,1))
      CALL MULTI_MUL_3 (4,R2p2b,PRECS(1,1,1),NUTAT(1,1,2),RSCpsi(1,1,1),&
     &                  RW2K(1,1,1))
      CALL MULTI_ADD_3 (2,R2p2a, 1.D0, R2p2b, 1.D0, R2p2)
! ----
      CALL MULTI_MUL_3 (4,R2p3a,PRECS(1,1,1),RNCpsi(1,1,1),DRTM_DIU,    &
     &                  RW2K(1,1,1))
      CALL MULTI_MUL_3 (4,R2p3b,PRECS(1,1,1),NUTAT(1,1,1),RSCpsi(1,1,2),&
     &                  RW2K(1,1,1))
      CALL MULTI_ADD_3 (2,R2p3a, 1.D0, R2p3b, 1.D0, R2p3)
! ----
      CALL MULTI_MUL_3 (4,R2p4a,PRECS(1,1,1),RNCpsi(1,1,1), RTM_DIU,    &
     &                  RW2K(1,1,2))
      CALL MULTI_MUL_3 (4,R2p4b,PRECS(1,1,1),NUTAT(1,1,1),RSCpsi(1,1,1),&
     &                  RW2K(1,1,2))
      CALL MULTI_ADD_3 (2,R2p4a, 1.D0, R2p4b, 1.D0, R2p4)


      CALL MULTI_ADD_3 (4,RC2Kpsi(1,1,2), 1.D0, R2p1, 1.D0, R2p2,       &
     &                                    1.D0, R2p3, 1.D0, R2p4)


! -------- Compute the second derivative of the rotation matrix. Only one
! -------- term is taken into account. Relative error of the expression below
! -------- is only 1.D-5
!
           CALL VTD_ROTMAT_DER2 ( 3, -TETAG, THETAGR, 0.0D0, SRTM_DIU  )
!
           CALL MULTI_MUL_3 ( 9, SRTM_1,  RTM_P1,  RTM_P2,  RTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3, SRTM_DIU,  RTM_XWOB,  RTM_YWOB )

           CALL MULTI_MUL_3 ( 9, SRTM_2, DRTM_P1, RTM_P2, RTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_3, RTM_P1, DRTM_P2, RTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_4, RTM_P1, RTM_P2, DRTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_5, RTM_P1, RTM_P2, RTM_P3, &
     &        RTM_N1, DRTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_6, RTM_P1, RTM_P2, RTM_P3, &
     &        RTM_N1,  RTM_N2, DRTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_7, RTM_P1, RTM_P2, RTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU, DRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_8, RTM_P1, RTM_P2, RTM_P3, &
     &        RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB, DRTM_YWOB )
!
           CALL MULTI_ADD_3 ( 8, TRS_TO_CRS_DER2, &
     &                           1.0D0, SRTM_1, &
     &                          -2.0D0, SRTM_2, &
     &                           2.0D0, SRTM_3, &
     &                          -2.0D0, SRTM_4, &
     &                          -2.0D0, SRTM_5, &
     &                          -2.0D0, SRTM_6, &
     &                          -2.0D0, SRTM_7, &
     &                          -2.0D0, SRTM_8  )
!
      RETURN
      END
