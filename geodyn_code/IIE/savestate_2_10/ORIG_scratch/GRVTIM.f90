!$GRVTIM
      SUBROUTINE GRVTIM(MJDSEC, FSEC, IPTC, IPTS, CSAVE, SSAVE, CDT,    &
              CPDA, CPDB, OMGPC, SDT, SPDA, SPDB, OMGPS, COSOMC,        &
              SINOMC, COSOMS, SINOMS, C, S)
!********1*********2*********3*********4*********5*********6*********7**
! GRVTIM           00/00/00            8805.0    PGMR - DDR/WX/WFE
!
! FUNCTION: MODIFIES GRAVITATIONAL C & S COEFFICIENTS FOR TIME DEPENDENT
!           GRAVITY AND FOR DYNAMIC POLAR MOTION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    CURRENT INTEGRATION STEP TIME IN MJDS SECONDS
!   FSEC     I    S    FRACTIONAL PART OF CURRENT STEP TIME
!   IPTC     I    A    POINTER ARRAY MAPPING THE TIME DEPENDENT C(N,M)
!                      COEFFICIENTS TO THE PROPER LOCATION IN THE
!                      C COEFFICIENT ARRAY
!   IPTS     I    A    POINTER ARRAY MAPPING THE TIME DEPENDENT S(N,M)
!                      COEFFICIENTS TO THE PROPER LOCATION IN THE
!                      S COEFFICIENT ARRAY
!   CSAVE    I    A    C COEFFICIENTS SAVED AT THE START OF THE GLOBAL
!                      ITERATION
!   SSAVE    I    A    S COEFFICIENTS SAVED AT THE START OF THE GLOBAL
!                      ITERATION
!   CDT      I    A    DOT TERM FOR C COEFFICIENTS
!   CPDA     I    A    A  PERIODIC TERM FOR C COEFS
!   CPDB     I    A    B  PERIODIC TERM FOR C COEFS
!   OMGPC    I    A    OMEGA PERIODIC TERM FOR C COEFS
!   SDT      I    A    DOT TERM FOR S COEFFICIENT
!   SPDA     I    A    A  PERIODIC TERM FOR S COEF
!   SPDB     I    A    B  PERIODIC TERM FOR S COEF
!   OMGPS    I    A    OMEGA PERIODIC TERM FOR S COEF
!   COSOMC   O    A    COSINE OF OMEGA FOR C COEF
!   SINOMC   O    A    SINE OF OMEGA FOR C COEF
!   COSOMS   O    A    COSINE OF OMEGA FOR S COEF
!   SINOMS   O    A    SINE OF OMEGA FOR S COEF
!   C       I/O   A    CURRENT VALUE FOR THE C COEFFICIENTS
!   S       I/O   A    CURRENT VALUE FOR THE S COEFFICIENTS
!
! COMMENTS:
!
!*********1*********2*********3*********4*********5*********6*********7
      USE LOAD_COEFS_MOD, ONLY : OLOAD_DEF_COEFS, DESAI_COEFS

      IMPLICIT DOUBLE PRECISION(A-H,O-Z), LOGICAL(L)
      SAVE

      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CLGVTM/LGVTM,LDPM,LGVTPM,LOPT,LCSDPM,L_MP_IERS2003,        &
     &              L_CS_IERS2003,L_MP_IERS2010,L_CS_IERS2010
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CGVTMI/NCDT,NSDT,NCPD,NSPD,NCDTA,NSDTA,NCPDA,NSPDA,NGRVTM, &
     &       KCDT,KCPDA,KCPDB,KSDT,KSPDA,KSPDB,KOMGC,KOMGS,KCOMGC,      &
     &       KSOMGC,KCOMGS,KSOMGS,NXGVTI
      COMMON/CGVTMR/EPGVTM,ELAPGT,XCGVTR
      COMMON/DYNPOL/DPMEP ,DPMXP ,DPMYP ,DPMXDT,DPMYDT,DPMOPT,DPMXPC,   &
     &              DPMYPC,DPMKF ,DPMC21,DPMS21,DPMPD ,DPMUDK,DPMVDK,   &
     &              DPMOCP,DPMNLD,DPMERS,XDYNPL
      COMMON/CEARTH/AEG,AEGSQ,FGINV,FG,EGSQ,EGSQP1,C1,C2,FOURC1,TWOC2,  &
     &              XH2,XL2,WREF,RMEAN,REFC20,REFC40,REFC60,BEG,CBLTC,  &
     &              WAE2,GAMMAA,FSTAR,F4
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT

      DOUBLE PRECISION, PARAMETER :: sday = 86400.D0
      DOUBLE PRECISION, PARAMETER :: dayyr  = 365.25D0
      DOUBLE PRECISION, PARAMETER :: secyr = dayyr * sday
      DOUBLE PRECISION, PARAMETER :: secyri = 1.0D0 / secyr
      DOUBLE PRECISION, PARAMETER :: fourpi = 0.1256637061435917D+02
      DOUBLE PRECISION, PARAMETER :: GCONST = 6.673D-11
      DOUBLE PRECISION, PARAMETER :: RHOW = 1.025D3
      DOUBLE PRECISION, PARAMETER :: G2R = 0.6870D0
      DOUBLE PRECISION, PARAMETER :: G2I = 0.0036D0
      DOUBLE PRECISION, PARAMETER :: SQRT3 = SQRT(3.0D0)

      INTEGER :: MJDSEC
      INTEGER :: IPTC(*), IPTS(*)
      DOUBLE PRECISION :: FSEC
      DOUBLE PRECISION :: CSAVE(*), SSAVE(*)
      DOUBLE PRECISION :: CDT(*), CPDA(*), CPDB(*), OMGPC(*)
      DOUBLE PRECISION :: SDT(*), SPDA(*), SPDB(*), OMGPS(*)
      DOUBLE PRECISION :: COSOMC(*), SINOMC(*), COSOMS(*), SINOMS(*)
      DOUBLE PRECISION :: C(*), S(*)

      INTEGER :: I, IPOS, IPTT, JPOS, MDES, NDES, NPOS
      DOUBLE PRECISION, ALLOCATABLE :: OLDCOEF(:) ! SAVE
      DOUBLE PRECISION, ALLOCATABLE :: DESC(:) ! SAVE
      DOUBLE PRECISION :: ARGC, ARGS, DSR, STIME, TMPC1, TMPC2
      DOUBLE PRECISION :: Djxp, Djyp

      LOGICAL :: LINIT = .TRUE. ! SAVE

!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************

      ! Current integration step time
      STIME = DBLE(MJDSEC) + FSEC

      IF (ICBDGM == ITBDGM) THEN
          ! On first time, read the DESAI's self-consistent equilibrium
          ! coefficients for ocean pole tide
          IF (LINIT .AND. LOPT) THEN
              ALLOCATE(OLDCOEF(SIZE(OLOAD_DEF_COEFS)-1))
              ALLOCATE(DESC(SIZE(DESAI_COEFS)))
              DESC(:) = DESAI_COEFS(:)

              ! Set the flag to be false
              LINIT = .FALSE.

              ! Calculate the mutiplication coefficient R
              ! ***************************************************
              !    R = Omega^2 a_E^4 4 PI G rho_water / GM / g_E
              ! ***************************************************
              DSR = WREF * WREF * AEGSQ * AEGSQ * FOURPI * GCONST * RHOW
              DSR = DSR / BDSTAT(7,9) / GAMMAA
              DO NDES = 1, 360
                  OLDCOEF(NDES) = DSR * (1.D0-OLOAD_DEF_COEFS(NDES+1))  &
     &                    / (2.D0*NDES+1.D0)
                  NPOS = NDES * (NDES+1) / 2
                  DO MDES = 0, NDES
                      !*************************************************
                      ! The Desai coefficients A_Real(n,m), B_Real(n,m),
                      ! A_Imag(n,m), and B_Imag(n,m) are stored in a
                      ! one-dimensional array DESC.
                      ! For example:
                      !   A_real(n,m) is referenced by
                      !               DESC(4*(n*(n+1)/2+m-1)+1).
                      !   B_real(n,m) is referenced by
                      !               DESC(4*(n*(n+1)/2+m-1)+2).
                      !   A_Imag(n,m) is referenced by
                      !               DESC(4*(n*(n+1)/2+m-1)+3).
                      !   B_Imag(n,m) is referenced by
                      !               DESC(4*(n*(n+1)/2+m-1)+4).
                      ! See IERS Conventions 2003 for the definition of
                      ! these coefficients.
                      !*************************************************
                      IPOS = NPOS + MDES - 1
                      DESC(4*IPOS+1) = DESC(4*IPOS+1) * OLDCOEF(NDES)
                      DESC(4*IPOS+2) = DESC(4*IPOS+2) * OLDCOEF(NDES)
                      DESC(4*IPOS+3) = DESC(4*IPOS+3) * OLDCOEF(NDES)
                      DESC(4*IPOS+4) = DESC(4*IPOS+4) * OLDCOEF(NDES)
                  END DO
              END DO
          END IF

          ! Reset current C and S values back to the initial values
          IF (LDPM) THEN
              ! Reset the C(2,1) and S(2,1) values for dynamic polar
              ! motion
              C(6) = DPMC21
              S(6) = DPMS21
          END IF
          IF (LGVTM) THEN
              ! Reset values used for time dependent gravity option
              IF (NCDT+NCPD > 0) THEN
                  DO I = 1, NCDT+NCPD
                     IPTT = IPTC(I)
                     C(IPTT) = CSAVE(I)
                  END DO
              END IF
              IF (NSDT+NSPD > 0) THEN
                  DO I = 1, NSDT+NSPD
                     IPTT = IPTS(I)
                     S(IPTT) = SSAVE(I)
                  END DO
              END IF
          END IF

          IF (LDPM) THEN
              ! COMPUTE THE MEAN POLE
              CALL IERSMP(MJDSEC, FSEC, Djxp, Djyp)
              ! DPMUDK CORRESPONDS TO M_1, DPMVDK CORRESPONDS TO -M_2
              ! (NOTE THE NEGATIVE)
              DPMUDK = DPMXPC - Djxp
              DPMVDK = DPMYPC - Djyp

              ! ADD DYNAMIC POLAR MOTION CONTRIBUTION TO C(2,1)
              ! AND S(2,1)
              CALL IERSCS21(Djxp, Djyp, C(5), C(6), C(7), S(6), S(7),   &
     &                LCSDPM)

              IF (LOPT) THEN
                  ! Add the ocean pole tide contributions to C(n,m) and
                  ! S(n,m)
                  ! The array index of C(k) for C(n,m):
                  !         k = ((n+6)*(n-1))/2+m+1
                  ! The array index of S(k) for S(n,m):
                  !         k = ((n+6)*(n-1))/2+m+1
                  TMPC1 = DPMUDK*G2R - DPMVDK*G2I
                  TMPC2 = -DPMVDK*G2R - DPMUDK*G2I
                  DO NDES = 1, INT(DPMNLD)
                      NPOS = NDES * (NDES+1) / 2
                      DO MDES = 0, NDES
                          IPOS = ((NDES+6)*(NDES-1))/2 + MDES + 1
                          JPOS = NPOS + MDES - 1
                          C(IPOS) = C(IPOS) + (DESC(4*JPOS+1)*TMPC1     &
     &                            + DESC(4*JPOS+3)*TMPC2)
                          S(IPOS) = S(IPOS) + (DESC(4*JPOS+2)*TMPC1     &
     &                            + DESC(4*JPOS+4)*TMPC2)
                      END DO
                  END DO
              END IF
          END IF
      END IF ! ICBDGM == ITBDGM

      IF (LGVTM) THEN
          ! ADD TIME DEPENDENT GRAVITY CONTRIBUTION TO C AND S
          ! COEFFICIENTS

          ! COMPUTE ELAPSED TIME FROM EPOCH OF TIME DEPENDENT GRAVITY
          ! COEFFICIENTS TO THE CURRENT STEP TIME OF THE INTEGRATOR
          ELAPGT = STIME - EPGVTM
          ! ... CONVERT ELAPSED TIME TO YEARS
          ELAPGT = ELAPGT * secyri

          ! MODIFY LINEAR C COEFFICIENTS
          IF (NCDT > 0) THEN
              DO I = 1, NCDT
                  IPTT = IPTC(I)
                  C(IPTT) = C(IPTT) + ELAPGT*CDT(I)
              END DO
          END IF

          ! MODIFY PERIODIC C COEFFICIENTS
          IF (NCPD > 0) THEN
              DO I = 1, NCPD
                  IPTT = IPTC(I+NCDT)
                  ARGC = OMGPC(I) * ELAPGT
                  COSOMC(I) = COS(ARGC)
                  SINOMC(I) = SIN(ARGC)
                  C(IPTT) = C(IPTT) + CPDA(I)*COSOMC(I)                 &
     &                    + CPDB(I)*SINOMC(I)
              END DO
          END IF

          ! MODIFY LINEAR S COEFFICIENTS
          IF (NSDT > 0) THEN
              DO I = 1, NSDT
                  IPTT = IPTS(I)
                  S(IPTT) = S(IPTT) + ELAPGT*SDT(I)
              END DO
          END IF

          ! MODIFY PERIODIC S COEFFICIENTS
          IF (NSPD > 0) THEN
              DO I = 1, NSPD
                  IPTT = IPTS(I+NSDT)
                  ARGS = OMGPS(I) * ELAPGT
                  COSOMS(I) = COS(ARGS)
                  SINOMS(I) = SIN(ARGS)
                  S(IPTT) = S(IPTT) + SPDA(I)*COSOMS(I)                 &
     &                    + SPDB(I)*SINOMS(I)
              END DO
          END IF
      END IF

      END SUBROUTINE
