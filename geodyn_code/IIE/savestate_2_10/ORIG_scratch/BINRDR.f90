!$BINRDR
      SUBROUTINE BINRDR(AA,II,MJDSEC,FSEC,EDSIG,RESID,OTD,THTG,         &
     &                  ELEV,NELEVS,LEDIT,SIGOUT,NM,UTDT)
!********1*********2*********3*********4*********5*********6*********7**
! BINRDR           85/10/18            8510.0    PGMR - TOM MARTIN
!                  85/04/02            8504.0    PGMR - D. ROWLANDS
!
! FUNCTION:  WRITE OUT OBSERVATION RESIDUAL RECORDS FOR BINRES
!            FILE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   MJDSEC   I    S    INTEGER UTC SECS TO WHICH OBS ARE REFERENCED
!   FSEC     I    A    FRACTIONAL UTC SECS FROM MJDSEC FOR EACH OBS
!   EDSIG    I    A    EDITING SIGMA
!   RESID    I    A    OBSERVATION RESIDUALS
!   OTD      I    A    OBSERVATION TIME DERVIATIVES
!   THTG     O    A    GREENWHICH HR ANGLE AT NOMINAL MEAS TIMES
!   ELEV     I    A    ELEVATION ANGLES COMPUTED FOR ALL LINKS
!   NELEVS   I    A    TOTAL NUMBER OF COMPUTED ELEVATION ANGLES
!   LEDIT    I    A    LOGICAL FLAGS INDICATING WHICH OBSERVATIONS
!                      HAVE BEEN EDITED.
!   SIGOUT  I/O   A    WORKING ARRAY
!   NM       I    S    NUMBER OF MEASUREMENTS
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CORA03/KRFMT ,KROTMT,KSRTCH,KCFSC ,KTHG  ,KCOSTG,KSINTG,   &
     &              KDPSI ,KEPST ,KEPSM ,KETA  ,KZTA  ,KTHTA ,KEQN  ,   &
     &              KDPSR ,KSL   ,KH2   ,KL2   ,KXPUT ,KYPUT ,KUT   ,   &
     &              KSPCRD,KSPSIG,KSTAIN,KSXYZ ,KSPWT ,KDXSDP,KDPSDP,   &
     &              KXLOCV,KSTNAM,KA1UT ,KPLTIN,KPLTVL,KPLTDV,KPUTBH,   &
     &              KABCOF,KSPEED,KANGFC,KSCROL,KSTVEL,KSTVDV,KTIMVL,   &
     &              KSTEL2,KSTEH2,KSL2DV,KSH2DV,                        &
     &              KAPRES, KASCAL, KASOFF,KXDOTP,KSAVST,               &
     &              KANGT , KSPDT , KCONAM,KGRDAN,KUANG ,KFAMP ,        &
     &              KOFACT, KOTSGN,KWRKRS,KANFSD,KSPDSD,KPRMFS,KSCRFR,  &
     &              KCOSAS,KSINAS,KDXTID,KALCOF,KSTANF,KNUTIN,KNUTMD,   &
     &              KPDPSI,KPEPST,KDXDNU,KDPSIE,KEPSTE,NXCA03
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION AA(1),II(1),FSEC(NM),EDSIG(NM),RESID(NM),OTD(NM),       &
     &          ELEV(NM,12),THTG(NM),LEDIT(NM),SIGOUT(NM),UTDT(4,NM)
      DIMENSION DUM(3),DUM1(3,2,1)
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
! GET ET TIMES SO CAN EVALUATE GRHRAN
!
      CALL UTCET(.TRUE.,NM,MJDSEC,FSEC,AA(KCFSC),AA(KA1UT))
!LST  CALL GRHRAN(MJDSEC,AA(KCFSC),.TRUE.,.FALSE.,AA(KEQN),AA(KSRTCH),
      CALL GRHRAN(MJDSEC,AA(KCFSC),.TRUE.,.TRUE. ,AA(KEQN),AA(KSRTCH),  &
     &            THTG,AA(KCOSTG),AA(KSINTG),NM,AA,II,UTDT(1,1),.FALSE.,&
     &            DUM,DUM1)
!
      DO 1000 N=1,NM
      SIGMA    =EDSIG(N)
      IF(LEDIT(N)) SIGMA=-SIGMA
      SIGOUT(N)=SIGMA
 1000 END DO
!
!     ....NELEVS CAN VARY BETWEEN 1 AND 12, OLD CODE FIXED AT 3
!
      N3 = MAX( NELEVS, 3 )
!     N3 = NELEVS
      WRITE(IUNT19) FSEC,RESID,SIGOUT,OTD,THTG,                         &
     &              ((ELEV(I,J),I=1,NM),J=1,N3)
      RETURN
      END
