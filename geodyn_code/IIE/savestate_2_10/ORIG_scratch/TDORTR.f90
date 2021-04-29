!$TDORTR
      SUBROUTINE TDORTR(MJDSEC,FSEC,X1,X2,AA,NM,NDIM,LMATC,LTDTR,II)
!********1*********2*********3*********4*********5*********6*********7**
! TDORTR           87/09/30            8710.0    PGMR - D. ROWLANDS
!
! FUNCTION:  TRANFORM TRUE OF DATE EARTH EQUATOR & EQUINOX
!            (TDEEES) SYSTEM TO TRUE OF REF SATELLTE SYSTEM
!            (TRSS). THE TRSS MAY BE THE IAU SYSTEM IF THE
!            CENTER OF INTEGRATION IS NOT THE EARTH). THE
!            ROUTINE CAN GO IN THE OTHER DIRECTION ALSO.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I         INTEGER NUMBER OF SECONDS SINCE GEODYN REF.TIME
!                      FOR THE BLOCK TO BE TRANSFORMED.
!   FSEC     I         ELAPSED SECONDS SINCE MJDSEC FOR EACH DATA
!                      POINT
!   X1       I         TDEEES COORDINATES OR TRSS COORDINATES
!   X2       O         TRSS COORDINATES OR TDEEES COORDINATES
!   AA      I/O   A
!   II      I/O   A
!   NM       I         NUMBER OF LIGHT TIMES TO BE COMPUTED
!   NDIM     I         FIRST DIMENSION OF X1 & X2
!   LMATC    I         .TRUE. IF MATRIX CALCULATION IS NECESSARY.
!   LTDTR    I         .TRUE. IF GOING FROM TDEEES TO TRSS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
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
      COMMON/CRDDIM/NDCRD2,NDCRD3,NDCRD4,NDCRD5,NDCRD6,NDCRD7,          &
     &              NDCRD8,NDCRD9,NXCRDM
      DIMENSION AA(1),II(1),X1(1),X2(1),FSEC(NM)
      DIMENSION DUM(3),DUM2(3,2,1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      IF(.NOT.LMATC) GO TO 100
! CALCULATE THE MATRIX THAT WILL BE NECESSARY
      CALL BUFEAR(MJDSEC,FSEC(1),MJDSEC,FSEC(NM),1,MJDR,FSECR,LRECAL,   &
     &            AA,II)
      CALL PRECSS(MJDSEC,FSEC,AA(KETA),AA(KZTA),AA(KTHTA),              &
     &            AA(KSRTCH),NM)
      CALL NUVECT(MJDSEC,FSEC,AA(KDPSI),AA(KEPST),AA(KEPSM),            &
     &            NM,AA(KSRTCH))
      CALL NPVECT(MJDSEC,FSEC,AA(KDPSI),AA(KEPST),AA(KEPSM),            &
     &            AA(KETA),AA(KTHTA),AA(KZTA),NM,AA(KSRTCH),            &
     &            AA(KROTMT),AA(KEQN),NDCRD2,.FALSE.,                   &
     &            AA(KPDPSI),AA(KPEPST),AA(KNUTIN),AA(KNUTMD),.FALSE.,  &
     &            AA(KDXDNU),AA(KDPSIE),AA(KEPSTE),DUM,DUM2)
  100 CONTINUE
      CALL TDTR(AA(KROTMT),AA(KROTMT+NDCRD2),AA(KROTMT+NDCRD3),         &
     &    AA(KROTMT+NDCRD4),AA(KROTMT+NDCRD5),AA(KROTMT+NDCRD6),        &
     &    AA(KROTMT+NDCRD7),AA(KROTMT+NDCRD8),AA(KROTMT+NDCRD9),        &
     &   X1,X2,AA(KSRTCH),NM,NDIM,LTDTR)
      RETURN
      END
