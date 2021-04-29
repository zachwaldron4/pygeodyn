!$EHAT_TO_ECF
      SUBROUTINE EHAT_TO_ECF(MJDSN,FSECN,NM,COSTHG,SINTHG,PMPXE,PMPXI, &
     &                       UTDT,AA)
!***********************************************************************
!
! FUNCTION
!
! INPUT PARAMETERS  - MJDSN=MODIFIED JULIAN DAY SECONDS OF THE FINAL
!                           RECEIVED TIME OF THE FIRST OBSERVATION IN
!                           THE BLOCK
!                     FSECN=ELAPSED SECONDS FROM MJDSN OF THE TIME
!                           TAG ASSOCIATED WITH STATION 1
!                     NM=NUMBER OF MEASUREMENTS
!
!  OUTPUT PARAMETERS

!                   - COSTHG=COSINES OF RIGHT ASCENSION OF GREENWICH
!                   - SINTHG= SINES  OF RIGHT ASCENSION OF GREENWICH
!                   - PMPXE = QUASAR UNIT VECTOR INERTIAL TOD
!                   - PMPXI = QASAR UNIT VECTTR ECF
!
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CBLOKV/QUANO(3),EHAT(3),DEDA(3),DEDD(3),EFLG
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
      COMMON/CSBSAT/KR,KRT,KZT,KXY2,KUZ,KUZSQ,KTEMP,KC3,KTHETG
!
      DIMENSION FSECN(NM),COSTHG(NM,4),SINTHG(NM,4),PMPXI(NM,3),  &
     & PMPXE(NM,3),UTDT(NM,4)
      DIMENSION AA(1)
      DIMENSION DUM(3),DUM1(3,2,1)
!
!
      DATA J1,J2,J3,J4/1,2,3,4/
      DATA TWO/2.0D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
      CALL GRHRAN(MJDSN,FSECN,.TRUE.,.FALSE.,AA(KEQN),AA(KSRTCH), &
     & AA(KTHETG),COSTHG(1,1),SINTHG(1,1),NM,AA,II,UTDT(1,J1),.FALSE.,&
     & DUM,DUM1)
!
!     WRITE(6,*) '  GCCORD : CALL TO TDORT3 FOR CF QUASAR*** '
      CALL TDORT3(MJDSN,FSECN,EHAT,PMPXE,AA,NM,.FALSE.,II)
!
!  ROTATE ROT2T BY THETA G TO BE USED FOR THE ANT. CORR.
      CALL ECFIXP(PMPXE,COSTHG(1,1),SINTHG(1,1),PMPXI,NM,NM,NM)
!
      RETURN
      END
