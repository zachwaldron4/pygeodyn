!$GETDYN
      SUBROUTINE GETDYN(AA,MJDSBL,FSEC,DYNTIM,NPERDY,LDYNAP)
!********1*********2*********3*********4*********5*********6*********7**
! GETDYN           00/00/00            0000.0    PGMR - D. E. Pavlis
!
! FUNCTION:  GET LDYNAP FLAG (IS THIS TIME WITHIN DYNSEL?)
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    DYNAMIC ARRAY FOR REAL VARIABLES.
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
!
      DIMENSION DYNTIM(1,2),AA(1)
      DIMENSION NPERDY(1)

      DATA kentry/0/

!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      kentry = kentry + 1

!   DYNAMIC ACCELERATION  PERIODS
      IJ=NPERDY(1)
! ***  PRINT INCOMING INFORMATION  ************************************
      IF(KENTRY.EQ.1) THEN
!     write(6,*) DYNTIM(1,1),T,DYNTIM(1,2)
      ENDIF

      T=DBLE(MJDSBL)+FSEC

!   DYNAMIC ACCELERATION TIMES
!     write(6,*)'  DYNAMIC ACC PERIODS= ',IJ
!     write(6,*)'  DYNAMIC ACC TIMES '
      DO I=1,IJ
!     write(6,*) DYNTIM(I,1),T,DYNTIM(I,2),I
      IF(T.LE.DYNTIM(I,2).AND.T.GE.DYNTIM(I,1)) LDYNAP=.TRUE.
      ENDDO

      RETURN
      END
