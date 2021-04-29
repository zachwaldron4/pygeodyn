!$GETXAC
      SUBROUTINE GETXAC(AA,MJDSBL,FSEC,EXACTM,EXACIN,ISATAC,EXACOB,     &
     &LDYNAP)
!********1*********2*********3*********4*********5*********6*********7**
! GETXAC           00/00/00            0000.0    PGMR - D. E. Pavlis
!
! FUNCTION:  GET EXTERNAL ACCELERATION DATA
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/ACCELO/IDYNDL(3),IDYNSN(3),IGEOSN(3),NXACCO
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
      COMMON/SUBACC/DDTEXT(3),XYZDOT(3)

      DIMENSION AA(1)
      DIMENSION EXACTM(2,1),EXACIN(1)
      DIMENSION ISATAC(1)
      DIMENSION EXACOB(1)

      DATA kentry/0/

!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      kentry = kentry + 1

! ***  PRINT INCOMING INFORMATION  ************************************

      IF(KENTRY.EQ.1)  THEN


!   EXTERNAL ACCELERATION FILE
!     write(6,*)'  EXTERNAL ACCELERATION TIMES '
!     write(6,*)EXACTM(1,1),EXACTM(2,1)
!     write(6,*)'  EXTERNAL ACCELERATION INTERVAL '
!     write(6,*)EXACIN(1)
!     write(6,*)'  EXTERNAL ACCELERATION SATELLITES '
!     write(6,*)ISATAC(1)
!   FIGURE OUT LENGTH OF OBS FILE FOR THIS SATELLITE
      NOBS=((EXACTM(2,1)-EXACTM(1,1))/EXACIN(1))+1
      J=1
! PRINT OBSERVATIONS FROM EXTERNAL ACCELERATION FILE
!     DO I=1,NOBS
!     WRITE(6,*)'OBS ',EXACOB(J),EXACOB(J+1),EXACOB(J+2),J
!     J=J+3
!     ENDDO

             ! TO IF(KENTRY.EQ.1)
      ENDIF

! ***  END PRINT INCOMING INFORMATION **********************************
!  INITIALIZE DDTEXT AND XYZDOT
!  DDTEXT  AND XYZDOT BELONG IN THE CB SUBACC , DDTEXT HOLDS THE
!  ACCELERATIONS FOR THIS STEP AND XYZDOT HOLDS THE APPROXIMATION
!  OF THE FIRST DERIVATIVE OF THE ACCELERATION WRT TIME, NEDDED FOR THE
!  TIMING PARTIALS.

      DO I=1,3
      DDTEXT(I)=0.D0
      XYZDOT(I)=0.D0
      ENDDO

      T=DBLE(MJDSBL)+FSEC
      DIFF=T-EXACTM(1,1)
      IF(DIFF.LT.0.D0) THEN
      WRITE(6,*)' DBG TIME OF OBSERVATION OUTSIDE OF EXTACC TIMES'
      WRITE(6,*)T,' LESS THAN ',EXACTM(1,1)
      LDYNAP=.FALSE.
      GOTO 1000
      ENDIF
      NGAP=(0.001D0+DIFF/EXACIN(1))
      IF(NGAP.LT.0) THEN
      WRITE(6,*)' DBG TIME OF OBSERVATION OUTSIDE OF EXTACC TIMES'
      WRITE(6,*)' NGAP ',NGAP
      LDYNAP=.FALSE.
      GOTO 1000
      ENDIF
      K=1+NGAP*3
!     WRITE(6,*)'GOT OBS ',EXACOB(K),EXACOB(K+1),EXACOB(K+2),K,T
      IF(EXACOB(K).EQ.-9999999.D0.OR.EXACOB(K+1).EQ.-9999999.D0.OR&
     &.EXACOB(K+2).EQ.-9999999.D0)  THEN
      LDYNAP=.FALSE.
      GOTO 1000
      ENDIF
      DDTEXT(1)=DBLE(IDYNSN(1))*EXACOB(K)
      DDTEXT(2)=DBLE(IDYNSN(2))*EXACOB(K+1)
      DDTEXT(3)=DBLE(IDYNSN(3))*EXACOB(K+2)
      DT=2.D0*EXACIN(1)
      XYZDOT(1)=DBLE(IDYNSN(1))*(EXACOB(K+3)-EXACOB(K-3))/DT
      XYZDOT(2)=DBLE(IDYNSN(2))*(EXACOB(K+4)-EXACOB(K-2))/DT
      XYZDOT(3)=DBLE(IDYNSN(3))*(EXACOB(K+5)-EXACOB(K-1))/DT
!     write(6,*)' dbg XYZDOT ',XYZDOT,EXACIN(1)
 1000 CONTINUE

      RETURN
      END
