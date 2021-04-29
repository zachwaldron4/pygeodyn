!$MM2EJ2
      SUBROUTINE MM2EJ2(MJDSEC,FSEC,ARRAY,NMA,SCRTCH,LSTART)
!********1*********2*********3*********4*********5*********6*********7**
! MM2EJ2           93/05/14            0000.0    PGMR -  D. Pavlis
!
!  FUNCTION: ROTATES A 3X3 MATRIX FROM THE MARS MEAN EQUATOR AND EQUINOX
!            OF J2000 SYSTEM TO EARTH MEAN OF J2000
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    CURRENT DATE IN MJDS
!   FSEC     I    A    FRACTIONAL SECONDS
!   ARRAY   I/O   A    ARRAY TO BE ROTATED
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
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
      DIMENSION FSEC(NMA)
      DIMENSION CRTCH(1000)
      DIMENSION ARRAY(NMA,9)
      DIMENSION ARRAY0(3,3)
      DIMENSION ARRAY1(3,3)
      DIMENSION ARRAY2(3,3)
      DIMENSION ARRAY3(3,3)
      DIMENSION ARRAY4(3,3)
      DIMENSION ARRAY6(3,3)
      DIMENSION ARRAY7(3,3)
      DIMENSION SCRTCH(NMA,5)
!     DATA A/-217.65100887D0/,B/-37.114D0/,C/-47.681D0/
!     DATA A/-3.79872672506888D0/
      DATA RATE/-2.699619537D-06/
      DATA A/37.65100887D0/
      DATA B/-.647761498585175D0/
      DATA C/-0.832190440643416D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
      XM2000= 51544.5D0
      XJDSC0=DBLE(MJDSEC)
      X0=-(A+180.D0)
      DO 1000 I=1,NMA
      CRTCH(I)=0.D0
      IF(LSTART) THEN
      CRTCH(I)=(XJDSC0+FSEC(1))/86400.D0 - XM2000
!     CRTCH(I)=(XJDSC0+FSEC(1))/86400.D0 - XM2000
      ELSE
      CRTCH(I)=(XJDSC0+FSEC(I))/86400.D0 - XM2000
      ENDIF
!     CRTCH(I)=X0+CRTCH(I)*RATE
      CRTCH(I)=X0
!     CRTCH(I)=-((CRTCH(I)*DEGRAD)+TWOPI)
      CRTCH(I)=CRTCH(I)*DEGRAD
!1000  CONTINUE
! rotation from mars vernal equinox to IAU
      AB=CRTCH(I)
      CALL ROTAT(AB,ARRAY0,3)
! rotation from mars to earth
      CALL ROTAT(B,ARRAY1,1)
! rotation from IAU to earth vernal equinox
      CALL ROTAT(C,ARRAY2,3)
      CALL MULTI(ARRAY1,ARRAY0,ARRAY4,3,3,3)
! array3 is the rotation from  mars mean to earth eq J2000
      CALL MULTI(ARRAY2,ARRAY4,ARRAY3,3,3,3)
!     DO 10 I=1,NMA
      ARRAY6(1,1)=ARRAY(I,1)
      ARRAY6(1,2)=ARRAY(I,2)
      ARRAY6(1,3)=ARRAY(I,3)
      ARRAY6(2,1)=ARRAY(I,4)
      ARRAY6(2,2)=ARRAY(I,5)
      ARRAY6(2,3)=ARRAY(I,6)
      ARRAY6(3,1)=ARRAY(I,7)
      ARRAY6(3,2)=ARRAY(I,8)
      ARRAY6(3,3)=ARRAY(I,9)
      CALL MULTI(ARRAY3,ARRAY6,ARRAY7,3,3,3)
      ARRAY(I,1)=ARRAY7(1,1)
      ARRAY(I,2)=ARRAY7(1,2)
      ARRAY(I,3)=ARRAY7(1,3)
      ARRAY(I,4)=ARRAY7(2,1)
      ARRAY(I,5)=ARRAY7(2,2)
      ARRAY(I,6)=ARRAY7(2,3)
      ARRAY(I,7)=ARRAY7(3,1)
      ARRAY(I,8)=ARRAY7(3,2)
      ARRAY(I,9)=ARRAY7(3,3)
 1000 END DO
!10    CONTINUE
      RETURN
      END
