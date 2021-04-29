      SUBROUTINE GVMAT(VMATRX,P,AORN,SINLAM,COSLAM,TANPSI,CORPAR,       &
     &                 VRARAY,VMATB,EXPRFL,CS,SS,AA)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  PURPOSE:  COMPUTE THE COMPONENT OF THE PARTIALS OF ACCELERATION WRT
!            SPACECRAFT POSITION (THE VMAT)  THAT ARISES FROM
!            GEOPOTENTIAL (POINT MASS AND SPHERICAL HARMONIC TERMS
!            INCLUDED).
!            (1) OUTPUT THIS 3x3 MATRIX IN PLANET BODY FIXED
!                COORDINATES,
!            (2) ALSO COMPUTE THIS 3x3 MATRIX IN J2000 COORDINATES AND
!                THIS GEOPOTENTIAL CONTRIBUTION INTO THE TOTAL MATRIX
!                (THE PARTIALS OF ACCELERATION WRT SPACECRAFT POSITION
!                FROM ALL SOURCES OF ACCELERATION)
!
!  ARGUMENTS: VMATRX - (I & O) 3x6 MATRIX OF PARTIALS OF ACCELERATION
!                      WRT CURRENT SPACECRAFT STATE (ALL IN J2000)
!             P      - (I) LGENDR POLYNOMIALS COMPUTED BY SUBOUTINE
!                      EGRAV (POLYNOMIALS HAVE BEEN COMPUTED FOR
!                      CURRENT SPACECRAFT LATITUDE)
!             AORN   - (I) ARRAY OF (GM/R)*(AE/R)**N
!             SINLAM - (I) ARRAY OF SIN(LAMDA*M)
!             COSLAM - (I) ARRAY OF COS(LAMDA*M)
!             TANPSI - (I) ARRAY OF M*TAN(PSI)
!             CORPAR - (I  TRANSFORMATION MATRIX FOR BODY FIXED R,PSI,
!                          LAMDA TO BODY FIXED XYZ (CORPAR(7)=SINPSI)
!             VRARAY - (I) PLANET FIXED INFORMATION ABOUT SATELLITE
!                          (1-3)ACCELERATION DUE TO GEOPOTENTIAL
!                          IN R,PPSI,LAMDA, (4) R, (5) RSQ,
!                          (6) Xpf**2 + Ypf**2 ,
!                          (7) DSQRT(Xpf**2 + Ypf**2) ,
!                          (8) GM/R , (9-11) Xpf,Ypf, Zpf
!             VMATB  - (O) GEOPOTEBTIAL CONTRIBUTION TO 3x3 MATRIX OF
!                          PARTIALS OF ACCELERATION WRT CURRENT
!                          SPACECRAFT STATE; PLANET FIXED
!             EXPRFL - (I) ARRAY OF EXPLICIT PARTIALS OF ACCELERATION
!                          IN RFL WRT C&S
!             CS     - (I) STOKES GRAVITATIONAL C COEFFICENTS OF
!                          PERTURBING BODY
!             SS     - (I) STOKES GRAVITATIONAL S COEFFICENTS OF
!                          PERTURBING BODY
!             AA     - (I) GEODYN ARRAY THAT HOLDS ALL DYNAMICALLY
!                          ALLOCARED FLOATING POINT ARRAYS
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      COMMON/CORA01/KFSEC0,KFSECB,KFSEC ,KFSECV,KH    ,KHV   ,KCTOL ,   &
     &              KRSQ  ,KVMATX,KCPP  ,KCPV  ,KCCP  ,KCCV  ,KCCPV ,   &
     &              KCCVV ,KXPPPP,KX    ,KPX   ,KSUMX ,KXDDOT,KSUMPX,   &
     &              KPXDDT,KAB   ,KPN   ,KAORN ,KSINLM,KCOSLM,KTANPS,   &
     &              KCRPAR,KVRARY,KXM   ,KXNP1 ,KXPRFL,KXM2  ,KXNNP1,   &
     &              KWRK  ,KFI   ,KGE   ,KB0DRG,KBDRAG,KAPGM ,KAPLM ,   &
     &              KCN   ,KSN   ,KSTID ,KTIDE ,KSTDRG,KSTSRD,KSTACC,   &
     &              KLGRAV,KGM   ,KAE   ,KFPL  ,KFEQ  ,KPLNPO,KPLNVL,   &
     &              KXEPOC,KCD   ,KCDDOT,KCR   ,KGENAC,KACN  ,KASN  ,   &
     &              KTHDRG,KCKEP ,KCKEPN,KXNRMZ,KXNRMC,KFSCEP,KFSCND,   &
     &              KAREA ,KXMASS,KRMSPO,KTCOEF,KTXQQ ,KTIEXP,KTXMM ,   &
     &              KTXLL1,KTXSN1,KTS2QQ,KT2M2H,KT2MHJ,KTXKK ,KTSCRH,   &
     &              KPXPK ,KAESHD,KCSAVE,KSSAVE,KCGRVT,KSGRVT,KXDTMC,   &
     &              KDNLT ,KTXSN2,KTNORM,KTWRK1,KTWRK2,KUNORM,KAERLG,   &
     &              KSINCO,KPARLG,KCONST,KBFNRM,KTDNRM,KCSTHT,KTPSTR,   &
     &              KTPSTP,KTPFYW,KPLMGM,KTPXAT,KEAQAT,KEAFSS,KEAINS,   &
     &              KACS  ,KECS  ,KSOLNA,KSOLNE,KSVECT,KSFLUX,KFACTX,   &
     &              KFACTY,KADIST,KGEOAN,KPALB ,KALBCO,KEMMCO,KCNAUX,   &
     &              KSNAUX,KPPER ,KACOSW,KBSINW,KACOFW,KBCOFW,KANGWT,   &
     &              KWT   ,KPLNDX,KPLANC,KTGACC,KTGDRG,KTGSLR,KWTACC,   &
     &              KWTDRG,KWTSLR,KTMACC,KTMDRG,KTMSLR,KATTUD,KDYACT,   &
     &              KACCBT,KACPER,KXDDNC,KXDDAO,KXNC  ,KXPPNC,KSMXNC,   &
     &              KXDDTH,KPDDTH,KXSSBS,KCPPNC,KEXACT,KXACIN,KXACOB,   &
     &              KPXHDT,KTPXTH,KPACCL,KTXSTA,KDELXS,KSMRNC,KPRX  ,   &
     &              KSMRNP,KDSROT,KXUGRD,KYUGRD,KZUGRD,KSUMRC,KXDDRC,   &
     &              KTMOS0,KTMOS, KTMOSP,KSMXOS,KSGTM1,KSGTM2,KSMPNS,   &
     &              KXGGRD,KYGGRD,KZGGRD,KXEGRD,KYEGRD,KZEGRD,KSSDST,   &
     &              KSDINS,KSDIND,KSSDSR,KSSDDG,KTATHM,KTAINS,KTAFSS,   &
     &              KSRAT ,KTRAT ,KHLDV ,KHLDA1,KHLDA4,KHLDA7,KQAST1,   &
     &              KQAST2,KQAST3,KQAST4,KQAST5,KQAST6,NXCA01
      COMMON/CRMB/RMB(9), rmb0(9)
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      COMMON/RFLMAT/RFLADR(3,3)
!
! ARGUMENTS
!
      DIMENSION VMATRX(3,6),P(NP),AORN(NMAX),SINLAM(NMAXP1),            &
     &   COSLAM(NMAXP1),TANPSI(NMAXP1),CORPAR(3,3),VRARAY(14),          &
     &   VMATB(3,3),EXPRFL(NP,6),CS(NP),SS(NP),AA(1)
!
! SCRATCH ARRAYS
!
      DIMENSION X(3),TEMP(3,3),VMAT(3,3)
!
      DATA ZERO/0.D0/,TWO/2.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      X(1)=VRARAY(9)
      X(2)=VRARAY(10)
      X(3)=VRARAY(11)
!   GET R,PSI,LAMDA VMAT FROM GEOPOTENTIAL
      CALL RFLMT(EXPRFL,AA(KXNP1),AA(KXM),AA(KXM2),AA(KXNNP1),          &
     &            COSLAM,SINLAM,TANPSI,CS,SS,                           &
     &            P,VRARAY,AORN)
! FILL IN REST OF 2ND ORDER PARTIALS BY SYMMETRY
      RFLADR(2,1)=RFLADR(1,2)
      RFLADR(3,1)=RFLADR(1,3)
      RFLADR(3,2)=RFLADR(2,3)
!   CONVERT PARTIALS TO XYZ BODY FIXED;VMAT WILL CONTAIN GEOPOTENTIAL
!   FIRST GET THE CONTRIBUTION OF SECOND ORDER PARTIALS OF R,PSI,LAMDA
!   WRT XYZ
      DO 700 J=1,3
      DO 700 I=1,3
  700 TEMP(I,J)=RFLADR(I,1)*CORPAR(1,J)+                                &
     &          RFLADR(I,2)*CORPAR(2,J)+                                &
     &          RFLADR(I,3)*CORPAR(3,J)
      DO 800 I=2,3
      DO 800 J=1,I
  800 VMATB(I,J)=CORPAR(1,I)*TEMP(1,J)+                                 &
     &           CORPAR(2,I)*TEMP(2,J)+                                 &
     &           CORPAR(3,I)*TEMP(3,J)
! SUM INTO VMAT CONTRIBUTION FROM FIRST ORDER PARTIAL  WRT R
      DENOM=VRARAY(4)*VRARAY(5)/VRARAY(1)
      VMATB(2,1)=VMATB(2,1)-X(1)*X(2)/DENOM
      VMATB(2,2)=VMATB(2,2)+(VRARAY(5)-X(2)*X(2))/DENOM
      VMATB(3,1)=VMATB(3,1)-X(1)*X(3)/DENOM
      VMATB(3,2)=VMATB(3,2)-X(2)*X(3)/DENOM
      VMATB(3,3)=VMATB(3,3)+VRARAY(6)/DENOM
! SUM INTO VMATRX PARTIALS WRT PSI
!
!   NOTE:MOST ELEMENTS IN THIS SECTION SHOULD BE DIVIDED BY DENOM WHERE
!     DENOM=VRARAY(5)*VRARAY(5)*VRARAY(7)*VRARAY(6)/VRARAY(2)
!        ON SOME MACHINES THIS CAUSES OVERFLOWS SO DO IN 2 CHUNKS
!
      DENOM1=VRARAY(5)*VRARAY(5)/VRARAY(2)
      DENOM2=VRARAY(6)*VRARAY(7)
      DENOM3=VRARAY(7)
      VMATB(2,1)=VMATB(2,1)+X(1)*X(2)*X(3)*(TWO*VRARAY(6)+VRARAY(5))/   &
     &   DENOM2/DENOM1
      Y2=X(2)**2
      RSQZ=VRARAY(5)*X(3)
      VMATB(2,2)=VMATB(2,2)+(RSQZ*Y2-(RSQZ-TWO*Y2*X(3))*VRARAY(6))/     &
     &   DENOM2/DENOM1
      Z2=TWO*X(3)**2
      VMATB(3,1)=VMATB(3,1)+X(1)*(Z2-VRARAY(5))/DENOM3/DENOM1
      VMATB(3,2)=VMATB(3,2)-X(2)*(VRARAY(5)-Z2)/DENOM3/DENOM1
      VMATB(3,3)=VMATB(3,3)-X(3)*(TWO*VRARAY(5)-Z2)/DENOM3/DENOM1
! PARTIALS WRT LAMBDA
      DENOM=VRARAY(6)*VRARAY(6)
      VMATB(2,1)=VMATB(2,1)+VRARAY(3)*(X(2)**2-X(1)**2)/DENOM
      VMATB(2,2)=VMATB(2,2)-VRARAY(3)*TWO*X(2)*X(1)/DENOM
! GEOPOTENTIAL IS HARMONIC SO,
      VMATB(1,1)=-(VMATB(2,2)+VMATB(3,3))
!   GET MATRIX INTO TRUE OF REFERENCE FROM BODY FIXED.
      TEMP(1,1)=VMATB(1,1)*RMB(1)+VMATB(2,1)*RMB(2)                     &
     &         +VMATB(3,1)*RMB(3)
      TEMP(2,1)=VMATB(2,1)*RMB(1)+VMATB(2,2)*RMB(2)                     &
     &         +VMATB(3,2)*RMB(3)
      TEMP(3,1)=VMATB(3,1)*RMB(1)+VMATB(3,2)*RMB(2)                     &
     &         +VMATB(3,3)*RMB(3)
      TEMP(1,2)=VMATB(1,1)*RMB(4)+VMATB(2,1)*RMB(5)                     &
     &         +VMATB(3,1)*RMB(6)
      TEMP(2,2)=VMATB(2,1)*RMB(4)+VMATB(2,2)*RMB(5)                     &
     &         +VMATB(3,2)*RMB(6)
      TEMP(3,2)=VMATB(3,1)*RMB(4)+VMATB(3,2)*RMB(5)                     &
     &         +VMATB(3,3)*RMB(6)
      TEMP(1,3)=VMATB(1,1)*RMB(7)+VMATB(2,1)*RMB(8)                     &
     &         +VMATB(3,1)*RMB(9)
      TEMP(2,3)=VMATB(2,1)*RMB(7)+VMATB(2,2)*RMB(8)                     &
     &         +VMATB(3,2)*RMB(9)
      TEMP(3,3)=VMATB(3,1)*RMB(7)+VMATB(3,2)*RMB(8)                     &
     &         +VMATB(3,3)*RMB(9)
      VMAT(1,1)=RMB(1)*TEMP(1,1)+RMB(2)*TEMP(2,1)+RMB(3)*TEMP(3,1)
      VMAT(2,1)=RMB(4)*TEMP(1,1)+RMB(5)*TEMP(2,1)+RMB(6)*TEMP(3,1)
      VMAT(3,1)=RMB(7)*TEMP(1,1)+RMB(8)*TEMP(2,1)+RMB(9)*TEMP(3,1)
      VMAT(2,2)=RMB(4)*TEMP(1,2)+RMB(5)*TEMP(2,2)+RMB(6)*TEMP(3,2)
      VMAT(3,2)=RMB(7)*TEMP(1,2)+RMB(8)*TEMP(2,2)+RMB(9)*TEMP(3,2)
      VMAT(3,3)=RMB(7)*TEMP(1,3)+RMB(8)*TEMP(2,3)+RMB(9)*TEMP(3,3)
      VMAT(1,2)=VMAT(2,1)
      VMAT(1,3)=VMAT(3,1)
      VMAT(2,3)=VMAT(3,2)
!   SUM IN GEOPOTENTIAL
      DO 960 I=1,9
  960 VMATRX(I,1)=VMATRX(I,1)+VMAT(I,1)
      RETURN
      END
