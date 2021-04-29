!$REFCRS
      SUBROUTINE REFCRS(LGEOPL,SINTHG,COSTHG,DPSR,XPUT,YPUT,MJDSC,      &
     &   FSEC,ROTMT1,ROTMT2,ROTMT3,ROTMT4,ROTMT5,ROTMT6,ROTMT7,         &
     &   ROTMT8,ROTMT9,ETUTC,INDPI,XDPOLE,XDOTP,JRO)
!***********************************************************************
! VERSION 8306    DATE 830615   PGMMR D.ROWLANDS
!
! FUNCTION   COMPUTE THE MATRIX TO ROTATE FROM TRUE OF REFERENCE
!            TO BODY FIXED.
!            POLAR MOTION IS TAKEN INTO ACCOUNT IFF LGEOPL IS TRUE.
!            THE MATRIX TO GO FROM TRUE OF REFERENCE
!            TO TRUE OF MJDSC+FSEC IS ALSO COMPUTED AS A BY-PRODUCT.
!
! INPUT      LGEOPL=TRUE IF POLAR MOTION IS TO BE TAKEN INTO ACCOUNT.
!
!            SINTHG=SIN OF GREENWICH HOUR ANGLE.
!            COSTHG=COS OF GREENWICH HOUR ANGLE.
!            DPSR  =ARRAY CONTAINING POLE OFFSET TIMES
!            XPUT  =ARRAY CONTAINING POLE OFFSET X VALUES
!            YPUT  =ARRAY CONTAINING POLE OFFSET Y VALUES
!            MJDSC =INTEGER SECONDS ET SINCE GEODYN REFERENCE TIME
!            FSEC  =FRACTIONAL REMAINING SECONDS
!            ROTMT1-9 ARE THE ELEMENTS STORED COLUMNWISE OF THE
!                     MATRIX TO GO FROM MEAN OF 1950 TO TRUE OF
!                     MJDSC+FSEC
!            ETUTC =TABLE OF ETUTC TIMES
!
! OUTPUT     OUTPUT IS THROUGH COMMON BLOCKS RMI (INERTIAL-INERTIAL)
!            & RMB (INERTIAL-BODY FIXED)
!
! COMMENTS:
!           GEOPOL WAS ENABLED ALONG WITH THE DYNAMIC POLAR
!           MOTION MODIFICATIONS. 02/19/91 JJM
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      parameter( zero = 0.0D0 )
      parameter( one  = 1.0D0 )
!
      COMMON/CEFMAT/EFMAT(9)
      COMMON/COBGLO/MABIAS,MCBIAS,MPVECT,MINTPL,MPARAM,MSTA,MOBBUF,     &
     &       MNPITR,MWIDTH,MSIZE ,MPART ,MBUF  ,NXCOBG
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CRMB/RMB(9), rmb0(9)
      COMMON/CRMI/RMI(9)
      COMMON/CREFMT/REFMT(9)
      COMMON/CRM5OE/RM5OE(9)
      COMMON/DPLNOR/DXGEO1,DXGEO2,DXFRC1,DXFRC2,XPLNOR
      COMMON/DYNPOL/DPMEP ,DPMXP ,DPMYP ,DPMXDT,DPMYDT,DPMOPT,DPMXPC,   &
     &              DPMYPC,DPMKF ,DPMC21,DPMS21,DPMPD ,DPMUDK,DPMVDK,   &
     &              DPMOCP,DPMNLD,DPMERS,XDYNPL
      COMMON/POLINF/NPOLE,NPOLEA,NUTA,MXUTC,NMODEL,NXPOLI
      COMMON/STPTHG/COSSTG,SINSTG
!
      DIMENSION DPSR(1),XPUT(1),YPUT(1),ETUTC(1)
      DIMENSION DUM1(1),DUM2(1),DUM3(1),NDUM2(1)
      DIMENSION BIHMAT(9),FSECD(1)
      DIMENSION INDPI(1),INDPO(1)
      DIMENSION XDPOLE(3,2,MINTPL)
      DIMENSION XDOTP(3,NPOLE)
!
      DATA KENTRY/0/
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      IF(JRO.EQ.3) THEN
       THG=ATAN2(SINTHG,COSTHG)
       THG=THG+DXFRC2*DEGRAD
       COSTHG=COS(THG)
       SINTHG=SIN(THG)
      ENDIF
      COSSTG=COSTHG
      SINSTG=SINTHG
      IF(JRO.GT.0.AND.JRO.LT.3) THEN
         COSD=ROTMT8
         SIND=ROTMT9
         SINA=-ROTMT1
         COSA=-ROTMT2/SIND
         DEC=ATAN2(SIND,COSD)
         RA=ATAN2(SINA,COSA)
         IF(JRO.EQ.1) THEN
            RA=RA+DXFRC1*DEGRAD
            COSA=COS(RA)
            SINA=SIN(RA)
         ENDIF
         IF(JRO.EQ.2) THEN
            DEC=DEC+DXFRC1*DEGRAD
            COSD=COS(DEC)
            SIND=SIN(DEC)
         ENDIF
         SC1=SINA
         SC2=COSA
         SC3=SIND
         SC4=COSD
         ROTMT1=-SC1
         ROTMT2=-SC2*SC3
         ROTMT3=SC2*SC4
         ROTMT4=SC2
         ROTMT5=-SC1*SC3
         ROTMT6=SC1*SC4
         ROTMT7=0.D0
         ROTMT8=SC4
         ROTMT9=SC3
      ENDIF
      KENTRY = KENTRY + 1
!
!   DUMP MEAN 50 TO TRUE EPOCH INTO COMMOM
!
      RM5OE(1)=ROTMT1
      RM5OE(2)=ROTMT2
      RM5OE(3)=ROTMT3
      RM5OE(4)=ROTMT4
      RM5OE(5)=ROTMT5
      RM5OE(6)=ROTMT6
      RM5OE(7)=ROTMT7
      RM5OE(8)=ROTMT8
      RM5OE(9)=ROTMT9
!
!   GET ROTATION FROM TRUE OF REFERENCE TO TRUE OF EPOCH
!
      RMI(1)=ROTMT1*REFMT(1)+ROTMT4*REFMT(2)+ROTMT7*REFMT(3)
      RMI(2)=ROTMT2*REFMT(1)+ROTMT5*REFMT(2)+ROTMT8*REFMT(3)
      RMI(3)=ROTMT3*REFMT(1)+ROTMT6*REFMT(2)+ROTMT9*REFMT(3)
      RMI(4)=ROTMT1*REFMT(4)+ROTMT4*REFMT(5)+ROTMT7*REFMT(6)
      RMI(5)=ROTMT2*REFMT(4)+ROTMT5*REFMT(5)+ROTMT8*REFMT(6)
      RMI(6)=ROTMT3*REFMT(4)+ROTMT6*REFMT(5)+ROTMT9*REFMT(6)
      RMI(7)=ROTMT1*REFMT(7)+ROTMT4*REFMT(8)+ROTMT7*REFMT(9)
      RMI(8)=ROTMT2*REFMT(7)+ROTMT5*REFMT(8)+ROTMT8*REFMT(9)
      RMI(9)=ROTMT3*REFMT(7)+ROTMT6*REFMT(8)+ROTMT9*REFMT(9)
!
!
      IF( DPMOPT .GT. ZERO .OR. LGEOPL ) THEN
!
         if( kentry .lt. 2 ) then
          WRITE(6,*) '   '
          WRITE(6,*) 'refcrs:  dynamic polar motion is on '
          WRITE(6,*) '   '
         endif
!
! GET THE POLAR MOTION VALUES FOR THIS INTEGRATION STEP AND SAVE IN
! COMMON DYNPOL FOR USE IN DYNAMIC POLAR MOTION COMPUTATION IN GRVTIM.
!
      FSECD(1)=FSEC
      CALL INPOLE(ETUTC,DPSR,XPUT,YPUT,.FALSE.,.FALSE.,.FALSE.,MJDSC,   &
     &            FSECD,1,DUM1,INDPI,NDUM1,NDUM2,INDPO,DUM2,BIHMAT,DUM3,&
     &            XDPOLE,XDOTP)
      IF (DPMOPT .GT. ZERO) THEN
          DPMXPC= BIHMAT(3)
          DPMYPC= BIHMAT(8)
          DPMPD = INDPO(1)
      END IF
!
! DEBUG
!     PRINT 99100,DPMXPC,DPMYPC,DPMPD
!9100 FORMAT(1X,'REFCRS - DPMXPC,DPMYPC,DPMPD ',3E20.12)
! DEBUG
!
      ENDIF
!
!  THE FOLLOWING LINES WERE REPLACED WHEN DYNAMIC POLAR MOTION WAS
!  ADDED AND GEOPOL WAS REMOVED.   WFE 12/88
!
!
      if( lgeopl ) then
!
         if( kentry .lt. 2 ) then
          WRITE(6,*) '   '
          WRITE(6,*) 'refcrs:  geopol option is on '
          WRITE(6,*) 'refcrs:  tides not rotated   '
          WRITE(6,*) '   '
         endif
         EFMAT(1)=BIHMAT(1)*COSTHG-BIHMAT(2)*SINTHG
         EFMAT(2)=BIHMAT(4)*COSTHG-BIHMAT(5)*SINTHG
         EFMAT(3)=BIHMAT(7)*COSTHG-BIHMAT(8)*SINTHG
         EFMAT(4)=BIHMAT(1)*SINTHG+BIHMAT(2)*COSTHG
         EFMAT(5)=BIHMAT(4)*SINTHG+BIHMAT(5)*COSTHG
         EFMAT(6)=BIHMAT(7)*SINTHG+BIHMAT(8)*COSTHG
         EFMAT(7)=BIHMAT(3)
         EFMAT(8)=BIHMAT(6)
         EFMAT(9)=BIHMAT(9)
!
!        ....GET MATRIX TO GO FROM TRUE OF REF TO BODY FIXED
!        ....including the GEOPOL rotation
!
         RMB(1)=EFMAT(1)*RMI(1)+EFMAT(4)*RMI(2)+EFMAT(7)*RMI(3)
         RMB(2)=EFMAT(2)*RMI(1)+EFMAT(5)*RMI(2)+EFMAT(8)*RMI(3)
         RMB(3)=EFMAT(3)*RMI(1)+EFMAT(6)*RMI(2)+EFMAT(9)*RMI(3)
         RMB(4)=EFMAT(1)*RMI(4)+EFMAT(4)*RMI(5)+EFMAT(7)*RMI(6)
         RMB(5)=EFMAT(2)*RMI(4)+EFMAT(5)*RMI(5)+EFMAT(8)*RMI(6)
         RMB(6)=EFMAT(3)*RMI(4)+EFMAT(6)*RMI(5)+EFMAT(9)*RMI(6)
         RMB(7)=EFMAT(1)*RMI(7)+EFMAT(4)*RMI(8)+EFMAT(7)*RMI(9)
         RMB(8)=EFMAT(2)*RMI(7)+EFMAT(5)*RMI(8)+EFMAT(8)*RMI(9)
         RMB(9)=EFMAT(3)*RMI(7)+EFMAT(6)*RMI(8)+EFMAT(9)*RMI(9)
!
!
!
         EFMAT(1)= COSTHG
         EFMAT(2)=-SINTHG
         EFMAT(3)= ZERO
         EFMAT(4)= SINTHG
         EFMAT(5)= COSTHG
         EFMAT(6)= ZERO
         EFMAT(7)= ZERO
         EFMAT(8)= ZERO
         EFMAT(9)= ONE
!
!        ....GET MATRIX TO GO FROM TRUE OF REF TO BODY FIXED
!        ....without the GEOPOL rotation
!
         rmb0(1)=EFMAT(1)*RMI(1)+EFMAT(4)*RMI(2)+EFMAT(7)*RMI(3)
         rmb0(2)=EFMAT(2)*RMI(1)+EFMAT(5)*RMI(2)+EFMAT(8)*RMI(3)
         rmb0(3)=EFMAT(3)*RMI(1)+EFMAT(6)*RMI(2)+EFMAT(9)*RMI(3)
         rmb0(4)=EFMAT(1)*RMI(4)+EFMAT(4)*RMI(5)+EFMAT(7)*RMI(6)
         rmb0(5)=EFMAT(2)*RMI(4)+EFMAT(5)*RMI(5)+EFMAT(8)*RMI(6)
         rmb0(6)=EFMAT(3)*RMI(4)+EFMAT(6)*RMI(5)+EFMAT(9)*RMI(6)
         rmb0(7)=EFMAT(1)*RMI(7)+EFMAT(4)*RMI(8)+EFMAT(7)*RMI(9)
         rmb0(8)=EFMAT(2)*RMI(7)+EFMAT(5)*RMI(8)+EFMAT(8)*RMI(9)
         rmb0(9)=EFMAT(3)*RMI(7)+EFMAT(6)*RMI(8)+EFMAT(9)*RMI(9)
!
!
      else
!
         if( kentry .lt. 2 ) then
          WRITE(6,*) '   '
          WRITE(6,*) 'refcrs:  geopol option is off '
          WRITE(6,*) '   '
         endif
!
         EFMAT(1)= COSTHG
         EFMAT(2)=-SINTHG
         EFMAT(3)= ZERO
         EFMAT(4)= SINTHG
         EFMAT(5)= COSTHG
         EFMAT(6)= ZERO
         EFMAT(7)= ZERO
         EFMAT(8)= ZERO
         EFMAT(9)= ONE
!
!        ....GET MATRIX TO GO FROM TRUE OF REF TO BODY FIXED
!        ....without the GEOPOL rotation
!
         RMB(1)=EFMAT(1)*RMI(1)+EFMAT(4)*RMI(2)+EFMAT(7)*RMI(3)
         RMB(2)=EFMAT(2)*RMI(1)+EFMAT(5)*RMI(2)+EFMAT(8)*RMI(3)
         RMB(3)=EFMAT(3)*RMI(1)+EFMAT(6)*RMI(2)+EFMAT(9)*RMI(3)
         RMB(4)=EFMAT(1)*RMI(4)+EFMAT(4)*RMI(5)+EFMAT(7)*RMI(6)
         RMB(5)=EFMAT(2)*RMI(4)+EFMAT(5)*RMI(5)+EFMAT(8)*RMI(6)
         RMB(6)=EFMAT(3)*RMI(4)+EFMAT(6)*RMI(5)+EFMAT(9)*RMI(6)
         RMB(7)=EFMAT(1)*RMI(7)+EFMAT(4)*RMI(8)+EFMAT(7)*RMI(9)
         RMB(8)=EFMAT(2)*RMI(7)+EFMAT(5)*RMI(8)+EFMAT(8)*RMI(9)
         RMB(9)=EFMAT(3)*RMI(7)+EFMAT(6)*RMI(8)+EFMAT(9)*RMI(9)
!
         do 4000 i=1,9
            rmb0(i) = rmb(i)
 4000    continue
!
      endif
!        if( kentry .lt. 2 ) then
!         WRITE(6,*) '   '
!         WRITE(6,*) 'refcrs: rmi ', rmi
!         WRITE(6,*) 'refcrs: rmb ', rmb
!         WRITE(6,*) 'refcrs: rmb0 ', rmb0
!         WRITE(6,*) '   '
!        endif
!
      RETURN
      END
