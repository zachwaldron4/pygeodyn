      SUBROUTINE TROPDEL     ( ISTA, TROP_HZD, TROP_WZD, TROP_DEL, &
     &                         TROP_DEL_RATE, ATM_PRES,    &
     &                         ELEV,ELEV_DER,LAT_GCN,LAT_GDT,HEI_ELL, &
     &                         MJD,TAI,IUER,LONG)
! ************************************************************************
! *                                                                      *
! *   Routine  TROPDEL computes hydrostatic constituent of the           *
! *   atmosphere zenith path delay, (wet) non-hydrostatic constituent    *
! *   troposphere path delay, multiplies hydrostatic zenith path delay   *
! *   by hydrostatic mapping function, multiplies wet path delay by      *
! *   wet mapping function, adds two components of path delay together   *
! *   and writes then in TROP_DEL. It computes the rate of change of the *
! *   troposphere path delay as well.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     ISTA ( INTEGER*4 ) -- Index of the station in the VTD station    *
! *                           list.                                      *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * TROP_HZD ( REAL*8    ) -- Hydrostatic component of the zenith path   *
! *                           delay. Units: seconds.                     *
! * TROP_WZD ( REAL*8    ) -- Non-hydrostatic, wet component of the      *
! *                           zenith path delay. Units: seconds.         *
! * TROP_DEL ( REAL*8    ) -- Total path delay in the direction to the   *
! *                           source. Units: seconds.                    *
! * TROP_DEL_RATE ( REAL*8    ) -- Rate of change of the total path      *
! *                                delay in the direction to the source. *
! *                                Units: dimensionless.                 *
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
! *  ### 29-JAN-2004   VTD_TROPDEL  v1.1 (c)  L. Petrov 01-OCT-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE

      INCLUDE 'COMMON_DECL.inc'
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI

      DOUBLE PRECISION TROP_HZD, TROP_WZD, TROP_DEL, TROP_DEL_RATE
      DOUBLE PRECISION HEI_GPT,GAC_ELL,TAI,LONG(2),SINEL,COSEL,TANEL
      DOUBLE PRECISION ATM_PRES(2),ELEVAT,ZD,VM1FH,VM1FW,DMJD,DNMFH
      DOUBLE PRECISION ZENDEL_SAA, NMF_H, NMF_H_RATE,NMF_W_RATE,HEIGHT_ORO,HTG
      DOUBLE PRECISION ATM_PRES_LOW, ATM_PRES_HIGH,DLAT,DLON,ZDH,ZDW
      DOUBLE PRECISION      ELEV(2),ELEV_DER(2),LAT_GCN(2),LAT_GDT(2),HEI_ELL(2)
      DOUBLE PRECISION HT_DIFF
      DOUBLE PRECISION AH,AW
      PARAMETER  ( ATM_PRES_LOW  =  50000.0D0 )
      PARAMETER  ( ATM_PRES_HIGH = 120000.0D0 )
      INTEGER  ISTA, IUER, MJD,HZD_MODEL,WZD_MODEL,HMF_MODEL
      INTEGER    VTD__NMFH, VTD__NMFW, VTD__SAA
      INTEGER IOROGRAPHY_ARRAY(91,145)
      PARAMETER  ( VTD__NMFH = 9001 )
      PARAMETER  ( VTD__NMFW = 9002 )
      PARAMETER  ( VTD__SAA  = 9003 )
      LOGICAL  LVMF1
!
      HZD_MODEL=9003
      HMF_MODEL=9001
      WZD_MODEL =0
!The wet zenith path delay is set ot zero because it is not yet
! modelled.

      LVMF1 = .FALSE.

      IF (MODEL(31).EQ.8.OR.MODEL(32).EQ.8.OR.MODEL(36).EQ.8) THEN

        LVMF1 = .TRUE.

      ENDIF

      IF(LVMF1) GO TO 500

!      write(6,*) "TROPDEL: LVMF1 is:", LVMF1

!     write(6,*)' dbg vtd_tropdel ATM_PRES0',ATM_PRES(ISTA)
!     write(6,*)' dbg vtd_tropdel HEI_ELL ',HEI_ELL(ISTA)
      IF ( ATM_PRES(ISTA) .LT. ATM_PRES_LOW   .OR.  &
     &     ATM_PRES(ISTA) .GT. ATM_PRES_HIGH        ) THEN
!
! -------- Surface pressure is insane. Use IMA standard atmosphere instead
! -------- of that.
! -----------GAC_ELL is the local gravity accelratino on the ellipsoid
! -----------formerly computed in vtd_load.f
!
       HEI_GPT = HEI_ELL(ISTA)*9.80665D0/GAC_ELL
       ATM_PRES(ISTA) = 101324.2D0 * EXP ( -1.1859D-4*HEI_GPT &
     &            -1.1343D-9*HEI_GPT**2 -2.5644D-14*HEI_GPT**3 )
      END IF
!
! --- Compute hydrostatic zenith path delay
!
      IF ( HZD_MODEL == VTD__SAA ) THEN
           TROP_HZD = ZENDEL_SAA ( ATM_PRES(ISTA), &
     &                             LAT_GCN(ISTA), &
     &                             HEI_ELL(ISTA))
         ELSE IF ( HZD_MODEL == 0 ) THEN
           TROP_HZD = 0.0D0
      END IF

!
! --- Compute mapping function for hydrostatic path delay and the rate of
! --- change of mppaing function.
! --- Multiply it by zenith path delay and get the total delay
!
      IF ( HMF_MODEL .EQ. VTD__NMFH ) THEN
           TROP_DEL = TROP_HZD * NMF_H ( MJD, TAI, &
     &                LAT_GDT(ISTA), HEI_ELL(ISTA), &
     &                ELEV(ISTA) )
      END IF

      GOTO 900

!**********************  VMF-1 **************************************
!
500   CONTINUE
!
! --- Compute wet (non-hydrostatic) zenith pat delay
!
      IF ( WZD_MODEL == 0 ) THEN
           TROP_WZD = 0.0D0
      END IF
! ADDING CODE TO COMPUTE ZENITH DISTANCE IN RADIANS AND CALL TO GET
! VMF1 VALUES.

      IF(LVMF1) THEN

        CALL READ_OROGRAPHY(IOROGRAPHY_ARRAY)

        DLAT = LAT_GDT(ISTA)/DEGRAD
        DLON = LONG(ISTA)/DEGRAD

!        write(6,*) "TROPDEL: DLAT,DLON :", DLAT,DLON

        CALL DET_HEIGHTS(DLAT,DLON,IOROGRAPHY_ARRAY,HEIGHT_ORO)

        HTG = HEIGHT_ORO

!        write(6,*) "TROPDEL: HEIGHT_ORO :", HEIGHT_ORO

        HT_DIFF = HEI_ELL(ISTA) - HTG

!        write(6,*) "TROPDEL: HEIGHT_STA :", HEI_ELL(ISTA)
!
!        write(6,*) "TROPDEL: HEIGHT_DIFF :", HT_DIFF

        DMJD = DBLE(MJD - 30000)*86400.0D0 + TAI + 32.184D0

        SINEL = SIN(ELEV(ISTA))
        COSEL = COS(ELEV(ISTA))

        TANEL = SINEL/COSEL

        ELEVAT = ATAN(TANEL)

        ZD = (PI * 0.5D0) - ELEVAT

!        HTG = height from orography file

        DNMFH = NMF_H ( MJD, TAI, &
     &                LAT_GDT(ISTA), HEI_ELL(ISTA), &
     &                ELEV(ISTA) )

!        write(6,*) "TROPDEL: DMJD is :",DMJD
!        write(6,*) "TROPDEL: LAT_GDT is :", LAT_GDT
!        write(6,*) "TROPDEL: LONG is :", LONG
!        write(6,*) "TROPDEL: ELLIPSOID HEIGHT is:", HEI_ELL
!        write(6,*) "TROPDEL: Zenith Distance is:", ZD


        CALL VMF_GRID(DMJD,LAT_GDT(ISTA),LONG(ISTA),HTG,HEI_ELL(ISTA),&
     &  HT_DIFF,ZD,VM1FH,VM1FW, ZDH, ZDW,AH,AW)

!        write(6,*) "TROPDEL: DNMFH :", DNMFH
!        write(6,*) "TROPDEL: VM1FH is :", VM1FH
!        write(6,*) "TROPDEL: ZDH is :", ZDH

        ZDH = ZDH /VLIGHT
        ZDW = ZDW /VLIGHT

!        write(6,*) "TROPDEL: ZDH is :", ZDH

        TROP_HZD = ZDH
        TROP_WZD = ZDW

        TROP_DEL = ZDH * VM1FH+ZDW*VM1FW

!        write(6,*) "TROPDEL: TROP_DEL is:", TROP_DEL

!      write(6,*) "TROPDEL: TROP_DEL is:", TROP_DEL
      END IF

!
!**********************  VMF-1 END ***********************************


900   CONTINUE
! ELEV_DER was computed in exec_vlbi before

           TROP_DEL_RATE = TROP_HZD * NMF_H_RATE ( MJD, TAI, &
     &                LAT_GDT(ISTA), HEI_ELL(ISTA), &
     &                ELEV(ISTA) , ELEV_DER(ISTA)) +         &
     &                TROP_WZD * NMF_W_RATE(LAT_GDT(ISTA),   &
     &                ELEV(ISTA) , ELEV_DER(ISTA))

!
      RETURN
      END  SUBROUTINE  TROPDEL
