!$ANTSAC
      SUBROUTINE ANTSAC(ROT,XV,YV,ZV,PHASEC,NDIMA,NDIMZ,PHAHDR,     &
     &                  ISEQ,PHC,IUPDN,L2SATL)
!********1*********2*********3*********4*********5*********6*********7**
! ANTSAC                                         PGMR - D. ROWLANDS
!
!
! FUNCTION:  COMPUTE THE ANTENNA MAP CORRECTION AT A USER SATELLITE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ROT      I    A    MATRIX FROM SPACECRAFT BF TO TOR (ROUTINE USES
!                      TRANSPOSE)
!   XV       I    S    X COMP OF TOR UNIT VECTOR FROM U SAT TO GPS SAT
!   YV       I    S    Y COMP OF TOR UNIT VECTOR FROM U SAT TO GPS SAT
!   ZV       I    S    Z COMP OF TOR UNIT VECTOR FROM U SAT TO GPS SAT
!                      STATION TO SATELLITE
!   PHASEC   I    A    ANTENNA CORRECTION ARRAY. CORRECTIONS SHOULD
!                      BE ADDED TO COMPUTED OR
!                         SUBTRACTED FROM OBSERVED
!                      THE FIRST  DIMENSION OF PHASEC IS FOR AZIMUTH
!                      THE SECOND DIMENSION OF PHASEC IS FOR ELEVATION
!   NDIMA    I    S    AZIMUTH DIMENSION
!   NDIMZ    I    S    ZENITH  DIMENSION
!   PHAHDR   I    A    PHASE TABLE HEADER RECORD
!                      PHAHDR(1): NUMBER OF ZENITH  ANGLES
!                      PHAHDR(2): STARTING  ZENITH  ANGLES
!                      PHAHDR(3): STEP OF   ZENITH  ANGLES
!                      PHAHDR(4): NUMBER OF AZIMUTH ANGLES
!                      PHAHDR(5): STARTING  AZIMUTH ANGLES
!                      PHAHDR(6): STEP OF   AZIMUTH ANGLES
!   ISEQ     I    S    SEQUENCE NUMBER (1-4) OF A GPS MEASUREMENT
!                      (i.e. WHICH PART OF THE DIFFERENCED MEASUREMENT IS
!                       THIS CALL BEING MADE FOR)
!   PHC      O    S    CORRECTION FROM PHASEC
!   IUPDN    I    S    1=UP (HIGH) SATELLITE, 2=DOWN (LOW) SATELLITE
!   L2SATL   I    S    FLAG SET TO .TRUE. WHEN A LINK CONTAINS TWO SATS
!
!   NOTE               THE L2SATL LINK IS NECSSARY. WHEN LSAT2L IS TRUE.
!                      THE VLOW ORIENTATION IS BEING USED EVEN WHEN
!                      IUPDN=1. THIS IS FOR SLR AND DORIS APPLICATIONS
!                      SEE THE SECTION OF OFFDEL THAT SETS UP MT 39 AND 51
!
! COMMENTS: SEE VOLUME 3 (ANTPHC CARD) FOR STRUCTURE OF PHASEC ARRAY
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)

!     COMMON/ADJANT/
!     COMMON/ADJANI/
!     COMMON/TRKQP/
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/NPCOM /NPNAME,NPVAL(92),NPVAL0(92),IPVAL(92),IPVAL0(92),   &
     &              MPVAL(28),MPVAL0(28),NXNPCM
      COMMON/NPCOMX/IXARC ,IXSATP,IXDRAG,IXSLRD,IXACCL,IXGPCA,IXGPSA,   &
     &              IXAREA,IXSPRF,IXDFRF,IXEMIS,IXTMPA,IXTMPC,IXTIMD,   &
     &              IXTIMF,IXTHTX,IXTHDR,IXOFFS,IXBISA,IXFAGM,IXFAFM,   &
     &              IXATUD,IXRSEP,IXACCB,IXDXYZ,IXGPSBW,IXCAME,IXBURN,  &
     &              IXGLBL,IXGPC ,IXGPS, IXTGPC,IXTGPS,IXGPCT,IXGPST,   &
     &              IXTIDE,IXETDE,IXOTDE,IXOTPC,IXOTPS,IXLOCG,IXKF  ,   &
     &              IXGM  ,IXSMA ,IXFLTP,IXFLTE,IXPLTP,IXPLTV,IXPMGM,   &
     &              IXPMJ2,IXVLIT,IXEPHC,IXEPHT,IXH2LV,IXL2LV,IXOLOD,   &
     &              IXPOLX,IXPOLY,IXUT1 ,IXPXDT,IXPYDT,IXUTDT,IXVLBI,   &
     &              IXVLBV,IXXTRO,IXBISG,IXSSTF,IXFGGM,IXFGFM,IXLNTM,   &
     &              IXLNTA,IX2CCO,IX2SCO,IX2GM ,IX2BDA,IXRELP,IXJ2SN,   &
     &              IXGMSN,IXPLNF,IXPSRF,IXANTD,IXTARG,                 &
     &              IXSTAP,IXSSTC,IXSSTS,IXSTAV,IXSTL2,                 &
     &              IXSTH2,IXDPSI,IXEPST,IXCOFF,IXTOTL,NXNPCX
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)
      DIMENSION PHASEC(NDIMA,NDIMZ),PHAHDR(6)
      DIMENSION ROT(9)
      LOGICAL LDEBUG

!-------------------------------------------------------------------

      LDEBUG=.FALSE.

!     In this case ROT is the matrix of rotation from inertial to SBF

      XXX=ROT(1)*XV+ROT(2)*YV+ROT(3)*ZV
      YYY=ROT(4)*XV+ROT(5)*YV+ROT(6)*ZV
      ZZZ=ROT(7)*XV+ROT(8)*YV+ROT(9)*ZV

!     VLOW are the vectors of orientation of the R(y) matrix with
!     y = 15 degrees for JASON2. It should be not applied since
!     for the PCV maps we want them to be in the horizontal plane

      IF (L2SATL .AND. IUPDN == 1) THEN
          ! FOR "HIGH" (e.g. GPS) satellites in SAT-SAT links
          XX=VHIGH(1,1,ISEQ)*XXX+VHIGH(2,1,ISEQ)*YYY+VHIGH(3,1,ISEQ)*ZZZ
          YY=VHIGH(1,2,ISEQ)*XXX+VHIGH(2,2,ISEQ)*YYY+VHIGH(3,2,ISEQ)*ZZZ
          ZZ=VHIGH(1,3,ISEQ)*XXX+VHIGH(2,3,ISEQ)*YYY+VHIGH(3,3,ISEQ)*ZZZ
      ELSE
          XX=VLOW(1,1,ISEQ)*XXX+VLOW(2,1,ISEQ)*YYY+VLOW(3,1,ISEQ)*ZZZ
          YY=VLOW(1,2,ISEQ)*XXX+VLOW(2,2,ISEQ)*YYY+VLOW(3,2,ISEQ)*ZZZ
          ZZ=VLOW(1,3,ISEQ)*XXX+VLOW(2,3,ISEQ)*YYY+VLOW(3,3,ISEQ)*ZZZ
      END IF

!      print*, XX, YY, ZZ
!       XX=XXX
!       YY=-YYY
!       ZZ=-ZZZ
!       write(6,'(A,3(1x,E15.7))')'antsac: XX,YY,ZZ ',  XX, YY, ZZ

!       IF(LDEBUG) THEN
!        PRINT 90010,-VLOW(1,1,ISEQ), VLOW(2,1,ISEQ), -VLOW(3,1,ISEQ)
!        PRINT 90011, VLOW(1,2,ISEQ), VLOW(2,2,ISEQ),  VLOW(3,2,ISEQ)
!        PRINT 90012,-VLOW(1,3,ISEQ), VLOW(2,3,ISEQ), -VLOW(3,3,ISEQ)
!90010 FORMAT(1X,&
!      'DEBUG ANTSAC.f, VLOW(1,1,ISEQ), VLOW(2,1,ISEQ), VLOW(3,1,ISEQ):',&
!       3(1X,F19.6))
!90011 FORMAT(1X,&
!       'DEBUG ANTSAC.f, VLOW(1,2,ISEQ), VLOW(2,2,ISEQ), VLOW(3,2,ISEQ):',&
!       3(1X,F19.6))
!90012 FORMAT(1X,&
!       'DEBUG ANTSAC.f, VLOW(1,3,ISEQ), VLOW(2,3,ISEQ), VLOW(3,3,ISEQ):',&
!       3(1X,F19.6))
!       ENDIF

      ZEN=ACOS(ZZ)/DEGRAD
      AZ=ATAN2(YY,XX)/DEGRAD
      AZ=AZ+3600.D0
      AZ=MOD(AZ,360.D0)
      AZ=360.D0-AZ
      AZ=MOD(AZ,360.D0)

      !write(6,'(A,2(1x,F12.4))') 'antsac:  az, zen=',az, zen

!     print*,'ddd in antsac az= (-yy,xx)',dmod(3600.d0+datan2(-yy,xx)/&
!    &degrad,360.d0)
!     print*,'ddd in antsac az= (yy,-xx)',dmod(3600.d0+datan2(yy,-xx)/&
!    &degrad,360.d0)
!     print*,'ddd in antsac az= (-yy,-xx)',dmod(3600.d0+datan2(-yy,-xx)/&
!    &degrad,360.d0)

! CALCULATE THE ELEVATION ANGLES
! ZENITH IS FROM 0 TO 90 DEGREES.
! WE NEED TWO-DIMENSIONAL INTERPOLATION

      J1=(ZEN-PHAHDR(2))/PHAHDR(3)
      J1=J1+1
      IF(J1.GE.NDIMZ) J1=NDIMZ-1
      J2=J1+1
      I1=(AZ-PHAHDR(5))/PHAHDR(6)
      I1=I1+1
      I2=I1+1
      IF(I2.GT.NDIMA) I2=1
      ZEN1=PHAHDR(2)+DBLE(J1-1)*PHAHDR(3)
      PHV1=PHASEC(I1,J1)+(ZEN-ZEN1)*(PHASEC(I1,J2)-PHASEC(I1,J1))/ &
     &                PHAHDR(3)
      PHV2=PHASEC(I2,J1)+(ZEN-ZEN1)*(PHASEC(I2,J2)-PHASEC(I2,J1))/ &
     &                PHAHDR(3)

      AZ1=PHAHDR(5)+DBLE(I1-1)*PHAHDR(6)

      PHC  = PHV1+(AZ-AZ1)*(PHV2-PHV1)/PHAHDR(6)

!      write(6,'(A,2(1x,I10))') 'antsac: ndima, ndime ',ndima,ndime
!      write(6,'(A,3(1x,E15.7))') &
!            'antsac: phahdr(1),phahdr(2),phahdr(3) ', &
!                     phahdr(1),phahdr(2),phahdr(3)
!      write(6,'(A,3(1x,E15.7))') &
!            'antsac: phahdr(4),phahdr(5),phahdr(6) ', &
!                     phahdr(4),phahdr(5),phahdr(6)

!      write(6,'(A,3(1x,E15.7))') &
!            'antsac: phc, zen,az ', phc, zen,az



!          !stop
!!!!!CODE FOR TUNING JASON-2 PHASE MAP
! ADD PARTIALS HERE FOR JASON-2 PHASE CENTER MAP
! FIRST SAVE THE POINTERS FOR THE FOUR PARAMETERS USED FOR INTERPOLATION
!
!      IF(LSTINR.AND.NPVAL0(IXFGGM).GT.0) THEN  ! fantom global geometric
!
!        INTPAR(IMQP,1)=(J1-1)*NDIMA+I1
!        INTPAR(IMQP,2)=(J1-1)*NDIMA+I2
!        INTPAR(IMQP,3)=(J2-1)*NDIMA+I1
!        INTPAR(IMQP,4)=(J2-1)*NDIMA+I2
!         print*,'ddd: ',imqp,intpar(imqp,1),intpar(imqp,3)
! SECOND CALCULATE THE PARTIAL DERIVATIVE
!        FACTR=(AZ-AZ1)/PHAHDR(6)
!        FACTL=1.D0-FACTR
!        FACTD=(ZEN-ZEN1)/PHAHDR(3)
!        FACTU=1.D0-FACTD
! NOTE WE HAVE USED A MINUS SIGN WHEN CALCULATING THE PARTIALS.
! SEE THE COMMENT IN ROBSUM BELOW
! NOTE: THE MINUS SIGN USED IS TO ACCOUNT FOR THE FACT THAT THIS OFFSET
! IS BEING COMPUTED FOR THE SATELLITE FROM WHICH THE UNIT VECTOR URNM
! IS POINTING. IN THIS CASE WHEN THE DOT PRODUCT BETWEEN THE SATELLITE
! OFFSET AND URNM GIVES +1, THE COMPUTED RANGE SHOULD BE SHORTENED
! CD'S COMMENT BELOW
! THE UNIT VECTOR IS POINTING TO THE GPS SATELLITE FROM
! USER SATELLITE, WHILE IN THE CASE OF STATIONS, THE UNIT VECTOR IS
! FROM STATIONS TO GPS SATELLITES. THIS IS WHY THERE IS NO MINUS IN ROBSUM
! WHEN EVALUATING THE OFFSET FOR STATION TO GPS RANGES.
!        ANTPAR(IMQP,1)=FACTL*FACTU
!        ANTPAR(IMQP,2)=FACTR*FACTU
!        ANTPAR(IMQP,3)=FACTL*FACTD
!        ANTPAR(IMQP,4)=FACTR*FACTD
!       print*, 'ddd ',imqp,intpar(imqp,1),intpar(imqp,3),factl,factu
!      ENDIF
!!!!!END OF CODE FOR TUNING JASON-2 PHASE MAP



      RETURN
      END
