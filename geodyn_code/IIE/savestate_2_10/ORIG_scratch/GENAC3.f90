!$GENAC3
      SUBROUTINE GENAC3(MJDSEC,FSEC,XS,DXDDA,PXDDT,VMATRX,LORBIT,       &
     &                  NEQN,ITACC,ACC,STACC,LAJGP)
!********1*********2*********3*********4*********5*********6*********7**
! GENAC3           83/11/16            8311.0    PGMR - D. ROWLANDS
!
! FUNCTION:  (1) CALCULATE ACCELERATION DUE TO GENERAL ACCELERATION
!            (2) IF AN ADJUSTMENT IS BEING DONE,CALCULATE THE
!                CONTRIBUTION FROM GENERAL ACCELERATION THE VMATRX
!            (3) IF THESE ACCELERATIONS ARE BEING ADJUSTED,CALCULATE
!                THEIR EXPLICIT PARTIALS.
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    CURRENT INTEGRATION TIME (ET SECONDS)
!                      SINCE GEODYN REFERENCE TIME
!   FSEC     I    S    REMAINING FRACTIONAL SECONDS
!   XS       I    A    TRUE OF INTEGRATION STATE OF SATELLITE
!   DXDDA    O    A    TRUE OF INTEGRATION GENERAL ACCELERATION
!                      CONTRIBUPION TO ACC
!   PXDDT    O    A    EXPLICIT PARTIALS OF FORCE MODEL PARAMETRS
!                      IN TRUE OF REFERENCE
!   VMATRX  I/O   A    VMATRX IN TRUE OF INTEGRATION WITHOUT
!                      DRAG CONTRIBUTION AS INPUT
!                      VMATRX IN TRUE OF INTEGRATION WITH
!                      GENERAL ACCELERATION CONTRIBUTION AS OUTPUT
!   LORBIT   I    S    .TRUE. IF ORBIT GENERATOR RUN
!   NEQN     I    S    NUMBER OF FORCE MODEL PARAMETERS
!   ITACC    I    S    TYPE ACCELERATION (1 ALONG TRACK,2 RADIAL,
!                      3 CROSS TRACK
!   ACC      I    A    GENERAL ACCELERATION FROM ZERO-IORGA ORDERS INCL
!                      UDING CORRCTIONS TO HIGHEST ORDER
!                      IN VARIOUS  PERIODS.
!   STACC    I    A    STOP TIMES (EPHEMERIS SECONDS SINCE GEODYN REF.TI
!                      OF CORRECTION  PERIODS
!   LAJGP    I    A    LOGICAL TELLING WHETHER EACH PERIOD IS ADJUSTED
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/CRMI/RMI(9)
      COMMON/SETADJ/LSADRG(4),LSASRD(4),LSAGA(4),LSAGP,LSATID,LSAGM,    &
     &              LSADPM,LSAKF,LSADNX,LSADJ2,LSAPGM
      COMMON/SETAPT/                                                    &
     &       JSADRG(1),JSASRD(1),JSAGA(1),JFAADJ,JTHDRG,JACBIA,JATITD,  &
     &       JDSTAT,JDTUM ,                                             &
     &       JSACS, JSATID,JSALG,JSAGM,JSAKF,JSAXYP,JSADJ2,JSAPGM,      &
     &       JFGADJ,JLTPMU,JLTPAL,JL2CCO,JL2SCO,JL2GM,JLRELT,           &
     &       JLJ2SN,JLGMSN,JLPLFM,JL2AE,JSAXTO,JSABRN
      COMMON/SETIOR/IORDRG,IORSRD,IORGA,IPDDRG,IPDSRD,IPDGA
      COMMON/STARTT/ESSTRT,FSSTRT
!
      DIMENSION XS(6),DXDD(3),PXDDT(NEQN,3),VMATRX(3,6),                &
     &          STACC(1),ACC(1),TM(3),VECT(3),FACT(3),LAJGP(1),         &
     &          UHCL(3,3),DXDDA(3),                                     &
     &          AHCL(3)
!
      DATA ZERO/0.D0/,ONE/1.D0/,TWO/2.D0/
      DATA KCALL/0/
      DATA LDEBUG/.FALSE./
!
!
!**********************************************************************
!* SPART OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!
!
      DXDDA(1)=ZERO
      DXDDA(2)=ZERO
      DXDDA(3)=ZERO
      LDEBUG = .FALSE.
!CC   IF( FSSTRT .GE. 7190.0 .AND. FSSTRT .LT. 7800 ) LDEBUG = .TRUE.
!CC   IDBG = 0
!CC   IF( LDEBUG ) IDBG = 1
!CC   KCALL = KCALL+1
!CC   IF( KCALL .GT. 20) STOP 20
!
!-----------------------------------------------------------------------
!
!     ....PRINT OUT INPUT STUFF
!
!
!-----------------------------------------------------------------------
!
!     ....JSAGA(1) IS POINTER TO FIRST ADJUSTED GENERAL ACCELERATION
!
      KPART=JSAGA(1)
!
      IF(LDEBUG) WRITE(6,11001) KPART
11001 FORMAT(/' '/' GENACC: KPART  ', I12/' '/' '/)
!
!----------------------------------------------------------
!
!
!     ....DETERMINE IF THERE ARE ANY ADJUSTED PERIODS
!     ....IF THERE ARE, ZERO OUT PARTIALS FOR THESE PERIODS
!
!
!CC      DO 667 KDAVE=1,NEQN
!CC      IF(LDEBUG) WRITE(6,11017)  KDAVE,(PXDDT(KDAVE,JQ),JQ=1,3)
!CC667   CONTINUE
!
      LANYAJ = .FALSE.
      NUMADJ = 0
      DO 401 JJ=1,IPDGA
      LANYAJ = LANYAJ .OR. LAJGP(JJ)
      IF( LAJGP(JJ) ) NUMADJ = NUMADJ +  1
  401 END DO
      IF( .NOT. LANYAJ ) GO TO 402
      DO 403  JJ=1,NUMADJ
      PXDDT(KPART+JJ-1,1) = ZERO
      PXDDT(KPART+JJ-1,2) = ZERO
      PXDDT(KPART+JJ-1,3) = ZERO
  403 END DO
!
  402 CONTINUE
!
!CC      DO 668 KDAVE=1,NEQN
!CC      IF(LDEBUG) WRITE(6,11017)  KDAVE,(PXDDT(KDAVE,JQ),JQ=1,3)
!CC668   CONTINUE
!-----------------------------------------------------------------------
!
      TM(1)=ONE
!--------------------------
!     ....FIX IORGA = 1 FOR PATCH
      IORGA = 1
!--------------------------
      TTEST=MJDSEC+FSEC
      IF(LDEBUG) WRITE(6,10001) TM(1), IORGA, TTEST, FSSTRT
10001 FORMAT(' GENACC:TM(1), IORGA, TTEST, FSSTRT ', G24.16,            &
     &    I12, 2G24.16)
!
!
!
!     ....CALCULATE TRANSFORMATION FROM XYZ TO HCL
!
      CALL UVCOMP( UHCL, XS, .FALSE. )
      IF(LDEBUG) WRITE(6,10002) XS, UHCL
10002 FORMAT(' GENACC:XS'/1X,6G20.10/' UHCL '/(1X,3G24.16))
!-----------------------------------------------------------------------
!
!     ....CALCULATE  VALUES FOR SUBSCRIPTS
!
!     ....NPATCH = NUMBER OF GENACC CARDS FOR EACH COMPONENT
      NPATCH = ( IPDGA   )/ 3
!
!     ....NDELT IS THE OFFSET TO GET VALUES IN 2ND AND 3RD SETS
      NDELT = 0
      IF(LDEBUG) WRITE(6,10003) NPATCH, NDELT, IPDGA
10003 FORMAT(' GENACC:NPATCH, NDELT, IPDGA',3I12)
!----------------------------------------------------------------------
!
!     ....KTYPE IS THE ACCELERATION COMPONENT (1=L, 2=H, 3=C)
!
      DO 5 KTYPE = 1,3
!
!------------------------------------------------------------------
!
!     ....SET VECT TO PROPER COMPONENT
!
      IF( KTYPE .EQ. 1) ISUB = 3
!CC   IF( KTYPE .EQ. 2) ISUB = 1
!CC   IF( KTYPE .EQ. 3) ISUB = 2
!
!     ....CHANGE THE ORDER SO THAT CROSS AND RADIAL ARE DONE
!     ....AS IN DOCUMENTATION  (FIRST, L, THEN C, THEN H)
!CC   IF( KTYPE .EQ. 2) ISUB = 2
!CC   IF( KTYPE .EQ. 3) ISUB = 1
      ISUB = 4 - KTYPE
!
      DO 6 J=1,3
      VECT(J) = UHCL(J,ISUB)
      AHCL(J) = ZERO
      IF( J .EQ. ISUB ) AHCL(J) = ONE
    6 END DO
!
      IF(LDEBUG) WRITE(6,10004) KTYPE, ISUB, VECT
10004 FORMAT(' GENACC:KTYPE, ISUB, VECT ',2I12/1X,3G24.16)
!-------------------------------------------------------------------
!
      DO 10 K = 1, NPATCH
!
!     ....K WILL BE THE SUBSCRIPT FOR THE TIMES K:1 -> NPATCH
!     ....K2 WILL BE FOR OTHER ARRAYS K2:1+NDELT -> NPATCH+NDELT
!
      K2 = K + NDELT
      IF(LDEBUG) WRITE(6,10005) K, K2
10005 FORMAT(' GENACC:K, K2 ',2I12)
!
!CC   LAST = .FALSE.
!CC   IF( K .EQ. NPATCH ) LAST = .TRUE.
!-------------------------------------------------
      TM(2) = ZERO
      IF(IORGA.GT.1) TM(2)=FSSTRT
      IF(IORGA.GT.2) TM(3)=TM(2)*TM(2)
         YMAG=ZERO
!CC      IPD=0
!CC      IUP=IORGA-1
!CC      IF(IUP.LE.0) GO TO 610
!CC      DO 600 I=1,IUP
!CC 600  YMAG=YMAG+TM(I)*ACC(I)
!CCC ADD CORRECTION TO HIGHEST ORDER TERM IF NECESSARY
!
!610   IF(IPDGA.GT.0) GO TO 620
      IF(IPDGA.GT.0) GO TO 620
!
!CC      YMAG=YMAG+TM(IORGA)*ACC(IORGA)
!CC      IF(LDEBUG) WRITE(6,10006) IORGA, IUP, IPD, YMAG
!CC10006 FORMAT(' GENACC:IORGA, IUP, IPD, YMAG ',3I12,G24.16)
!
         GO TO 750
  620 CONTINUE
!
!---------------------------------
!     ....SET YMAG TO THE ACCELERATION OF THE K TH PERIOD
      YMAG = ZERO
      K2A = K2 + 1
      IF( K2A .LE. IPDGA ) YMAG = ACC(K2A)
!---------------------------------
!
!     ....THE NEXT FEW LINES DETERMINE WHICH TIME PERIOD YOU
!     ....ARE IN
!
      IF(LDEBUG) WRITE(6,10007) K,K2, TTEST, STACC(K)
10007 FORMAT(' GENACC: K, K2, TTEST, STACC(K) ',2I12,2G24.16)
      IF( K .GT. 1 ) GO TO 641
      IF(  TTEST .LT. STACC(K) ) GO TO 660
      GO TO 9
  641 CONTINUE
      IF( TTEST .GE. STACC(K-1)  .AND. TTEST .LT. STACC(K) )GO TO 660
!-----------------------------------------------
!     ....IF YOU GET HERE, YOU MUST BE IN THE OUTSIDE PERIOD
      GO TO 9
!-----------------------------------------------
!CC      IPGN=IPDGA
!     ....CORRECT?????????????????????
!CC   IPGN = NPATCH
!CC   IPD = 0
!CC   GO TO 700
!
  660 CONTINUE
      IPGN = K - 1
      IPD = K
      IF(LDEBUG) WRITE(6,10008) K, IPGN, IPD
10008 FORMAT(' GENACC:K, IPGN, IPD ',3I12)
!
!-------------------------------------------------
!     ....ESSTRT IS TIME OF EPOCH IN SECONDS
!
      T0=ESSTRT
!
!
      TD=TTEST-T0
!     ....POWER RETURNS TDN = TD ** IORGA  1 IF IORGA=1
!     CALL POWER(IORGA,TD,TDN,.TRUE.)
!
!     YMAG=YMAG+TDN*ACC(IPD+IORGA)
!
!     IF(LDEBUG) WRITE(6,10009) K, IPD, IORGA, ACC(IPD+IORGA),
!    1      TTEST, TD, TDN, YMAG
!0009 FORMAT(' GENACC:K, IPD, IORGA, ACC(IPD+IORGA),TTEST ',
!    1  3I12,2X,2G24.16/' TD, TDN, YMAG ',3G24.16/)
!
  750 CONTINUE
!
      DXDD(1) = YMAG * VECT(1)
      DXDD(2) = YMAG * VECT(2)
      DXDD(3) = YMAG * VECT(3)
      DXDDA(1) = DXDDA(1) + DXDD(1)
      DXDDA(2) = DXDDA(2) + DXDD(2)
      DXDDA(3) = DXDDA(3) + DXDD(3)
!
!     ....AHCL IS ACCELERATION IN HCL COORDINATES
!
      AHCL(1) = YMAG * AHCL(1)
      AHCL(2) = YMAG * AHCL(2)
      AHCL(3) = YMAG * AHCL(3)
!
!CC      IF( ABS(YMAG) .GT. 1.D-14) WRITE(6,9991) YMAG, VECT, DXDD
!CC9991  FORMAT(' GENACC: YMAG, VECT ',4G24.16/' DXDD ',3G24.16)
!----------------------------------------------------------------------
!
!     ....WRITE MESSAGE IN RUN WHEN ACCELERATION IS APPLIED
!
      IF( ABS(YMAG) .GT. 1.D-7 ) WRITE(6,12000) KTYPE, K,               &
     &                               FSSTRT, DXDD, YMAG
12000 FORMAT(' COMP:',I2,2X,'PERIOD:',I2,2X,'TIME:',G11.4,2X,'ACC:',    &
     &     3G16.8,5X,'ACC MAG: ',G16.8)
      IF(LORBIT) GO TO 10
!---------------------------------------------------------------------
!
!     ....CALCULATE THE V-MATRIX CONTRIBUTION
!
      IF(LDEBUG) WRITE(6,10010)
10010 FORMAT(' GENACC:ENTER THVMAT '/)
      CALL  THVMAT( AHCL, XS, UHCL, VMATRX )
      IF(LDEBUG) WRITE(6,10011)
10011 FORMAT(' GENACC:LEAVE THVMAT '/)
!
!CC   IF(ABS(YMAG) .GT. 1.D-15) WRITE(6,4000)
!CC  1            ((VMATRX(I,J),I=1,3),J=1,6)
!CC4000  FORMAT(' GENACC: VMATRX '/(1X,3D24.16))
!
!-------------------------------------------------
!
!   ADD VMATRX CONTRIBUTION & (IF NECESSARY) CALCULATE EXPLICIT
!   PARTIALS
!
! PUT PARTIALS INTO TRUE OF REFERENCE
      FACT(1)=VECT(1)*RMI(1)+VECT(2)*RMI(2)+VECT(3)*RMI(3)
      FACT(2)=VECT(1)*RMI(4)+VECT(2)*RMI(5)+VECT(3)*RMI(6)
      FACT(3)=VECT(1)*RMI(7)+VECT(2)*RMI(8)+VECT(3)*RMI(9)
!
      IF(LDEBUG) WRITE(6,10012) RMI
10012 FORMAT(' GENACC:RMI '/(1X,3G24.16))
      IF(LDEBUG) WRITE(6,10013) VECT, FACT
10013 FORMAT(' GENACC:VECT ',1X,3G24.16/'  FACT ',3G24.16)
!
!
!------------------------------------------------------------
!
!     ....FROM HERE ON  FOR ADJUSTED TIME PERIODS ONLY
!     ....RETURN IF NO ADJUSTED TIME PERIODS
!
      IF(LDEBUG) WRITE(6,10015) LSAGA
10015 FORMAT(' GENACC:LSAGA ',4L5)
!
      IF(.NOT.LSAGA(4)) GO TO 10
!
!--------------------------------------------------------------
!
      IF(LDEBUG) WRITE(6,10016) K, K2, LAJGP(K2)
10016 FORMAT(' GENACC: K, K2, LAJGP(K2) ',2I12, L5)
!
      T0=ESSTRT
!--------------------------------------------------------------
!     ....THIS TEST MAKES SURE THAT THE SETUP IS CORRECT
!     ....SO THAT IF A PERIOD IS ADJUSTED (OR NOT ADJUSTED) FOR
!     ....THE ALONG-TRACK, THEN THE CORRESPONDING PERIODS FOR
!     ....H AND C MUST BE ADJUSTED (OR NOT)
!
!     .... LOGICAL OPERATOR NEQV IS EQUIVALENT TO XOR (OR NE)
!     .... AND IS FORTRAN 77 APPROVED
!
      IF( LAJGP(K) .NEQV. LAJGP(K2) ) GO TO 8000
!--------------------------------------------------------------
!
      IF(.NOT.LAJGP(K2)) GO TO 10
!
      PXDDT(KPART,1)=FACT(1)
      PXDDT(KPART,2)=FACT(2)
      PXDDT(KPART,3)=FACT(3)
!
!CC      IF(ABS(YMAG) .GT. 1.D-15 ) WRITE(6,10017) I, KPART,
!CC     1     (PXDDT(KPART,JQ),JQ=1,3)
!CC10017 FORMAT(' GENACC:I, KPART, PXDDT(KPART,JQ) ',2I12/
!CC     1   (1X,3G24.16))
!
    9 CONTINUE
      IF(.NOT.LAJGP(K2)) GO TO 10
      KPART=KPART+1
!
   10 END DO
!
      NDELT = NDELT + NPATCH
    5 END DO
!
!CC      DO 777 KDAVE=1,NEQN
!CC      IF(LDEBUG) WRITE(6,11017)  KDAVE,(PXDDT(KDAVE,JQ),JQ=1,3)
!CC11017 FORMAT(' GENACC: KDAVE, PXDDT(KDAVE,JQ) ',I12/
!CC     1   (10X,3G24.16))
!CC777   CONTINUE
      RETURN
!
!--------------------------------------------------------
!     ....ERROR RETURN
!
!
 8000 CONTINUE
      WRITE(IOUT6,30000) K, K2, LAJGP(K), LAJGP(K2)
      STOP
!---------------------------------------------------------
!
30000 FORMAT(///' STOPPING IN SUBROUTINE GENAC3 '/                      &
     &          ' ADJUSTED PERIOD SWITCHES DO NOT MATCH FOR '/          &
     &  ' PERIODS ', I3,' AND ', I3,'  --  ',2L5///)
      END
