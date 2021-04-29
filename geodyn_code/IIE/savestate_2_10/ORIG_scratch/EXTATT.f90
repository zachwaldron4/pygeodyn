!$EXTATT
      SUBROUTINE EXTATT(MJDS,FSEC,BFNRM1,BFNRM2,BFNRM3,                 &
     &                  TDNRM1,TDNRM2,TDNRM3,CTHETA,                    &
     &                  NFACE,NMOVE,LFORCE,REAQAT,IEAPPP,               &
     &                  MEAMEM,MEAPLA,LINATO,                           &
     &                  IDSATS,VLOUVS,NSTLOV,ISLVID,TSLOUV,             &
     &                  SBFTOD,XTEMP,LSHAD,PLASRD,PLADRG,               &
     &                  SLFETM,SLFSRD,SLFDRG )
!*******************************************************************
!  ROUTINE NAME:   EXTATT   DATE: 06/29/93      PGMR: S.B. LUTHCKE
!
!   FUNCTION - APPLY EXTERNAL QUATERNION INFORMATION TO ROTATE BODY
!              FIXED PANEL NORMAL VECTORS AND MOVABLE PANEL NORMAL
!              VECTORS TO TOD UNIT VECTORS.  ALSO, ROTATE BODY FIXED
!              LOUVER ACCELERATIONS TO TOD.
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   MJDS     I      S    INTEGER EPHEMERIS SECONDS SINCE GEODYN REF.TIME
!   FSEC     I      S    FRACTIONAL REMAINING SECONDS
!   BFNRM1   A      I    SPACECRAFT FLAT PLATE UNIT NORMAL VECTORS IN
!                        SPACECRAFT BODY-FIXED SYSTEM (X COMP.)
!   BFNRM2   A      I    SPACECRAFT FLAT PLATE UNIT NORMAL VECTORS IN
!                        SPACECRAFT BODY-FIXED SYSTEM (Y COMP.)
!   BFNRM3   A      I    SPACECRAFT FLAT PLATE UNIT NORMAL VECTORS IN
!                        SPACECRAFT BODY-FIXED SYSTEM (Z COMP.)
!   TDNRM1   A      O    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (X COMP.)
!   TDNRM2   A      O    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (Y COMP.)
!   TDNRM3   A      O    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (Z COMP.)
!   CTHETA   A      O    COS OF ANGLE BETWEEN TOD PLATE NORMAL AND
!                        SATELLITE-SUN VECTOR
!   NFACE    S      I    NUMBER OF FLAT PLATES USED TO MODEL SATELLITE S
!   NMOVE    S      I    NUMBER OF MOVEABLE FLAT PLATES USED
!   LFORCE   S      I    = .TRUE. INDICATES THIS ROUTINE CALLED BY F
!   REAQAT   A      I    QUATERNION INFORMATION ARRAY
!   IEAPPP   A      I    PANEL QUATERNION POINTER ARRAY
!   MEAMEM   S      I    4*MEAMEM IS THE MAXIMUM DIMENSION OF REAQAT
!   MEAPLA   S      I    2*MEAPLA IS THE MAXIMUM DIMENSION OF IEAPPP
!   LINATO   S      I    =.TRUE. THEN INTERNAL ATTITUDE MODEL WAS CALLED
!   XTEMP    A      I    SATELLITE TOD COORDINATES
!   LSHAD    S      I    LOGICAL INDICATOR FOR SELF SHADOWING
!   PLASRD   A      O    INTERPOLATED PLATE AREA OF CURRENT SATELLITE
!                        FOR SOLAR RADIATION IN SELF SHADOWING MODEL
!   PLADRG   A      O    INTERPOLATED PLATE AREA OF CURRENT SATELLITE
!                        FOR DRAG IN SELF SHADOWING MODEL
!   SLFETM   A      I    START, STOP, AND INTERVAL ET TIME FOR SHADOWING
!   SLFSRD   A      I    INPUT DATA FROM EXTERNAL FILE FOR SOLAR RAD
!   SLFDRG   A      I    INPUT DATA FROM EXTERNAL FILE FOR DRAG
!
!***********************************************************************
!
! NOTES:
!            TOD = GEODYN TRUE OF DATE INERTIAL FRAME
!                  When CB is not the EARTH TOD is IAU vector of date.
!            SBF = SATELLITE BODY-FIXED FRAME
!            MVP = MOVEABLE PANEL FRAME
!
!            SBFTOD = ROTATION FROM SBF TO TOD FRAME
!            PMVTOD = MOVABLE PLATE TO TOD FRAME
!
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBDTRU/BDTRUE(7,999)
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CRM5OE/RM5OE(9)
      COMMON/EXATFI/ISBJ20,NUMPEA,IPMPEA,MJDSEA
      COMMON/EXATFR/FSSCEA,RINTEA
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/LSTRT/LSTART
      COMMON/MGSSBF/SBFMGS(3,3)
      COMMON/TOPEXA/BETAP,PRVBET,SOMEGA,PRVOMG,YAWANG,SGAMMA,           &
     &              BETAMX,TDLOUV(8,3)
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/SLFSHD/NSHSAT,IDSASH(200),NSPANL(200),IFLGSH(200),         &
     &              IATYSH(200),NSHRCD(200),MNSHPL,NXSFSD
!
      DIMENSION BFNRM1(NFACE),BFNRM2(NFACE),BFNRM3(NFACE)
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION REAQAT(MEAMEM,4),                                       &
     &          IEAPPP(MEAPLA,4),CTHETA(NFACE),QSBF(4),Q(4),            &
     &          SBFTOD(3,3),PMVTOD(3,3),ROT(3,3),UNTSUN(3)
      DIMENSION ROTLK3(3,3),ROTLK2(3,3)
!      DIMENSION TODSBF(3,3)

      DIMENSION Q0(4),Q1(4)
      DIMENSION VLOUVS(3,1,NISLV)
      DIMENSION NSTLOV(NISLV),ISLVID(NISLV)
      DIMENSION TSLOUV(NSTLVS,3)
      DIMENSION XTEMP(6)
      DIMENSION PLASRD(1),PLADRG(1),SLFETM(1)
      DIMENSION SLFSRD(1),SLFDRG(1)
!
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,C1DM1/1.0D-1/
      DATA C1D6/1.0D6/,C1D2/100.0D0/,C1DM3/1.0D-3/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!
! COMPUTATIONS FOR TELEM FILE INFORMATION
!
      IF(LTXPRT .AND. .NOT. LSTART) THEN
!....INTEGRATION STEP TIME
         CALL YMDHMS(MJDS,FSEC,IYMD,IHM,SEC,1)
         TIMEO = IYMD*C1D6 + IHM*C1D2 + SEC
      ENDIF
!
! COMPUTE SBF TO J2000 ROTATION MATRIX
!
! debug extatt
!      print *,'extatt: sbf reaqat: ',reaqat(isbj20,1),reaqat(isbj20,2),
!     .         reaqat(isbj20,3),reaqat(isbj20,4)
! debug extatt
! ... compute time difference between current time and start of quaterni
      MJDSDF=MJDS-MJDSEA
      FSECDF=FSEC-FSSCEA
      EADIFF=DBLE(MJDSDF)+FSECDF
      IF(EADIFF.LT.ZERO) GOTO 9100
      D1=EADIFF-RINTEA
      IF (D1.LT.0.D0) EADIFF=RINTEA
! ... compute pointer to quaternions preceding the current time
      IPNT1=INT(EADIFF/RINTEA)+1
      IPNTM1=IPNT1-1
! .... (no minus 1 in index to reaqat below because a header is
! .... stored at isbj20)
      IQTPT1=ISBJ20+IPNT1
      IQTPT2=IQTPT1+1
! debug extatt
!      print *,'extatt: mjds,mjdsea,mjdsdf: ',mjds,mjdsea,mjdsdf
!      print *,'extatt: fsec,fsscea,fsecdf: ',fsec,fsscea,fsecdf
!      print *,'extatt: eadiff,rintea,ipnt1: ',eadiff,rintea,ipnt1
!      print *,'extatt: reaqat(iqtpt1,4): ',reaqat(iqtpt1,4)
! debug extatt
! ... test to determine if either of the boundary quaternions is marked
! .... -9999999
      TMP1=REAQAT(IQTPT1,1) - C1DM1
      ITMP1=TMP1
      TMP2=REAQAT(IQTPT2,1) - C1DM1
      ITMP2=TMP2
! debug extatt
!      print *,'extatt: tmp1,itmp1,tmp2,itmp2: ',tmp1,itmp1,tmp2,itmp2
! debug extatt
      IF((ITMP1.NE.-9999999).AND.(ITMP2.NE.-9999999)) THEN
! ... compute time of preceding boundary quaternion referenced to the st
! .... time of the quaternions of this set
       TIMPRE=DBLE(IPNTM1)*RINTEA
       TIMDIF=EADIFF-TIMPRE
! debug extatt
!      print *,'extatt: ipntm1,rintea,timpre: ',ipntm1,rintea,timpre
!      print *,'extatt: timdif: ',timdif
! debug extatt
       IF((TIMDIF.LT.ZERO).OR.(TIMDIF.GT.RINTEA)) GOTO 9300
! ... compute interpolated SBF to J2000 quaternions at the requested tim
! .... J2000 or whatever the basis for the input GEODYN ephemeris file
! .... (could be B1950)
       DT=TIMDIF/RINTEA
       DO 100 I=1,4
         Q0(I)=REAQAT(IQTPT1,I)
         Q1(I)=REAQAT(IQTPT2,I)
!!!!       QSBF(I)=(REAQAT(IQTPT2,I)-REAQAT(IQTPT1,I))*DT +                 &
!!!!     &        REAQAT(IQTPT1,I)
  100  CONTINUE
       CALL SLERP(DT,Q0,Q1,QSBF)
! test to see if quaternion magnitude is close to zero
       QATMAG=QSBF(1)**2+QSBF(2)**2+QSBF(3)**2+QSBF(4)**2
       QDIFF=ABS(ONE-QATMAG)
       IF(QDIFF.GT.C1DM3) THEN
        WRITE(6,97000) REAQAT(IQTPT1,1),REAQAT(IQTPT1,2),               &
     &                 REAQAT(IQTPT1,3),REAQAT(IQTPT1,4),               &
     &                 REAQAT(IQTPT2,1),REAQAT(IQTPT2,2),               &
     &                 REAQAT(IQTPT2,3),REAQAT(IQTPT2,4),               &
     &                 QSBF(1),QSBF(2),QSBF(3),QSBF(4)
       ENDIF
!
! .... normalize the interpolated quaternion
       QNORM=SQRT(QATMAG)
       DO 101 I=1,4
       QSBF(I)=QSBF(I)/QNORM
  101  CONTINUE
!
       CALL QATROT(QSBF,ROT)
! .... compute rotation from SBF to TOD by multiplying SBF to J2000 by
! .... rotation J2000 to TOD.  If central body other than Earth then TOD
! .... IAU vector of date.
       DO 110 I=1,3
       SBFTOD(1,I) = RM5OE(1)*ROT(1,I) +                                &
     &               RM5OE(4)*ROT(2,I) +                                &
     &               RM5OE(7)*ROT(3,I)
       SBFTOD(2,I) = RM5OE(2)*ROT(1,I) +                                &
     &               RM5OE(5)*ROT(2,I) +                                &
     &               RM5OE(8)*ROT(3,I)
       SBFTOD(3,I) = RM5OE(3)*ROT(1,I) +                                &
     &               RM5OE(6)*ROT(2,I) +                                &
     &               RM5OE(9)*ROT(3,I)
  110  CONTINUE

!       TODSBF = transpose( SBFTOD )

      ELSE

       IF(.NOT.LINATO) GOTO 9400
       IF(LTXPRT .AND. .NOT. LSTART) THEN
!!!8/06   WRITE(97) TIMEO,ZERO,ZERO,ZERO,ZERO
!!!       add 4 more zeroes to make this record same length as
!!!       the record with external attitude info.
         WRITE(97) TIMEO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO
       ENDIF
!      SBFTOD(1,1)=SBFMGS(1,1)
!      SBFTOD(2,1)=SBFMGS(2,1)
!      SBFTOD(3,1)=SBFMGS(3,1)
!      SBFTOD(1,2)=SBFMGS(1,2)
!      SBFTOD(2,2)=SBFMGS(2,2)
!      SBFTOD(3,2)=SBFMGS(3,2)
!      SBFTOD(1,3)=SBFMGS(1,3)
!      SBFTOD(2,3)=SBFMGS(2,3)
!      SBFTOD(3,3)=SBFMGS(3,3)
       RETURN
      ENDIF
!
! IF EXTERNAL ATTITUDE SBF TO TOD ROTATION EXISTS THEN ROTATE SBF UNIT N
! VECTORS (NON-MOVING PLATES) TO TOD FRAME
!
! debug extatt
!       print *,'extatt: rotating sbf bfnorm'
! debug extatt
       DO 120 I=1,NFACE-NMOVE
         TDNRM1(I) =                 SBFTOD(1,1)*BFNRM1(I) +            &
     &                               SBFTOD(1,2)*BFNRM2(I) +            &
     &                               SBFTOD(1,3)*BFNRM3(I)
         TDNRM2(I) =                 SBFTOD(2,1)*BFNRM1(I) +            &
     &                               SBFTOD(2,2)*BFNRM2(I) +            &
     &                               SBFTOD(2,3)*BFNRM3(I)
         TDNRM3(I) =                 SBFTOD(3,1)*BFNRM1(I) +            &
     &                               SBFTOD(3,2)*BFNRM2(I) +            &
     &                               SBFTOD(3,3)*BFNRM3(I)
  120  CONTINUE
!
! IF MOVEABLE PLATES EXIST COMPUTE THEIR ROTATIONS
!
! debug extatt
!       print *,'extatt: nmove is: ',nmove
! debug extatt
      IF(NMOVE.GT.0) THEN
!
      DO 140 IPLA=NFACE-NMOVE+1,NFACE
!
! COMPUTE POINTERS TO THIS MOVABLE PLATE'S QUATERNIONS
!
       DO 125 I=1,NUMPEA
       ITPNDX=IPMPEA+I-1
       IMVPSB=IEAPPP(ITPNDX,2)
       IF(IEAPPP(ITPNDX,1).EQ.IPLA) GOTO 126
! debug extatt
!       print *,'extatt: ipla,ieappp,itpndx: ',ipla,
!     .         ieappp(itpndx,1),itpndx
! debug extatt
  125  CONTINUE
!
! .... NO MATCH FOR MOVABLE PLATE IN EXTERNAL QUATERNION FILE.  THEREFOR
! .... USE INTERNAL MODEL.  IF INTERNAL MODEL NOT SELECTED TERMINATE.
!
       IF(LINATO) GOTO 140
       GOTO 9500
  126  CONTINUE
! debug extatt
!      print *,'extatt: mvp,ipla, reaqat: ',ipla,reaqat(imvpsb,1),
!     .         reaqat(imvpsb,2),
!     .         reaqat(imvpsb,3),reaqat(imvpsb,4)
! debug extatt
!
! COMPUTE POINTERS TO PROPER QUATERNIONS AND THEN COMPUTE ROTATION MATRI
!
! .... MOVABLE PLATES AND ANTENNA FOR A PARTICULAR SET HAVE THE SAME INT
! .... AND START AND STOP TIMES AS THE SBF TO J2000 QUATERNIONS.  THEREF
! .... THE VARIOUS QUANTITIES COMPUTED ABOVE MAY BE USED HERE!..........
! .... ALSO, -9999999 INTERNAL ATTITUDE OVERRIDE MAY NOT BE USED FOR MOV
! .... PLATE QUATERNIONS.
!
! check if link3 quternions exist.
! ieappp(itpndx,3)=0 indicates no link3 quaternions.
! the rotation matrix is saved into rotlk3
      IF(IEAPPP(ITPNDX,3).EQ.IPLA)  THEN
        IQTPT1=IEAPPP(ITPNDX,4)+IPNT1
        IQTPT2=IQTPT1+1
! ... test to determine if either of the boundary quaternions is marked
! ... -9999999
        TMP1=REAQAT(IQTPT1,1) - C1DM1
        ITMP1=TMP1
        TMP2=REAQAT(IQTPT2,1) - C1DM1
        ITMP2=TMP2
        IF((ITMP1.EQ.-9999999).OR.(ITMP2.EQ.-9999999)) THEN
          IF(LINATO) GOTO 140
          GOTO 9600
        ENDIF
        DO I=1,4
        Q0(I)=REAQAT(IQTPT1,I)
        Q1(I)=REAQAT(IQTPT2,I)
!!!!    Q(I)=(REAQAT(IQTPT2,I)-REAQAT(IQTPT1,I))*DT +                  &
!!!! &        REAQAT(IQTPT1,I)
        ENDDO
        CALL SLERP(DT,Q0,Q1,Q)
! test to see if quaternion magnitude is close to zero
        QATMAG=Q(1)**2+Q(2)**2+Q(3)**2+Q(4)**2
        QDIFF=ABS(ONE-QATMAG)
        IF(QDIFF.GT.C1DM3) THEN
        WRITE(6,98000) REAQAT(IQTPT1,1),REAQAT(IQTPT1,2),              &
     &                 REAQAT(IQTPT1,3),REAQAT(IQTPT1,4),              &
     &                 REAQAT(IQTPT2,1),REAQAT(IQTPT2,2),              &
     &                 REAQAT(IQTPT2,3),REAQAT(IQTPT2,4),              &
     &                 Q(1),Q(2),Q(3),Q(4)
        ENDIF
!
! .... normalize the interpolated quaternion
        QNORM=SQRT(QATMAG)
        DO I=1,4
        Q(I)=Q(I)/QNORM
        ENDDO
!
        CALL QATROT(Q,ROTLK3)
      ENDIF
!
      IQTPT1=IMVPSB+IPNT1
      IQTPT2=IQTPT1+1
! ... test to determine if either of the boundary quaternions is marked
! .... -9999999
      TMP1=REAQAT(IQTPT1,1) - C1DM1
      ITMP1=TMP1
      TMP2=REAQAT(IQTPT2,1) - C1DM1
      ITMP2=TMP2
      IF((ITMP1.EQ.-9999999).AND.(ITMP2.EQ.-9999999)) THEN
       IF(LINATO) GOTO 140
       GOTO 9600
      ENDIF
!
       DO 127 I=1,4
        Q0(I)=REAQAT(IQTPT1,I)
        Q1(I)=REAQAT(IQTPT2,I)
!!!!    Q(I)=(REAQAT(IQTPT2,I)-REAQAT(IQTPT1,I))*DT +                  &
!!!! &        REAQAT(IQTPT1,I)
  127  CONTINUE
       CALL SLERP(DT,Q0,Q1,Q)
! test to see if quaternion magnitude is close to zero
       QATMAG=Q(1)**2+Q(2)**2+Q(3)**2+Q(4)**2
       QDIFF=ABS(ONE-QATMAG)
       IF(QDIFF.GT.C1DM3) THEN
        WRITE(6,98000) REAQAT(IQTPT1,1),REAQAT(IQTPT1,2),               &
     &                 REAQAT(IQTPT1,3),REAQAT(IQTPT1,4),               &
     &                 REAQAT(IQTPT2,1),REAQAT(IQTPT2,2),               &
     &                 REAQAT(IQTPT2,3),REAQAT(IQTPT2,4),               &
     &                 Q(1),Q(2),Q(3),Q(4)
       ENDIF
!
! .... normalize the interpolated quaternion
       QNORM=SQRT(QATMAG)
       DO 128 I=1,4
       Q(I)=Q(I)/QNORM
  128  CONTINUE
!
       CALL QATROT(Q,ROT)
!
! Multiply the rotation matrixes for link3 and link2.
! Link3 rotation should be the first rotation to be applied.
! Do this only if ieappp(itpndx,3) != 0.
      IF(IEAPPP(ITPNDX,3).EQ.IPLA)  THEN
! save lik2 rotation matrix first
        DO I=1,3
          ROTLK2(I,1)=ROT(I,1)
          ROTLK2(I,2)=ROT(I,2)
          ROTLK2(I,3)=ROT(I,3)
        ENDDO
        DO I=1,3
       ROT(I,1) =               ROTLK2(I,1)*ROTLK3(1,1)                 &
     &                        + ROTLK2(I,2)*ROTLK3(2,1)                 &
     &                        + ROTLK2(I,3)*ROTLK3(3,1)
       ROT(I,2) =               ROTLK2(I,1)*ROTLK3(1,2)                 &
     &                        + ROTLK2(I,2)*ROTLK3(2,2)                 &
     &                        + ROTLK2(I,3)*ROTLK3(3,2)
       ROT(I,3) =               ROTLK2(I,1)*ROTLK3(1,3)                 &
     &                        + ROTLK2(I,2)*ROTLK3(2,3)                 &
     &                        + ROTLK2(I,3)*ROTLK3(3,3)
        ENDDO
      ENDIF
!
! COMPUTE ROTATION FROM MOVABLE PLATE TO TOD
!
! debug extatt
!       print *,'extatt: computing mvp rotation ipla: ',ipla
! debug extatt
       DO 130 I=1,3
       PMVTOD(I,1) =               SBFTOD(I,1)*ROT(1,1)                 &
     &                           + SBFTOD(I,2)*ROT(2,1)                 &
     &                           + SBFTOD(I,3)*ROT(3,1)
       PMVTOD(I,2) =               SBFTOD(I,1)*ROT(1,2)                 &
     &                           + SBFTOD(I,2)*ROT(2,2)                 &
     &                           + SBFTOD(I,3)*ROT(3,2)
       PMVTOD(I,3) =               SBFTOD(I,1)*ROT(1,3)                 &
     &                           + SBFTOD(I,2)*ROT(2,3)                 &
     &                           + SBFTOD(I,3)*ROT(3,3)
  130  CONTINUE
!
! ROTATE MVP UNIT NORMAL VECTORS(MOVING PLATES) TO TOD FRAME
!
! debug extatt
!       print *,'extatt: computing mvp tdnorm ipla: ',ipla
! debug extatt
         TDNRM1(IPLA) =              PMVTOD(1,1)*BFNRM1(IPLA)           &
     &                             + PMVTOD(1,2)*BFNRM2(IPLA)           &
     &                             + PMVTOD(1,3)*BFNRM3(IPLA)
         TDNRM2(IPLA) =              PMVTOD(2,1)*BFNRM1(IPLA)           &
     &                             + PMVTOD(2,2)*BFNRM2(IPLA)           &
     &                             + PMVTOD(2,3)*BFNRM3(IPLA)
         TDNRM3(IPLA) =              PMVTOD(3,1)*BFNRM1(IPLA)           &
     &                             + PMVTOD(3,2)*BFNRM2(IPLA)           &
     &                             + PMVTOD(3,3)*BFNRM3(IPLA)
  140  CONTINUE
      ENDIF
!
! COMPUTE UNIT NORMAL VECTORS IN TOD FRAME
!
! debug extatt
!       print *,'extatt: computing tdnorm unit vector'
! debug extatt
      DO 150 I=1,NFACE
         RSUM = SQRT(TDNRM1(I)**2+TDNRM2(I)**2+TDNRM3(I)**2)
         TDNRM1(I) = TDNRM1(I)/RSUM
         TDNRM2(I) = TDNRM2(I)/RSUM
         TDNRM3(I) = TDNRM3(I)/RSUM
  150 END DO
!
! COMPUTE TOD CENTRAL BODY TO SUN UNIT VECTOR
!
      UNTSUN(1)=(BDTRUE(1,8)-XTEMP(1))
      UNTSUN(2)=(BDTRUE(2,8)-XTEMP(2))
      UNTSUN(3)=(BDTRUE(3,8)-XTEMP(3))
      SUNMAG = SQRT(UNTSUN(1)**2+UNTSUN(2)**2+UNTSUN(3)**2)
      UNTSUN(1)=UNTSUN(1)/SUNMAG
      UNTSUN(2)=UNTSUN(2)/SUNMAG
      UNTSUN(3)=UNTSUN(3)/SUNMAG
!      IF(ICBDGM.EQ.11) GO TO 151
!      UNTSUN(1)=BDTRUE(1,8)/SUNMAG
!      UNTSUN(2)=BDTRUE(2,8)/SUNMAG
!      UNTSUN(3)=BDTRUE(3,8)/SUNMAG
!      GOTO 152
151   CONTINUE
!      UNTSUN(1)=(BDTRUE(1,8)-XTEMP(1))/SUNMAG
!      UNTSUN(2)=(BDTRUE(2,8)-XTEMP(2))/SUNMAG
!      UNTSUN(3)=(BDTRUE(3,8)-XTEMP(3))/SUNMAG
152   CONTINUE
!
! COMPUTE COSINE OF ANGLE BETWEEN TDNRM# AND UNTSUN
!
      DO 160 I=1,NFACE
        CTHETA(I)=TDNRM1(I)*UNTSUN(1)+TDNRM2(I)*UNTSUN(2)+              &
     &            TDNRM3(I)*UNTSUN(3)
  160 END DO
!
! ROTATE BODY-FIXED LOUVER ACCELERATION VECTOR IF NECESSARY
!     IF(RNTPLV .GT. ZERO) THEN
      CAll FNDNUM(IDSATS,ISLVID,NISLV ,IRET)
!     IF(IRET.LE.0)GOTO 999
      IF(IRET.LE.0)GOTO 1000
       KST=NSTLOV(IRET)
      IF(KST .GT. ZERO) THEN
       DO 170 INLV=1,KST
         TSLOUV(INLV,1) =        SBFTOD(1,1)*VLOUVS(1,INLV,IRET)        &
     &                         + SBFTOD(1,2)*VLOUVS(2,INLV,IRET)        &
     &                         + SBFTOD(1,3)*VLOUVS(3,INLV,IRET)
         TSLOUV(INLV,2) =        SBFTOD(2,1)*VLOUVS(1,INLV,IRET)        &
     &                         + SBFTOD(2,2)*VLOUVS(2,INLV,IRET)        &
     &                         + SBFTOD(2,3)*VLOUVS(3,INLV,IRET)
         TSLOUV(INLV,3) =        SBFTOD(3,1)*VLOUVS(1,INLV,IRET)        &
     &                         + SBFTOD(3,2)*VLOUVS(2,INLV,IRET)        &
     &                         + SBFTOD(3,3)*VLOUVS(3,INLV,IRET)
!        WRITE(6,*)'EXTATT*VLOUVS=',(VLOUVS(K,INLV,IRET),K=1,3)
!        WRITE(6,*)'EXTATT*TSLOUV=',(TSLOUV(INLV,K),K=1,3)
         RSUM = SQRT(TSLOUV(INLV,1)**2+TSLOUV(INLV,2)**2+               &
     &               TSLOUV(INLV,3)**2)
         TSLOUV(INLV,1) = TSLOUV(INLV,1)/RSUM
         TSLOUV(INLV,2) = TSLOUV(INLV,2)/RSUM
         TSLOUV(INLV,3) = TSLOUV(INLV,3)/RSUM
  170  CONTINUE
      ENDIF
 1000 CONTINUE
! SELF SHADOWING MODEL
      IF(LSHAD) THEN
! CHECK IF THE CURRENT SATELLITE IS REQUIRED FOR SLEF SHADOWING
        CALL FNDNUM(IDSATS,IDSASH,NSHSAT,IRET)
! DO NOTHING IF THE SATELLITE ID IS NOT FOUND FROM THE LIST
        IF(IRET.EQ.0) GOTO 2000
! COMPUTE THE CURRENT ET TIME
        SSDTIM=DBLE(MJDS)+FSEC
! CHECK IF THE TIME IS IN THE RANGE
! ITOFF IS THE POINTER OFFSET OF START TIME FOR SATELLITE IRET
! SLFETM(1+ITOFF) = START TIME
! SLFETM(2+ITOFF) = STOP  TIME
! SLFETM(3+ITOFF) = TIME STEP
        ITOFF=(IRET-1)*3
        IF(SSDTIM.LT.SLFETM(1+ITOFF) .OR.                       &
     &     SSDTIM.GT.SLFETM(2+ITOFF)) THEN
          WRITE(6,*) ' problem of coverage in shadowing file !'
          WRITE(6,*) ' start/stop time is: ',SLFETM(1+ITOFF),   &
     &       SLFETM(2+ITOFF)
          WRITE(6,*) ' but requested time is : ',SSDTIM
          STOP
        ENDIF
! NSHRCD(I) IS THE NUMBER OF DATA RECORD FOR THE SATELLITE I
! NSPANL(I) IS THE NUMBER OF PANELS FOR SATELLITE I
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! How the self shadowing data are stored in memory.
! All the data are stored in a one-dimensional array.
! For example, SLFSRD(1), SLFSRD(2), ..., SLFSRD(NSHRCD(1)) are
! the cross-section for the first plane of the first satellite.
! SLFSRD(NSHRCD(1)+1), SLFSRD(NSHRCD(1)+2), ..., SLFSRD(2*NSHRCD(1))
! are the cross-section for the second plane of the first satellite,etc
! The data for the second satellite are stored in the same way right
! after the first one.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Find the pointer for solar radiation and drag data for this satellite.
        JSLFSR=0
        JSLFDG=0
        DO I=1,IRET-1
          IF(IFLGSH(I).EQ.10 .OR. IFLGSH(I).EQ.11) THEN
            JSLFSR=JSLFSR+NSHRCD(I)*NSPANL(I)
          ENDIF
          IF(IFLGSH(I).EQ.1  .OR. IFLGSH(I).EQ.11) THEN
            JSLFDG=JSLFDG+NSHRCD(I)*NSPANL(I)
          ENDIF
        ENDDO
! COMPUTE THE POINTER FOR LINEAR INTERPOLATION
        IPTR=INT((SSDTIM-SLFETM(1+ITOFF))/SLFETM(3+ITOFF))+1
        SLFDLT=SSDTIM-SLFETM(1+ITOFF)-(IPTR-1)*SLFETM(3+ITOFF)
        SLFRTO=SLFDLT/SLFETM(3+ITOFF)
! LINEAR INTERPOLATION FOR SOLAR RADIATION
        IF(IFLGSH(IRET).EQ.10 .OR. IFLGSH(IRET).EQ.11) THEN
           JSLFSR=JSLFSR+IPTR
           DO I=1,NSPANL(IRET)
            PLASRD(I)=SLFSRD(JSLFSR)+(SLFSRD(JSLFSR+1)-SLFSRD(JSLFSR)) &
     &                            *SLFRTO
! UPDATE THE POINTER FOR NEXT PANEL
            JSLFSR=JSLFSR+NSHRCD(IRET)
           ENDDO
        ENDIF
! LINEAR INTERPOLATION FOR DRAG
        IF(IFLGSH(IRET).EQ.1  .OR. IFLGSH(IRET).EQ.11) THEN
           JSLFDG=JSLFDG+IPTR
           DO I=1,NSPANL(IRET)
            PLADRG(I)=SLFDRG(JSLFDG)+(SLFDRG(JSLFDG+1)-SLFDRG(JSLFDG)) &
     &                            *SLFRTO
! UPDATE THE POINTER FOR NEXT PANEL
            JSLFDG=JSLFDG+NSHRCD(IRET)
           ENDDO
        ENDIF
      ENDIF
 2000 CONTINUE
! END OF SELF SHADOWING INTERPOLATION
!
! OUTPUT EXTATT TELEM FILE INFORMATION
!
      IF(LTXPRT .AND. .NOT. LSTART) THEN
!
! Get SBF to TOD quaternion
!
       CALL ROTQAT(SBFTOD,Q)
! test to see if quaternion magnitude is close to zero
       QATMAG=Q(1)**2+Q(2)**2+Q(3)**2+Q(4)**2
       QDIFF=ABS(ONE-QATMAG)
       IF(QDIFF.GT.C1DM3) THEN
        WRITE(6,98100) Q(1),Q(2),Q(3),Q(4)
       ENDIF
!
! .... normalize the interpolated quaternion
       QNORM=SQRT(QATMAG)
       DO 2128 I=1,4
       Q(I)=Q(I)/QNORM
 2128  CONTINUE
!
         WRITE(97) TIMEO,QSBF(1),QSBF(2),QSBF(3),QSBF(4), &
     &             Q(1), Q(2), Q(3), Q(4)
      ENDIF
      RETURN
!      IF(IRET.LE.0) then
!999     WRITE(6,*)'SAT ID DOES NOT MATCH ANY ID in ISLVID ARRAY'
!        STOP
!      ELSE
!        RETURN
!      ENDIF
 9100 WRITE(6,91000)
      STOP 16
 9300 WRITE(6,93000)
      STOP 16
 9400 WRITE(6,94000)
      STOP 16
 9500 WRITE(6,95000)
      STOP 16
 9600 WRITE(6,94000)
      STOP 16
91000 FORMAT(1X,'ABNORMAL TERMINATION IN EXTATT, NEGATIVE TIME',        &
     &          'DIFFERENCE')
93000 FORMAT(1X,'ABNORMAL TERMINATION IN EXTATT, INTERPOLATION',        &
     &          'DIFFERENCE IS LESS THAN ZERO')
94000 FORMAT(1X,'ABNORMAL TERMINATION IN EXTATT, -9999999 DETECTED',    &
     &          'IN SBF QUATERNIONS',1X,/,                              &
     &           'BUT INTERNAL ATT MODEL NOT SELECTED')
95000 FORMAT(1X,'ABNORMAL TERMINATION IN EXTATT, NO MATCH ',            &
     &          'FOR MOVABLE PLATE',1X,/,                               &
     &          'IN QUATERNION FILE AND ',                              &
     &          'NO INTERNAL MODEL SELECTED')
96000 FORMAT(1X,'ABNORMAL TERMINATION IN EXTATT, -9999999 DETECTED',    &
     &          'IN MVP QUATERNIONS',1X,/,                              &
     &           'BUT INTERNAL ATT MODEL NOT SELECTED')
97000 FORMAT(1X,'WARNING FROM EXTATT: INTERPOLATED BODY-FIXED',         &
     &          ' QUATERNION MAGNITUDE',/,' IS DIFFERENT FROM ',        &
     &          ' ONE BY AT LEAST .001',/,' QUATERNION FOR ',           &
     &          ' INTERPOLATION BOUNDARY 1 IS: ',/,4D24.16,/,           &
     &          ' QUATERNION FOR INTERPOLATION BOUNDARY 2 IS: ',        &
     &          /,4D24.16,/,'THE INTERPOLATED QUATERNION IS: ',         &
     &          /,4D24.16,/,'THE QUATERNION WILL BE NORMALIZED')
98000 FORMAT(1X,'WARNING FROM EXTATT: INTERPOLATED MOVABLE PLATE',      &
     &          ' QUATERNION MAGNITUDE',/,' IS DIFFERENT FROM ',        &
     &          ' ONE BY AT LEAST .001',/,' QUATERNION FOR ',           &
     &          ' INTERPOLATION BOUNDARY 1 IS: ',/,4D24.16,/,           &
     &          ' QUATERNION FOR INTERPOLATION BOUNDARY 2 IS: ',        &
     &          /,4D24.16,/,'THE INTERPOLATED QUATERNION IS: ',         &
     &          /,4D24.16,/,'THE QUATERNION WILL BE NORMALIZED')
98100 FORMAT(1X,'WARNING FROM EXTATT: SBFTOD QUAT FOR TELEM FILE',      &
     &          ' QUATERNION MAGNITUDE',/,' IS DIFFERENT FROM ',        &
     &          ' ONE BY AT LEAST .001',/,' QUATERNION IS:',            &
     &          /,4D24.16,/ )
      END
