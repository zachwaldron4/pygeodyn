!$BCON
      SUBROUTINE BCON(PARMVC,PARMSG)
!********1*********2*********3*********4*********5*********6*********7**
!  BCON            00/00/00         0000.0      PGMR - ?
!
! FUNCTION: OUTPUTS MBIAS CARD INFORMATION TO UNIT 37 AT END OF RUN.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PARMVC   I    A    CURRENT VALUES OF ADJUSTED PARAMETERS
!   PARMSG   I    A    ADJUSTED PARAMETER VARIANCE ARRAY
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      CHARACTER*60 CARD1,CARD2,CARD2A,CARD2B,CARD2C,CARD2D,CARD3
      CHARACTER*60 CARDB2
      CHARACTER*17 MBIASM
      CHARACTER*13 DATE1,DATE2
      CHARACTER*10 DIG
      CHARACTER*6 MBIAS,MBIAS2,MBIAS3
      COMMON/CARDB/MCARDB,NCARDB,ITCARD(3,28000),ISTABC(3,28000),       &
     &             ISATBC(3,28000),ICRDBF(28000)
      COMMON/CARDBV/VCARDB(28000),SCARDB(28000)
      COMMON/XPARMB/MPARMB,NPARMB,MPOBS,JPAMB,ISKIPE,                   &
     &             ITPARM(5,28000),ISTABP(3,28000),                     &
     &             ISATBP(3,28000),IPRMBF(28000)
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
      DIMENSION PARMVC(1),PARMSG(1)
      DIMENSION FSEC(1),SEC(1),IHM(1),IYMD(1)
      DATA DIG/'1234567890'/
      DATA DATE1/'            .'/
      DATA DATE2/'            .'/
      DATA MBIAS/'MBIAS '/
      DATA MBIAS2/'MBIAS2'/
      DATA MBIAS3/'MBIAS3'/
      DATA MBIASM/'MBIASM         56'/
!
      IF(NPARMB.LE.0.AND.NCARDB.LE.0) RETURN
      NCC=0
!
      IF(NCARDB.GT.0.AND.NPARMB.GT.0) THEN
         DO 200 I=1,NCARDB
         DO 100 J=1,NPARMB
         IF(IPRMBF(J).GT.0) GO TO 100
         ITPC=ITCARD(1,I)
         IF(ITPC.GT.900) ITPC=900
         ITPP=ITPARM(1,J)
         IF(ITPP.GT.900) ITPP=900
         IF(ITPC.NE.ITPP) GO TO 100
         ITC1=ITCARD(2,I)-100
         IF(ITPARM(2,J).LT.ITC1) GO TO 100
         ITC2=ITCARD(3,I)+100
         IF(ITPARM(3,J).GT.ITC2) GO TO 100
         DO 50 K=1,3
         IF(ISTABC(K,I).NE.ISTABP(K,J)) GO TO 100
         IF(ISATBC(K,I).NE.ISATBP(K,J)) GO TO 100
   50    CONTINUE
         NCC=NCC+1
         ICRDBF(I)=J
         IPRMBF(J)=I
         VCARDB(I)=PARMVC(IPVAL0(IXBISA)-1+J)
         GO TO 200
  100    CONTINUE
  200    CONTINUE
      ENDIF
      NCU=NCARDB-NCC
      NPU=NPARMB-NCC
      NCU=MAX(NCU,0)
      NPU=MAX(NPU,0)
!
      NPNCD=0
      NPNCU=0
!
      IF(NPU.GT.0) THEN
         DO 300 J=1,NPARMB
         IF(IPRMBF(J).EQ.-98) NPNCD=NPNCD+1
         IF(IPRMBF(J).EQ.-99) NPNCU=NPNCU+1
  300    CONTINUE
      ENDIF
!
!
      CARD1(1:60)=                                                      &
     &'******  THE FOLLOWING INPUT BIAS CARDS WERE PARAMETERS  ****'
      CARD2(1:60)=                                                      &
     &'** THE FOLLOWING BIAS CARDS WERE PARAMETERS BUT CAN NOT BE  '
      CARD2A(1:60)=                                                     &
     &'  LINKED TO ANY ONE INPUT CARD      ************************'
      CARD2B(1:60)=                                                     &
     &'  IN ADDITION TO THAT THERE ARE      BIAS PARAMETERS THAT   '
      CARDB2(1:60)=                                                     &
     &' ***** WARNING:       THERE ARE      BIAS PARAMETERS THAT   '
      CARD2C(1:60)=                                                     &
     &'  HAVE NOT BEEN LINKED TO DATA BLOCKS THESE WILL NOT BE     '
      CARD2D(1:60)=                                                     &
     &'  PUNCHED ************************************************* '
      CARD3(1:60)=                                                      &
     &'**** THE FOLLOWING INPUT BIAS CARDS WERE NOT PARAMETERS ****'
      ITQ=MOD(NPNCU,10000)
      IT=ITQ/1000
      IF(IT.EQ.0) IT=10
      CARD2B(33:33)=DIG(IT:IT)
      ITQ=MOD(NPNCU,1000)
      IT=ITQ/100
      IF(IT.EQ.0) IT=10
      CARD2B(34:34)=DIG(IT:IT)
      ITQ=MOD(NPNCU,100)
      IT=ITQ/10
      IF(IT.EQ.0) IT=10
      CARD2B(35:35)=DIG(IT:IT)
      ITQ=MOD(NPNCU,10)
      IT=ITQ/1
      IF(IT.EQ.0) IT=10
      CARD2B(36:36)=DIG(IT:IT)
      CARDB2(33:36)=CARD2B(33:36)
!
! CARDS FOR WHICH THERE ARE PARMS
!
      IF(NCC.GT.0) THEN
         WRITE(37,7000) CARD1
         DO 400 I=1,NCARDB
         IF(ICRDBF(I).GT.0) THEN
            FSEC(1)=0.D0
            CALL YMDHMS(ITCARD(2,I),FSEC,IYMD,IHM,SEC,1)
            IYMD1=IYMD(1)
            IHM1=IHM(1)
            IS=SEC(1)
            IHM1=100*IHM1+IS
            if (IYMD1.gt. 1000000)IYMD1= IYMD1 - 1000000
!           CALL NUMASC(IYMD1,DATE1,16,.TRUE.)
!           CALL NUMASC(IHM1,DATE1(7:12),16,.TRUE.)
            write (DATE1(1:6),'(i6.6)')IYMD1
            write (DATE1(7:12),'(i6.6)')IHM1
            FSEC(1)=0.D0
            CALL YMDHMS(ITCARD(3,I),FSEC,IYMD,IHM,SEC,1)
            IYMD1=IYMD(1)
            IHM1=IHM(1)
            IS=SEC(1)
            IHM1=100*IHM1+IS
             if (IYMD1.gt. 1000000)IYMD1= IYMD1 - 1000000
!           CALL NUMASC(IYMD1,DATE2,16,.TRUE.)
!         CALL NUMASC(IHM1,DATE2(7:12),16,.TRUE.)
            write (DATE2(1:6),'(i6.6)')IYMD1
            write (DATE2(7:12),'(i6.6)')IHM1
            WRITE(37,81680) MBIAS,ISTABC(1,I),ITCARD(1,I),ISATBC(1,I),  &
     &                     VCARDB(I),DATE1,DATE2,SCARDB(I)
            IF(ITCARD(1,I).LT.99.AND.ITCARD(1,I).GE.57) THEN
              WRITE(37,81681) MBIAS2,ISTABC(2,I),ISATBC(2,I)
            ENDIF
            IF(ITCARD(1,I).EQ.156) THEN
              WRITE(37,81681) MBIAS2,ISTABC(2,I),ISATBC(2,I)
            ENDIF
            IF(ITCARD(1,I).GE.700.AND.ITCARD(1,I).LE.703) THEN
              WRITE(37,81682) MBIASM
            ENDIF
            IF(ITCARD(1,I).EQ.85) THEN
              WRITE(37,81681) MBIAS3,ISTABC(3,I),ISATBC(3,I)
            ENDIF
            IF(ITCARD(1,I).EQ.57) THEN
              WRITE(37,81681) MBIAS3,ISTABC(3,I),ISATBC(3,I)
            ENDIF
            IF(ITCARD(1,I).EQ.59) THEN
              WRITE(37,81681) MBIAS3,ISTABC(3,I),ISATBC(3,I)
            ENDIF
          ENDIF
  400    CONTINUE
      ENDIF
!
! PARMS FOR WHICH THERE ARE NOT CARDS
!
      IF(NPNCD.LE.0) THEN
         IF(NPNCU.GT.0) THEN
            WRITE(37,7000) CARDB2
            WRITE(37,7000) CARD2C
            WRITE(37,7000) CARD2D
         ENDIF
      ELSE
         WRITE(37,7000) CARD2
         WRITE(37,7000) CARD2A
         IF(NPNCU.GT.0) THEN
            WRITE(37,7000) CARD2B
            WRITE(37,7000) CARD2C
            WRITE(37,7000) CARD2D
         ENDIF
        DO 500 I=1,NPARMB
         IF(IPRMBF(I).EQ.-98) THEN
            FSEC(1)=0.D0
            CALL YMDHMS(ITPARM(2,I),FSEC,IYMD,IHM,SEC,1)
            IYMD1=IYMD(1)
            IHM1=IHM(1)
            IS=SEC(1)
            IHM1=100*IHM1+IS
            if (IYMD1.gt. 1000000)IYMD1= IYMD1 - 1000000
!           CALL NUMASC(IYMD1,DATE1,16,.TRUE.)
!         CALL NUMASC(IHM1,DATE1(7:12),16,.TRUE.)
            write (DATE1(1:6),'(i6.6)')IYMD1
            write (DATE1(7:12),'(i6.6)')IHM1
            FSEC(1)=0.D0
            CALL YMDHMS(ITPARM(3,I),FSEC,IYMD,IHM,SEC,1)
            IYMD1=IYMD(1)
            IHM1=IHM(1)
            IS=SEC(1)
            IHM1=100*IHM1+IS
            if (IYMD1.gt. 1000000)IYMD1= IYMD1 - 1000000
!           CALL NUMASC(IYMD1,DATE2,16,.TRUE.)
!         CALL NUMASC(IHM1,DATE2(7:12),16,.TRUE.)
            write (DATE2(1:6),'(i6.6)')IYMD1
            write (DATE2(7:12),'(i6.6)')IHM1
            IPT=IPVAL0(IXBISA)-1+I
            SG=SQRT(1.D0/PARMSG(IPT))
            WRITE(37,81680) MBIAS,ISTABP(1,I),ITPARM(1,I),ISATBP(1,I),  &
     &                     PARMVC(IPT),DATE1,DATE2,SG
            IF(ITPARM(1,I).LT.99.AND.ITPARM(1,I).GE.57) THEN
               WRITE(37,81681) MBIAS2,ISTABP(2,I),ISATBP(2,I)
            ENDIF
            IF(ITPARM(1,I).EQ.156) THEN
               WRITE(37,81681) MBIAS2,ISTABP(2,I),ISATBP(2,I)
            ENDIF
            IF(ITPARM(1,I).GE.700.AND.ITPARM(1,I).LE.703) THEN
               WRITE(37,81682) MBIASM
            ENDIF
            IF(ITPARM(1,I).EQ.85) THEN
               WRITE(37,81681) MBIAS3,ISTABP(3,I),ISATBP(3,I)
            ENDIF
            IF(ITPARM(1,I).EQ.57) THEN
               WRITE(37,81681) MBIAS3,ISTABP(3,I),ISATBP(3,I)
            ENDIF
            IF(ITPARM(1,I).EQ.59) THEN
               WRITE(37,81681) MBIAS3,ISTABP(3,I),ISATBP(3,I)
            ENDIF
         ENDIF
  500    CONTINUE
      ENDIF
!
! CARDS FOR WHICH THERE ARE NOT PARMS
!
      IF(NCU.GT.0) THEN
         WRITE(37,7000) CARD3
         DO 600 I=1,NCARDB
         IF(ICRDBF(I).EQ.-99) THEN
            FSEC(1)=0.D0
            CALL YMDHMS(ITCARD(2,I),FSEC,IYMD,IHM,SEC,1)
            IYMD1=IYMD(1)
            IHM1=IHM(1)
            IS=SEC(1)
            IHM1=100*IHM1+IS
            if (IYMD1.gt. 1000000)IYMD1= IYMD1 - 1000000
!           CALL NUMASC(IYMD1,DATE1,16,.TRUE.)
!         CALL NUMASC(IHM1,DATE1(7:12),16,.TRUE.)
            write (DATE1(1:6),'(i6.6)')IYMD1
            write (DATE1(7:12),'(i6.6)')IHM1
            FSEC(1)=0.D0
            CALL YMDHMS(ITCARD(3,I),FSEC,IYMD,IHM,SEC,1)
            IYMD1=IYMD(1)
            IHM1=IHM(1)
            IS=SEC(1)
            IHM1=100*IHM1+IS
            if (IYMD1.gt. 1000000)IYMD1= IYMD1 - 1000000
!           CALL NUMASC(IYMD1,DATE2,16,.TRUE.)
!         CALL NUMASC(IHM1,DATE2(7:12),16,.TRUE.)
            write (DATE2(1:6),'(i6.6)')IYMD1
            write (DATE2(7:12),'(i6.6)')IHM1
            WRITE(37,81680) MBIAS,ISTABC(1,I),ITCARD(1,I),ISATBC(1,I),  &
     &                     VCARDB(I),DATE1,DATE2,SCARDB(I)
            IF(ITCARD(1,I).LT.99.AND.ITCARD(1,I).GE.57) THEN
               WRITE(37,81681) MBIAS2,ISTABC(2,I),ISATBC(2,I)
            ENDIF
           IF(ITCARD(1,I).EQ.156) THEN
               WRITE(37,81681) MBIAS2,ISTABC(2,I),ISATBC(2,I)
            ENDIF
           IF(ITCARD(1,I).GE.700.AND.ITCARD(1,I).LE.703) THEN
               WRITE(37,81682) MBIASM
            ENDIF
           IF(ITCARD(1,I).EQ.85) THEN
               WRITE(37,81681) MBIAS3,ISTABC(3,I),ISATBC(3,I)
            ENDIF
           IF(ITCARD(1,I).EQ.57) THEN
               WRITE(37,81681) MBIAS3,ISTABC(3,I),ISATBC(3,I)
            ENDIF
           IF(ITCARD(1,I).EQ.59) THEN
               WRITE(37,81681) MBIAS3,ISTABC(3,I),ISATBC(3,I)
            ENDIF
         ENDIF
  600    CONTINUE
      ENDIF
      RETURN
 7000 FORMAT(A60)
81680 FORMAT(A6,I8,I3,I7,D20.13,1X,A13,1X,A13,1PD8.1)
81681 FORMAT(A6,I8,3X,I7)
81682 FORMAT(A17)
      END
