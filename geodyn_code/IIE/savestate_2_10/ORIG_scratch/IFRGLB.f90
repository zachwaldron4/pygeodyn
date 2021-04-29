!$IFRGLB
      SUBROUTINE IFRGLB(AA)
!********1*********2*********3*********4*********5*********6*********7**
! IFRGLB           00/00/00            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  1. READ INTERFACE FILE (IFF) FOR DATE AND TIME OF
!               THE IIS RUN
!
!            2. READ IFF  FOR GEODYN SETUP DECK
!
!            3. READ IFF FOR THE BUFFER LENGTHS AND POINTERS THAT
!               IIE USES FOR READING THE REST OF THE INTERFACE
!               FILES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!
! COMMENTS:
!           EXTENDED VIRTUAL MEMORY IN IIE IS USED AS FOLLOWS:
!           INTEGER BUFFER FOR READING IFF HEADER
!           INTEGER BUFFER FOR READING IFF LENGTHS RECORDS
!           INTEGER BUFFER FOR READING MISC. IFF RECORDS.
!           REAL BUFFER FOR HOLDING DATA PRIOR TO CONVERSION TO
!           IIE FLOATING POINT REPRESENTATION
!           REAL BUFFER FOR ALL EPHEMERIS DATA
!           INTEGER BUFFER FOR ARC HEADER INFORMATION
!           REAL    BUFFER FOR ARC COMMON BLOCKS**********
!           INTEGER BUFFER FOR ARC COMMON BLOCKS         *
!           LOGICAL BUFFER FOR ARC COMMON BLOCKS    REPEATED PER
!           REAL    BUFFER FOR ARC DYNAMIC ARRAYS       ARC
!           INTEGER BUFFER FOR ARC DYNAMIC ARRAYS        *
!           LOGICAL BUFFER FOR ARC DYNAMIC ARRAYS        *
!           REAL    BUFFER FOR ARC OBS. DIRECTORY DATA ***
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      CHARACTER*11 IDG
      CHARACTER*8 ECP,ECP2,ECP3
      CHARACTER*6 XPSSHD
      CHARACTER*2 TEST2,BLANK2
      COMMON/CIFF  /IFHEAD,IFLNTH,IFBUFL,IFBUFR,KDYNHD,KDYNLN,          &
     &              KDYNIF,KDYNFF,NXCIFF
      COMMON/CLASCI/LASCII,LIFCYB,LARECL,NXLASC
      COMMON/COBBUF/IOBSBF,IBUF1 ,IBUF2 ,IWORD1,NHEADR,NWORDM
      COMMON/CUNIT9/L9SET ,L9RES ,L9RSST,L9RSTY,L9EBS ,L9APAR,L9SUMP,   &
     &              L9GPAR,L9UPDT,L9LAST,L9OUT ,NXUNT9
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DYNMEM/MAXDM
      COMMON/DYNPTR/KDEPHM,KDEPH2,KDAHDR(3,9),KDYNAP(3,9,2),KDNEXT(3),  &
     &NXDYNP
      COMMON/G2EINF/G2EVER,G2EDAT,G2ERTM
      COMMON/GPSSHD/LGPSHD,LGPSHO,LSHADS
      COMMON/GRAVO/IGRVOK
      COMMON/PARPRC/NPROCS,NPROCI(5,10)
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      CHARACTER*8      CARDS

      DIMENSION AA(1),CARDS(10),CARDXX(1),CARDX(1),VERDTM(6),CARD72(9)
      DIMENSION IFFHDR(20),IBUF(1),HEADER(10),TEST2(4),DLIST(7)
!
      CHARACTER*8      TEST8
      EQUIVALENCE(IBUF(1),IFHEAD)
      EQUIVALENCE(CARD72(1),CARDS(1)),(TEST2(1),TEST8)
      EQUIVALENCE(CARDS7,CARDS(7)),(CARDS8,CARDS(8))
      EQUIVALENCE(CARDS(10),ECP)
      EQUIVALENCE(CARDS(9),ECP2)
      EQUIVALENCE(CARDS(8),ECP3)
      EQUIVALENCE(CARDS(1),XPSSHD)
!
      DATA IBLOCK/1 /
      DATA C1D6/1.0D6/,C0/0.0D0/,HALF/0.5D0/
      CHARACTER*8      DNLIST, DLIST, BLANK
      DATA DNLIST/'NOLIST  '/,NLIST /7/,LIST/.TRUE./
      DATA DLIST /'LIST    ','STAPOS  ','ENDSTA  ','ENDGLB  ',          &
     &            'DATA    ','ENDARC  ','ENDALL  '/
      DATA BLANK/'        '/,BLANK2/'  '/

!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
! GET RUN DATE AND TIME FOR IIE
      CALL SYSTIM
      DATE  =G2ERTM/C1D6
      TIME  =MOD(G2ERTM,C1D6)
      L1CRD=.TRUE.
      LGPSHD=.FALSE.
      LGPSHO=.FALSE.
!**********************************************************************
! READ HEADER RECORD FOR REAL DATA
!**********************************************************************
      ITYPE=1
      CALL BINRD(IUNT11,IFFHDR,20)
! TEST BLOCK NUMBER AND RECORD TYPE
      IF(IFFHDR(1).NE.IBLOCK .OR. IFFHDR(3).NE.ITYPE) GO TO 60100
! SET NUMBER OF RECORDS
      NRECS=IFFHDR(4)
! SET RECORD LENGTH
      IRECL=IFFHDR(5)
!**********************************************************************
! READ REAL DATA RECORDS - (DATE/TIME/GEODYN II SETUP DECK)
!**********************************************************************
! READ DATE AND TIME OF RUN FOR IIS AND TDF, AND OTHER MISC INFO
!     WRITE(6,*) 'IFRGLB:90 THE VALUE OF LASCII: ', LASCII
      IF(LASCII) THEN
         READ(IUNT11,18000) HEADER,CARDXX
      ELSE
         READ(IUNT11) HEADER,CARDXX
      ENDIF
      CALL Q9ICLA(HEADER,VERDTM,6,ISTAT)
!      CALL Q9ICLA(HEADER(7),CARDS(7),4,ISTAT)
      CALL Q9ICLA(HEADER(7),CARDS7,4,ISTAT)
      CALL Q9ICLA(CARDXX,CARDX,1,ISTAT)
      L9SET=CARDS7.GT.C0
      NLINPG=CARDS8+HALF
      MLINE6=NLINPG
      MLINE9=NLINPG
      L6SET=CARDX(1).LE.C0


! READ GEODYN SETUP DECK AND PRINT OUTPUT ON UNIT 6

! list the input cards

      NRECS1=NRECS-1
      KOUNT=0
      IPAGE6=IPAGE6+1
      ILINE6=5
      WRITE(IOUT6,80200 ) G2EVER,DATE,TIME,IPAGE6,VERDTM
      WRITE(IUNT88,80200 ) G2EVER,DATE,TIME,IPAGE6,VERDTM
      IF(.NOT.L9SET) GO TO 1000
      IPAGE9=IPAGE9+1
      ILINE9=5
      WRITE(IOUT9,80209 ) G2EVER,DATE,TIME,IPAGE9,VERDTM
 1000 CONTINUE
      IF(LASCII) THEN
         READ(IUNT11,17000) CARDS
      ELSE
         READ(IUNT11) CARDS
      ENDIF
      IF(L1CRD) THEN
        FACTPR=1.15D0
        IGRVOK=-99
        IDG=' 1234567890'
        DO 1002 J=1,6
        DO 1001 I=1,11
        IF(ECP(J:J).EQ.IDG(I:I)) GO TO 1002
 1001   CONTINUE
        GO TO 1003
 1002   CONTINUE
        READ(ECP(1:6),78912) IGRVOK
78912   FORMAT(I6)
 1003   CONTINUE
        DO 1012 J=1,6
        DO 1011 I=1,11
        IF(ECP2(J:J).EQ.IDG(I:I)) GO TO 1012
 1011   CONTINUE
        GO TO 1013
 1012   CONTINUE
        READ(ECP2(1:6),78912) NPROCS
 1013   CONTINUE
        LDOT=.FALSE.
        DO 1022 J=1,8
        DO 1021 I=1,11
        IF(ECP3(J:J).EQ.IDG(I:I)) GO TO 1022
 1021   CONTINUE
        IF(ECP3(J:J).EQ.'.'.AND..NOT.LDOT) THEN
            LDOT=.TRUE.
            GO TO 1022
        ENDIF
        GO TO 1023
 1022   CONTINUE
        IF(LDOT) READ(ECP3,78913) FACTPR
78913   FORMAT(F8.3)
 1023   CONTINUE
      ENDIF
      L1CRD=.FALSE.
      IF(XPSSHD.EQ.'GPSSHD') LGPSHD=.TRUE.
      KOUNT=KOUNT+1
!
      WRITE(IUNT14,80300) CARDS
      TEST8=CARDS(1)
      TEST2(4)=BLANK2
      IF(LIST) GO TO 1400
      DO 1300 I=1,NLIST
      LIST=LIST.OR.TEST8.EQ.DLIST(I)
 1300 END DO
 1400 CONTINUE
      IF(.NOT.LIST) GO TO 2000
      IF(.NOT.L6SET) GO TO 1700
      ILINE6=ILINE6+1
      IF(ILINE6.LE.MLINE6) GO TO 1600
      IPAGE6=IPAGE6+1
!      WRITE(IOUT6,80210 ) G2EVER,DATE,TIME,IPAGE6
      IF(IPAGE6.EQ.1)                                                   &
     & WRITE(IOUT6,80210 ) G2EVER,DATE,TIME,IPAGE6
      IF(IPAGE6.EQ.1)                                                   &
     & WRITE(IUNT88,80210 ) G2EVER,DATE,TIME,IPAGE6
      ILINE6=3
 1600 CONTINUE
      WRITE(IOUT6,80000) CARDS,KOUNT
 1700 CONTINUE
      IF(.NOT.L9SET) GO TO 2000
      ILINE9=ILINE9+1
      L10=CARDS(10).NE.BLANK
      IF(L10) ILINE9=ILINE9+1
      IF(ILINE9.LE.MLINE9) GO TO 1900
      IPAGE9=IPAGE9+1
      WRITE(IOUT9,80219 ) G2EVER,DATE,TIME,IPAGE9
      WRITE(IUNT88,80219 ) G2EVER,DATE,TIME,IPAGE9
      ILINE9=3
      IF(L10) ILINE9=ILINE9+1
 1900 CONTINUE
      WRITE(IOUT9,80009) I,CARD72
      IF(L10) WRITE(IOUT9,80109) CARDS(10)
 2000 CONTINUE
      IF(TEST8.EQ.DNLIST) LIST=.FALSE.
      IF(KOUNT.LT.NRECS1) GO TO 1000
!**********************************************************************
! READ HEADER RECORD FOR INTEGER DATA
!**********************************************************************
      ITYPE=2
      CALL BINRD(IUNT11,IFFHDR,20)
! TEST BLOCK NUMBER AND RECORD TYPE
      IF(IFFHDR(1).NE.IBLOCK .OR. IFFHDR(3).NE.ITYPE) GO TO 60100
! SET NUMBER OF RECORDS
      NRECS=IFFHDR(4)
! SET RECORD LENGTH
      IRECL=IFFHDR(5)
! SET OBSERVATION FILES PHYSICAL BLOCK SIZE(IN 64 BIT WORDS)
      IOBSBF=IFFHDR(6)
      MAXREC=IFFHDR(7)
!**********************************************************************
! READ DATA RECORD
!**********************************************************************
!
      CALL BINRD(IUNT11,IBUF,IRECL)
!**********************************************************************
! READ SEQUENTIAL OBSERVATION FILE FROM IIS
!**********************************************************************
      IF(IOBSBF.LE.0) GO TO 4000
      IF(KDEPHM.GT.MAXDM)GOTO 60200
      CALL OBSIO(AA, AA(KDYNFF),AA(KDYNFF+IOBSBF),IOBSBF,MAXREC)
 4000 CONTINUE
      RETURN
! BLOCK NUMBER OR DATA RECORD TYPE INCORRECT
60100 WRITE(IOUT6,80100) IBLOCK,ITYPE,IFFHDR
      WRITE(IOUT6,80105)
      WRITE(IOUT6,89999)
      STOP
60200 WRITE(IOUT6,80106) KDYNFF,MAXDM
      WRITE(IOUT6,80107)
      WRITE(IOUT6,89999)
      STOP
17000 FORMAT(BZ,10A8)
18000 FORMAT(BZ,3D24.16)

! 80000 is the card list format
!orig 80000 FORMAT(BZ,1X,10A8,I5)
80000 FORMAT(BZ,1X,10A8,I8)

80100 FORMAT(1X,'BLOCK NUMBER OR RECORD TYPE INCORRECT ',2I5/1X,20I5)
80105 FORMAT(/1X,'CHECK YOUR GIIS RUN SETUP FOR THE CORRECT '/          &
     &       1X,'I64G2E OR I32G2E CARD. '//                             &
     &       1X,'GIIE RUNS ON IBM **MUST** HAVE AN I32G2E CARD '/       &
     &       1X,'GIIE RUNS ON CRAY MAY HAVE I64G2E CARD OR NO CARD '/   &
     &       1X,'( DEFAULT IS I64G2E ) '//)
80106 FORMAT(1X,'MEMORY FOR THIS RUN ',I7,' EXCEEDS MAXIMUM FOR'/       &
     &       1X,'THIS LOAD MODULE',I7)
80107 FORMAT(/1X,'CHECK YOUR INPUT ON THE VECOPT CARD MAXIMUM '/        &
     &       1X,'AMOUNT OF MEMORY AVAILABLE SHOULD BE INCREASED. '//    &
     &       1X,'GIIE MEMORY SHOULD BE INCREASED TOO           ')
                                                                      !
80200 FORMAT('1','GEODYN IIE VERSION - ', F7.2,4X,'RUN TIME - ',F9.0,   &
     & 2X,F7.0,48X,'UNIT  6 PAGE NO.',I6/                               &
     & 1X,'GEODYN IIS VERSION - ', F7.2,4X,'RUN TIME - ',F9.0,2X,F7.0/  &
     & 1X,'       TDF VERSION - ', F7.2,4X,'RUN TIME - ',F9.0,2X,F7.0/  &
     & 1X )
                                                                       !
                                                                       !
                                                                      !j
80210 FORMAT('1','GEODYN IIE VERSION - ', F7.2,4X,'RUN TIME - ',F9.0,   &
     & 2X,F7.0,48X,'UNIT  6 PAGE NO.',I6/1X )
80009 FORMAT(1X,I4,1X,9A8)
80109 FORMAT(6X,A8)
                                                                  !jjm8/
80209 FORMAT('1','GEODYN IIE VERSION ', F7.2,4X,'RUN TIME ',F9.0,       &
     & 1X,F7.0, 3X,'UNIT  9 PAGE NO.',I4/                               &
     & 1X,'GEODYN IIS VERSION ', F7.2,4X,'RUN TIME ',F9.0,1X,F7.0/      &
     & 1X,'       TDF VERSION ', F7.2,4X,'RUN TIME ',F9.0,1X,F7.0/      &
     & 1X )
                                                                   !jjm8
                                                                   !jjm8
                                                                  !jjm8/
80219 FORMAT('1','GEODYN IIE VERSION ', F7.2,4X,'RUN TIME ',F9.0,       &
     & 1X,F7.0, 3X,'UNIT  9 PAGE NO.',I4/1X )
80300 FORMAT(10A8)
89999 FORMAT(1X,'EXECUTION TERMINATING IN SUBROUTINE IFRGLB' )
      END
