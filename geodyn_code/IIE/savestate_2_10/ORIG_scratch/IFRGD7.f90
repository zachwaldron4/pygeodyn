!$IFRGD7
      SUBROUTINE IFRGD7(AA,II,LL,IFFHDR,IFFLEN,FFBUF)
!********1*********2*********3*********4*********5*********6*********7**
! IFRGD7           00/00/00            0000.0    PGMR - B. EDDY
!                                                PGMR - D. PAVLIS
!
! FUNCTION:  READ GLOBAL DYNAMIC ARRAYS FROM THE INTERFACE FILE
!            FOR ALLOCATION GROUP #7 - SIMULATED DATA
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   LL      I/O   A    LOGICAL DYNAMIC ARRAY
!   IFFHDR   I    A    SCRATCH ARRAY USED FOR HEADER INFORMATION
!   IFFLEN   I    A    SCRATCH ARRAY USED FOR LENGTHS OF DATA
!   FFBUF    I    A    SCRATCH ARRAY USED FOR CONVERTING IBM
!                      FLOATING POINT NUMBERS TO CYBER F.P.
!
! COMMENTS:     INTERFACE FORMAT #2 USED:
!
! INTEGER  HEADER RECORD  - WITH IFFHDR(5)=0 INDICATING LENGTHS RECORDS
!                           FOLLOWS
! INTEGER LENGTHS RECORD  - CONTAINING IFFHDR(4)-1 LENGTHS
!            DATA RECORDS - IFFHDR(4)-1  DATA RECORDS. DATA RECORDS ARE
!                           OF THE TYPE INDICATED IN IFFHDR(3)
!
! COMMENTS:   TO READ ADDITIONAL RECORDS IN THIS ROUTINE
!             1. INCREMENT IRECS IN PROPER GROUP-REAL,INTEGER OR LOGICAL
!             2. ADD CALL TO BINRD2 (REAL) OR BINRD (INTEGER,LOGICAL)
!                IN PROPER GROUP
!             3. FOR FLOATING POINT DATA CALL SUBROUTINE Q9ICLA TO
!                CONVERT REAL WORDS TO THE CORRECT REPRESENTATION
!                FOR THE COMPUTER IIE IS RUNNING ON
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CIFF  /IFHEAD,IFLNTH,IFBUFL,IFBUFR,KDYNHD,KDYNLN,          &
     &              KDYNIF,KDYNFF,NXCIFF
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
      COMMON/CORI01/KMJDS0,KMJDSB,KMJDSC,KMJDSV,KIBACK,KIBAKV,KIORDR,   &
     &              KIORDV,KNOSTP,KNOCOR,KNSAT ,KN3   ,KNEQN ,KNEQN3,   &
     &              KNH   ,KNHV  ,KNSTPS,KNSTPV,KICPP ,KICPV ,KICCP ,   &
     &              KICCV ,KICCPV,KICCVV,KISUMX,KIXDDT,KISMPX,KIPXDD,   &
     &              KNMAX ,KNTOLD,KNTOLO,KICNT ,KISNT ,KMM   ,KKK   ,   &
     &              KJJ   ,KHH   ,KIBDY ,KSIGN1,KSIGN2,KLL   ,KQQ   ,   &
     &              KIORFR,KIPDFR,KITACC,KJSAFR,KJSPFR,KMJDEP,KMJDND,   &
     &              KNVSTP,KNSTRN,KNDARK,KTIPPT,KTJBDY,                 &
     &              KICNTA,KISNTA,KISHDP,KIPTC ,KIPTS ,KIGTSR,KIXTMC,   &
     &              KTIPTT,KTNOSD,KTQNDX,KTLL1 ,KTJJBD,KTITDE,KTCNTR,   &
     &              KTNN  ,KITACX,KNMOVE,KPANEL,KPLPTR,KNADAR,KNADSP,   &
     &              KNADDF,KNADEM,KNADTA,KNADTC,KNADTD,KNADTF,KNADTX,   &
     &              KITPMD,KSCATT,KITPAT,KILTPX,KEASBJ,KEANMP,KEANAN,   &
     &              KEAPMP,KEAPAN,KEAMJS,KEAPPP,KEAAAA,KICNTT,KISNTT,   &
     &              KTPGRC,KTPGRS,KTPC  ,KTPS  ,KALCAP,KEMCAP,KNSEG ,   &
     &              KICNTP,KISNTP,KNRDGA,KNRDDR,KNRDSR,KIRDGA,KIRDRG,   &
     &              KIRSLR,KSTRTA,KSTRTD,KSTRTS,KDYNPE,KACCPE,KIBCKN,   &
     &              KNRAT ,KIXDDN,KISMXN,KDXDDN,KDSMXN,KICPPN,KACSID,   &
     &              KNEQNH,KHRFRC,KPTFBS,KPTFSB,KIPXDA,KIACCP,KXSTAT,   &
     &              KPXST ,KSALST,KMAPLG,KNMBUF,KSTEPS,KSGMNT,KSATIN,   &
     &              KMEMST,KNEQNI,KBUFIN,KWEMGA,KWEMDR,KTPATS,KTANMP,   &
     &              KTAPPP,KTAMJS,KTASID,KGPSID,KNSSVA,KPNALB,KBRAX1,   &
     &              KBRAX2,KBRAX3,NXCI01
      COMMON/CORL01/KLORBT,KLORBV,KLNDRG,KLBAKW,KLSETS,KLAFRC,KLSTSN,   &
     &              KLSNLT,KLAJDP,KLAJSP,KLAJGP,KLTPAT,KEALQT,KLRDGA,   &
     &              KLRDDR,KLRDSR,KFHIRT,KLSURF,KHRFON,KLTPXH,KSETDN,   &
     &              KTALTA,NXCL01
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION AA(1)
      DIMENSION FFBUF(IFBUFR)
      DIMENSION IFFHDR(IFHEAD),IFFLEN(IFLNTH)
      DIMENSION II(1)
      DIMENSION LL(1)
!
      DATA IBLOCK/20/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!**********************************************************************
! READ HEADER RECORD FOR REAL DATA
!**********************************************************************
      CALL BINRD(IUNT11,IFFHDR,IFHEAD)
      ITYPE=IFFHDR(3)
      NRECS1=IFFHDR(4)
      NRECS=NRECS1-1
!**********************************************************************
! READ LENGTHS RECORD FOR REAL DATA
!**********************************************************************
      CALL BINRD(IUNT11,IFFLEN,NRECS1)
! SAVE LAST WORD READ WHICH IS THE SUM OF THE LENGTHS
      NWORDS=IFFLEN(NRECS1)
!**********************************************************************
! READ DATA RECORDS FOR REAL DATA
!**********************************************************************
      IREC=0
      IWORDS=0
! EXAMPLE FOR REAL DATA RECORDS-PLS REMOVE WHEN REAL DATA SENT
!     IREC=IREC+1
!     NX=IFFLEN(IREC)
!     IF(NX.GT.IFBUFR) GO TO 60100
!     IWORDS=IWORDS+NX
!     CALL BINRD2(IUNT11,FFBUF,NX  )
!     IF(NX.GT.0)  CALL Q9ICLA(FFBUF,AA(KCN),NX,ISTAT)
! END OF EXAMPLE
!
! TEST IF CORRECT NUMBER OF DATA RECORDS READ
!
      IF(IREC.NE.NRECS) GO TO 60200
! TEST IF CORRECT NUMBER OF WORDS READ
      IF(NWORDS.NE.IWORDS) GO TO 60300
!**********************************************************************
! READ HEADER RECORD FOR INTEGER DATA
!**********************************************************************
      ITYPE=2
      CALL BINRD(IUNT11,IFFHDR,IFHEAD)
! TEST BLOCK NUMBER AND DATA RECORD TYPE
      IF(IFFHDR(1).NE.IBLOCK .OR. IFFHDR(3).NE.ITYPE) GO TO 60400
      NRECS1=IFFHDR(4)
      NRECS =NRECS1-1
!**********************************************************************
! READ LENGTHS RECORD FOR INTEGER DATA
!**********************************************************************
      CALL BINRD(IUNT11,IFFLEN,NRECS1)
! SAVE SUM OF RECORD LENGTHS
      NWORDS=IFFLEN(NRECS1)
!**********************************************************************
! READ DATA RECORDS FOR INTEGER DATA
!**********************************************************************
      IREC=0
      IWORDS=0
! POINTER
!     IREC=IREC+1
!     IWORDS=IWORDS+IFFLEN(IREC)
!     CALL BINRD(IUNT11,II(POINTER),IFFLEN(IREC) )
!
! TEST IF CORRECT NUMBER OF DATA RECORDS READ
!
      IF(IREC.NE.NRECS) GO TO 60200
! TEST IF CORRECT NUMBER OF WORDS READ
      IF(NWORDS.NE.IWORDS) GO TO 60300
!**********************************************************************
! READ HEADER RECORD FOR LOGICAL DATA
!**********************************************************************
      ITYPE=3
      CALL BINRD(IUNT11,IFFHDR,IFHEAD)
! TEST BLOCK NUMBER AND DATA RECORD TYPE
      IF(IFFHDR(1).NE.IBLOCK .OR. IFFHDR(3).NE.ITYPE) GO TO 60400
      NRECS1=IFFHDR(4)
      NRECS =NRECS1-1
!**********************************************************************
! READ LENGTHS RECORD FOR LOGICAL DATA
!**********************************************************************
      CALL BINRD(IUNT11,IFFLEN,NRECS1)
! SAVE SUM OF DATA RECORD LENGTHS
      NWORDS=IFFLEN(NRECS1)
!**********************************************************************
! READ DATA RECORDS FOR LOGICAL DATA
!**********************************************************************
      IREC=0
      IWORDS=0
!
! LOGICAL DATA RECORDS GO HERE
!
! TEST IF CORRECT NUMBER OF DATA RECORDS READ
!
      IF(IREC.NE.NRECS) GO TO 60200
! TEST IF CORRECT NUMBER OF WORDS READ
      IF(NWORDS.NE.IWORDS) GO TO 60300
      RETURN
!**********************************************************************
! ERROR PROCESSING
!**********************************************************************
!
! RECORD SIZE EXCEEDS BUFFER SPACE
!60100 WRITE(IOUT6,80100) ITYPE,NX,IFBUFR
!     WRITE(IOUT6,89999)
!     STOP
! NUMBER OF RECORDS READ DISAGREES WITH NUMBER OF LENGTHS EXPECTED
60200 WRITE(IOUT6,80200) ITYPE,IREC,NRECS
      WRITE(IOUT6,89999)
      STOP
! NUMBER OF WORDS SENT DISAGREES WITH NUMBER OF WORDS EXPECTED
60300 WRITE(IOUT6,80300) ITYPE,NWORDS,IWORDS
      WRITE(IOUT6,89999)
      STOP
! BLOCK NUMBER OR DATA TYPE INCORRECT
60400 WRITE(IOUT6,80400) IBLOCK,ITYPE,IFFHDR
      WRITE(IOUT6,89999)
      STOP
80000 FORMAT(1X,'UNEXPECTED END OF FILE ON UNIT ',I2,' FOR DATA TYPE ', &
     & I2)
!0100 FORMAT(1X,'RECORD SIZE EXCEEDS BUFFER SPACE AVAILABLE. ITYPE,NX,IF
!    .BUFR FOLLOW: '/1X,3I10)
80200 FORMAT(1X,'NUMBER OF TYPE (',I2,') RECORDS READ FROM INTERFACE', &
     &'FILE DISAGREES WITH NUMBER OF LENGTHS EXPECTED',2I5)
80300 FORMAT(1X,'SUM OF RECORD LENGTHS FROM IFF LENGTHS RECORD DOES ',&
     &'NOT AGREE WITH SUM CALCULATED',3I6)
80400 FORMAT(1X,'BLOCK NUMBER OR DATA TYPE IS INCORRECT',2I5/1X,20I5)
89999 FORMAT(1X,'EXECUTION TERMINATING IN SUBROUTINE IFRGD7 ')
      END
