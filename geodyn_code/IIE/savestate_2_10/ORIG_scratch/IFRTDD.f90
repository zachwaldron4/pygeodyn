!$IFRTDD
      SUBROUTINE IFRTDD(AA,II,LL,IARCNO,LRDIFF,IFFHDR,FFBUF)
!********1*********2*********3*********4*********5*********6*********7**
! IFRTDD           00/00/00            0000.0    PGMR - SEVITSKI/EDDY
!
! FUNCTION:  READ ARC OBSERVATION DIRECTORY RECORD INFORMATION
!            FROM THE INTERFACE FILE AND STORE IN EXTENDED
!            VIRTUAL MEMORY
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    DYNAMIC ARRAY FOR REAL DATA
!   II      I/O   A    DYNAMIC ARRAY FOR INTEGER DATA
!   LL      I/O   A    DYNAMIC ARRAY FOR LOGICAL DATA
!   IARCNO   I    S    ARC NUMBER
!   LRDIFF   I    S    FLAG USED TO CONTROL READING/LOADING
!                      = T READ IFF AND STORE IN VIRTUAL MEMORY
!                      = F MOVE FROM EXTENDED VIRTUAL MEMORY
!                          TO ARC DYNAMIC ARRAYS OR VICE VERSA
!   IFFHDR   I    A    SCRATCH ARRAY USED FOR READING HEADER INFO
!   FFBUF    I    A    SCRATCH ARRAY USED FOR HOLDING FLOATING
!                      POINT DATA UNTIL IT IS CONVERTED TO CORRECT
!                      REPRESENTATION
!
! COMMENTS: INTERFACE FORMAT # 1 USED
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CIFF  /IFHEAD,IFLNTH,IFBUFL,IFBUFR,KDYNHD,KDYNLN,          &
     &              KDYNIF,KDYNFF,NXCIFF
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DYNPTR/KDEPHM,KDEPH2,KDAHDR(3,9),KDYNAP(3,9,2),KDNEXT(3),  &
     &NXDYNP
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION AA(1)
      DIMENSION FFBUF(1)
      DIMENSION II(1)
      DIMENSION IFFHDR(IFHEAD)
      DIMENSION LL(1)
!
      DATA IBLOCK/70/
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!**********************************************************************
! READ HEADER RECORD FOR REAL DATA
!**********************************************************************
      ITYPE=1
      CALL BINRD(IUNT11,IFFHDR,IFHEAD)
! TEST BLOCK NUMBER AND DATA TYPE
      IF(IFFHDR(1).NE.IBLOCK .OR. IFFHDR(3).NE.ITYPE) GO TO 60200
! SET NO. OF RECORDS AND RECORD LENGTH
      NRECS=IFFHDR(4)
      IRECL=IFFHDR(5)
! STORE NRECS AND IRECL IN EXTENDED VIRTUAL MEMORY
      IPTHDR=KDAHDR(ITYPE,9)
! WARNING - SINCE THE NUMBER OF DIRECTORY RECORDS CHANGES PER ARC
!           II(IPTHDR) IS OF LITTLE USE. THE FIRST DIRECTORY RECORD
!           WRITTEN BY IFWTDD FOR EACH ARC CONTAINS THE NO OF RECORDS
!           FOR THAT ARC
      II(IPTHDR)=NRECS
      II(IPTHDR+1)=IRECL
!***********************************************************************
! READ REAL RECORDS AND STORE IN EXTENDED VIRTUAL MEMORY
!******************************************************************
      IPTR=KDYNAP(ITYPE,9,1)+(IARCNO-1)*KDYNAP(ITYPE,9,2)
      IPTSTR=IPTR
      IF(NRECS.LE.0) GO TO 150
      DO 100 I=1,NRECS
      CALL BINRD2(IUNT11,FFBUF,IRECL)
      CALL Q9ICLA(FFBUF,AA(IPTR),IRECL,ISTAT)
      IF(ISTAT.NE.0) GOTO 60100
      IPTR=IPTR+IRECL
  100 END DO
  150 CONTINUE
      RETURN
60100 WRITE(IOUT6,80100) ISTAT
      WRITE(IOUT6,80999)
      STOP
! BLOCK NUMBER OR DATA TYPE INCORRECT
60200 WRITE(IOUT6,80200) IBLOCK,ITYPE,(IFFHDR(J),J=1,IFHEAD)
      WRITE(IOUT6,80999)
      STOP
! * FORMATS *
80100 FORMAT(1X,'SUBROUTINE Q9ICLA RETURNED CONDITION CODE:',I5)
80200 FORMAT(1X,'BLOCK TYPE OR DATA TYPE INCORRECT',2I6/(1X,20I5))
80999 FORMAT(1X,'EXECUTION TERMINATING IN SUBROUTINE IFRTDD')
      END
