!$IFRADR
      SUBROUTINE IFRADR(IFFHDR,IFFLEN)
!********1*********2*********3*********4*********5*********6*********7**
! IFRADR           00/00/00            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  READ THE INTERFACE FILE ARC DIRECTORY
!            INFORMATION THAT TELLS IIE WHERE
!            IN DYNAMIC MEMORY(UNLABELLED COMMON) TO STORE
!            ARC INFORMATION.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IFFHDR   I    A    SCRATCH ARRAY USED FOR READING HEADER RECORD
!   IFFLEN   I    A    SCRATCH ARRAY USED FOR READING LENGTHS RECORD
!
! COMMENTS:
!            POINTERS TO WHERE IIE WILL STORE ARC COMMON BLOCK,
!            ARC DYNAMIC ARRAYS AND ARC TRACKING DATA
!            DIRECTORY INFORMATION ARE STORED IN KDYNAP(1-9,1).
!            OFFSETS FOR SUBSEQUENT ARCS ARE COMPUTED AND STORED
!            IN KDYNAP(1-9,2).
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CIFF  /IFHEAD,IFLNTH,IFBUFL,IFBUFR,KDYNHD,KDYNLN,          &
     &              KDYNIF,KDYNFF,NXCIFF
      COMMON/CLASCI/LASCII,LIFCYB,LARECL,NXLASC
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
      DIMENSION IFFHDR(IFHEAD),IFFLEN(IFLNTH)
      DIMENSION IKDBUF(84)
      EQUIVALENCE(IKDBUF(1),KDAHDR(1,1))
!
      DATA IBLOCK/40/
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!**********************************************************************
! READ HEADER RECORD
!**********************************************************************
      ITYPE=2
      IF(LASCII) THEN
       IF(LARECL) THEN
       READ(IUNT11,17000) IFFHDR
       ELSE
       READ(IUNT11,18000) IFFHDR
       ENDIF
      ELSE
       CALL BINRD(IUNT11,IFFHDR,IFHEAD)
      ENDIF
! VERIFY BLOCK NUMBER AND DATA TYPE
      IF(IFFHDR(1).NE.IBLOCK .OR. IFFHDR(3).NE.ITYPE) GO TO 60100
!**********************************************************************
! READ DATA RECORD
!*********************************************************************
      IF(LASCII) THEN
       IF(LARECL) THEN
       READ(IUNT11,17000) KDAHDR,KDYNAP,KDNEXT
       ELSE
       READ(IUNT11,18000) KDAHDR,KDYNAP,KDNEXT
       ENDIF
      ELSE
       CALL BINRD(IUNT11,IKDBUF(1),84)
      ENDIF
      RETURN
!**********************************************************************
! ERROR MESSAGES
!**********************************************************************
60100 WRITE(IOUT6,80100) IBLOCK,ITYPE,IFFHDR
      STOP
17000 FORMAT(BZ,3I25)
18000 FORMAT(BZ,5I25)
80100 FORMAT(1X,'BLOCK NUMBER OR DATA TYPE DOES NOT MATCH IN ',&
     &'SUBROUTINE IFRADR '/1X,2I5/1X,20I5 )
      END
