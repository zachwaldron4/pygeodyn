!$OBSIN
      SUBROUTINE OBSIN (AA,OBSBUF,IOBSBF,IBUF1,IBUF2)
!********1*********2*********3*********4*********5*********6*********7**
! OBSIN            00/00/00            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  READ OBSERVATION RECORDS(PHYSICAL RECORDS) FROM A
!            DIRECT ACCESS FILE AND STORE IN A DYNAMIC ARRAY
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   OBSBUF  I/O   A    DYNAMIC ARRAY TO BE USED FOR OBSERVATIONS AS
!                      INPUT.  OBSERVATION BUFFER CONSISTING OF THE
!                      LOGICAL OBSERVATION RECORDS THAT ARE
!                      CONTAINED IN PHYSICAL RECORDS IBUF1 TO
!                      IBUF2 AS OUTPUT
!   IOBSBF   I    S    PHYSICAL RECORD LENGTH (?)
!   IBUF1    I    S    FIRST PHYSICAL RECORD NUMBER TO BE READ
!   IBUF2    I    S    LAST PHYSICAL RECORD NUMBER TO BE READ
!
! COMMENTS:
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
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION AA(1)
!CC   DIMENSION OBSBUF(IOBSBF) ! wrong dimension in old code
      DIMENSION OBSBUF(*)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      INDBUF=1
      DO 1000 IBUF=IBUF1,IBUF2
!CC         CALL DREAD(IUNT13,IBUF,OBSBUF(INDBUF),*60100)
         CALL DREAD(IUNT13,IBUF,OBSBUF(INDBUF))
         INDBUF=INDBUF+IOBSBF
 1000 END DO
!
! NORMAL RETURN
      RETURN
!
!     ....ERROR RETURN
!
!CC60100 CONTINUE
!CC      WRITE(IOUT6,80100) IBUF
!CC      STOP 16
!CC80100 FORMAT(1X,'I/O ERROR TRYING TO READ RECORD NUMBER ',I7,
!CC     . ' IN SUBROUTINE OBSIN')
      END
