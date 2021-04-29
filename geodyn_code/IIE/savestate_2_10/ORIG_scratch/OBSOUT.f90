!$OBSOUT
      SUBROUTINE OBSOUT (AA,OBSBUF,IOBSBF,IBUF1,IBUF2)
!********1*********2*********3*********4*********5*********6*********7**
! OBSOUT           00/00/00            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  OUTPUT OBSERVATION RECORDS FROM DYNAMIC ARRAY TO
!            DIRECT ACCESS FILE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   OBSBUF   I    A    DYNAMIC ARRAY CONTAINING OBSERVATION
!                      RECORDS TO BE OUTPUT TO DISK
!   IOBSBF   I    S    DIMENSION OF OBSBUF
!   IBUF1    I    S    FIRST PHYSICAL RECORD NUMBER TO BE OUTPUT
!   IBUF2    I    S    LAST PHYSICAL RECORD NUMBER TO BE OUTPUT
!
! COMMENTS:  OBSBUF CONSISTS OF THE LOGICAL OBSERVATION RECORDS
!            TO BE OUTPUT IN PHYSICAL RECORDS IBUF1 THROUGH
!            IBUF2
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
      DIMENSION OBSBUF(IOBSBF)
!
!**********************************************************************
! SPART OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      INDBUF=1
      DO 1000 IBUF=IBUF1,IBUF2
      CALL DWRITE(IUNT13,IBUF,OBSBUF(INDBUF))
      INDBUF=INDBUF+IOBSBF
 1000 END DO
      RETURN
      END
