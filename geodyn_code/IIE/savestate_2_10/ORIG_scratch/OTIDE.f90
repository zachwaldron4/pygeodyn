!$OTIDE
      SUBROUTINE OTIDE (AA,II,LL)
!********1*********2*********3*********4*********5*********6*********7**
! OTIDE            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    DYNAMIC REAL ARRAY
!   II      I/O   A    DYNAMIC INTEGER ARRAY
!   LL      I/O   A    DYNAMIC LOGICAL ARRAY
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      DIMENSION AA(1),II(1),LL(1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      JLINE6=ILINE6+3
      IF(JLINE6.LE.MLINE6) GO TO 1000
      IPAGE6=IPAGE6+1
      JLINE6=4
      WRITE(IOUT6,10000) IPAGE6
 1000 CONTINUE
      WRITE(IOUT6,20000)
      ILINE6=JLINE6
      RETURN
10000 FORMAT('1',109X,'UNIT  6 PAGE NO.',I6)
20000 FORMAT('0OTIDE  ** NOT IMPLEMENTED.'/1X )
      END
