!$ORBEND
      SUBROUTINE ORBEND
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CORBIN/MJDSNF,NENTRY
      COMMON/CORBUF/ORBBUF(50,7)
!
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!
      DATA D9S/99999999.0D0/
!C      DATA IUNT30/31/    ! original
!     ....set unit to 29 to avoid conflict with ORBFIL which uses
!     ....units 30 and up  -- jjm 12/5/95
      DATA IUNT30/29/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      NENTRY=MOD(NENTRY,7)
      IF(NENTRY.EQ.0) GO TO 2000
      NENTRY=NENTRY+1
      DO 1000 K=NENTRY,7
      DO 1000 I=1,50
      ORBBUF(I,K)=D9S
 1000 CONTINUE
      WRITE(IUNT30) ORBBUF
 2000 CONTINUE
      DO 3000 K=1,7
      DO 3000 I=1,50
      ORBBUF(I,K)=D9S
 3000 CONTINUE
      WRITE(IUNT30) ORBBUF
      WRITE(IUNT30) ORBBUF
      CLOSE(UNIT=30)
!c      write(iout6, 11111)
!c11111 FORMAT(' ** ORBEND **  ORBINF FILE CLOSED.')
      RETURN
      END
