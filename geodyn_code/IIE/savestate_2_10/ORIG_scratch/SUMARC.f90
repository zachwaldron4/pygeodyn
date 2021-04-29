!$SUMARC
      SUBROUTINE SUMARC(AA,II,LL)
!********1*********2*********3*********4*********5*********6*********7**
! SUMARC           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A
!   II      I/O   A
!   LL      I/O   A
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CORA05/KOBTYP,KDSCRP,KGMEAN,KGLRMS,KWGMEA,KWGRMS,KTYMEA,   &
     &       KTYRMS,KWTMTY,KWTYRM,KTMEAN,KTRMS ,KWMEAN,KWTRMS,KWTRND,   &
     &       KPRVRT,KEBSTT,KVLOPT,NXCA05
      COMMON/CORI05/KNOBGL,KNOBWG,KNOBTY,KNOBWY,KNOBST,KNOBWT,KNEBOB,   &
     &              KJSTAT,NXCI05
      COMMON/CORL05/KLGPRV,NXCL05
      COMMON/CSTATS/MSTATS,MTYPES,NXCSTT
      DIMENSION AA(1),II(1),LL(1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!***** DEBUG *****
!     PRINT 12345,MSTATS,MTYPES
!2345 FORMAT(' ** SUMARC **  MSTATS,MTYPES=',2I12)
!     PRINT 12346,  KOBTYP,KDSCRP,
!    1              KGMEAN,KGLRMS,KWGMEA,KWGRMS,
!    2              KTYMEA,KTYRMS,KWTMTY,KWTYRM,
!    3              KTMEAN,KTRMS ,KWMEAN,KWTRMS,
!    4              KWTRND,KPRVRT
!2346 FORMAT(' ** SUMARC ** CORA05=',2I12/(1X,4I12))
!     PRINT 12347,  KNOBGL,KNOBWG,KNOBTY,KNOBWY,KNOBST,KNOBWT
!2347 FORMAT(' ** SUMARC ** CORI05=',2I12/(1X,4I12))
!     PRINT 12348,  KLGPRV
!2348 FORMAT(' ** SUMARC ** CORL05=',I12)
!***** END DEBUG *****
      CALL SUMAA (AA(KGMEAN),AA(KTYMEA),MTYPES)
      CALL SUMAA (AA(KGLRMS),AA(KTYRMS),MTYPES)
      CALL SUMAA (AA(KWGMEA),AA(KWTMTY),MTYPES)
      CALL SUMAA (AA(KWGRMS),AA(KWTYRM),MTYPES)
      CALL SUMII (II(KNOBGL),II(KNOBTY),MTYPES)
      CALL SUMII (II(KNOBWG),II(KNOBWY),MTYPES)
      RETURN
      END
