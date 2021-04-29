      SUBROUTINE GDAT_TO_MJD_SEC ( GDAT, MJD, SEC )
! ************************************************************************
! *                                                                      *
! *   Routine  GDAT_TO_MJD_SEC  transforms the date in GDAT Geodyn       *
! *   format to MJD/SEC.                                                 *
! *                                                                      *
! * ### 14-OCT-2004 GDAT_TO_MJD_SEC v1.0 (c)  L. Petrov  14-OCT-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      DOUBLE PRECISION GDAT, SEC
      INTEGER  MJD
!
      MJD = INT ( GDAT/86400.0D0 ) + 30000
      SEC =  GDAT - (MJD-30000)*86400.0D0
      RETURN
      END  SUBROUTINE GDAT_TO_MJD_SEC
