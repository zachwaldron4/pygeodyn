      FUNCTION   STA_INDEX (  STA_NAME,C_STA,NSTA )
! ************************************************************************
! *                                                                      *
! *   Auxiliary function STA_INDEX finds the index of the station        *
! *   STA_NAME                                                           *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  STA_NAME ( CHARACTER ) -- Station name.                             *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <STA_INDEX> ( INTEGER*4 ) -- Index of the station                    *
! *                                                                      *
! * ### 09-MAR-2004   STA_INDEX  v1.0 (c)  L. Petrov 09-MAR-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  STA_NAME*(*)
      INTEGER  STA_INDEX
      INTEGER  J1
      INTEGER  NSTA
      CHARACTER  C_STA(NSTA)*8
!
      DO 410 J1=1,NSTA
         IF ( C_STA(J1) .EQ. STA_NAME ) THEN
              STA_INDEX = J1
              RETURN
         END IF
 410  CONTINUE
!
      STA_INDEX = 0
      RETURN
      END  FUNCTION   STA_INDEX
