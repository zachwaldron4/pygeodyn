      FUNCTION   SOU_INDEX (  SOU_NAME,C_SOU,NQUAB )
! ************************************************************************
! *                                                                      *
! *   Auxiliary function SOU_INDEX finds the index of the source         *
! *   SOU_NAME                                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  SOU_NAME ( CHARACTER ) -- Source name.                              *
! *                                                                      *
! * _________________________ Ouput parameters: ________________________ *
! *                                                                      *
! * <SOU_INDEX> ( INTEGER*4 ) -- Index of the source in the source       *
! *                                  list                                *
! *                                                                      *
! * ### 09-MAR-2004   SOU_INDEX  v1.0 (c)  L. Petrov 09-MAR-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  SOU_NAME*(*)
      INTEGER  SOU_INDEX
      INTEGER  J1
      INTEGER  NQUAB
      CHARACTER  C_SOU(NQUAB)*8
!
      DO 410 J1=1,NQUAB
         IF (C_SOU(J1).EQ.SOU_NAME ) THEN
              SOU_INDEX = J1
              RETURN
         END IF
 410  CONTINUE
!
      SOU_INDEX = 0
      RETURN
      END  FUNCTION   SOU_INDEX
