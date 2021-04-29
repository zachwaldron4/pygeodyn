      FUNCTION ILEN ( STR )
! ************************************************************************
! *                                                                      *
! *   Function ILEN returns the position of the last character of the    *
! *   string STR which is not blank or binary zero. If the string        *
! *   contains only blanks and/or binary zeroes, then ILEN=0             *
! *                                                                      *
! *  ### 17-JAN-1989               v1.0 (c)  L. Petrov  14-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  ILEN
      CHARACTER STR*(*)
      INTEGER  J1
!
      ILEN=0
      DO 410 J1=LEN(STR),1,-1
       ILEN=J1
       IF ( STR(J1:J1) .NE. ' '  .AND.  STR(J1:J1) .NE. CHAR(0) ) RETURN
 410  CONTINUE
      ILEN=0
      RETURN
      END
