!$CHIN
        SUBROUTINE CHIN ( CH, INTEG )
! ************************************************************************
! *                                                                      *
! *     Routine  CHIN  decodes the string CH of character type to the    *
! *   integer number INTEG of INTEGER*4 type.                            *
! *                                                                      *
! *     |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|                                 *
! *     | CHARACTER  ---->  INTEGER    |                                 *
! *     |______________________________|                                 *
! *                                                                      *
! *   If the string cannot be interperted as an integer number then      *
! *   INTEG = -1 111 111 111 .                                           *
! *                                                                      *
! *    Calls: NUM$ARG, PROBE_R, LR$STR, IFOR_MEN .                       *
! *                                                                      *
! *  ###  16-JAN-89      CHIN      v1.0  (c)  L. Petrov  28-OCT-93  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
!!        INTEGER*4 CHIN
        CHARACTER CH*(*), CHH*12
        INTEGER  INTEG
        INTEGER  IO
        INTEGER  ILEN
!
!       CALL CLRCH ( CHH )
        CHH=CH
!       IF ( ILEN(CHH) .EQ. 0 ) THEN
!!             CHIN=-2
!            INTEG= -1111111111
!            RETURN
!       END IF
        READ ( UNIT=CHH, FMT='(I12)', IOSTAT=IO ) INTEG
        IF ( IO .NE. 0 ) THEN
!!             CHIN=-4
             INTEG= -1111111111
             RETURN
        END IF
!
!!        CHIN=0
        RETURN
        END
