        SUBROUTINE GR_TAT ( GSTR, RAD, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine HR_TAT  transforms the input string in format            *
! *   GGG_MM_SS.FFFF  from degrees, minutes and arcseconds to radians.   *
! *                                                                      *
! * ____________________ Input parameters: _____________________________ *
! *                                                                      *
! *     GSTR  ( CHARACTER ) -- String which keeps the angle in form      *
! *                            GGG_MM_SS.FFFF, where GGG degrees, MM     *
! *                            arc-minutes, SS.FFFF arcsedoncs (FFFF --  *
! *                            fraction parts of seconds: 0-16 digits).  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     RAD   ( REAL*8    ) --  Angle in radians.                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *             Error codes:                                             *
! *             ~~~~~~~~~~~~                                             *
! *                                                                      *
! *             IUER=0  --  sucesfull completion;                        *
! *             IUER=1  --  Errror: wrong string format;                 *
! *             IUER=2  --  Error: empty line;                           *
! *                                                                      *
! *     Comment:                                                         *
! *         If IUER=1 or IUER-2 then RAD=-1.1111111111111D11             *
! *                                                                      *
! *  ### 05-JUL-1991     GR_TAT    v1.0 (c)  L. Petrov  17-SEP-2001 ###  *
! *                                                                      *
! ************************************************************************
        CHARACTER GSTR_W*40
!!        INTEGER*4 BOSTR, GSTR(2)
        CHARACTER  GSTR*(*)
        DOUBLE PRECISION RAD, RS, PI, PI180
        PARAMETER ( PI=3.141592653589793D0 )
        PARAMETER ( PI180=PI/180.D0 )
!
!       CALL CLRCH ( GSTR_W )
        GSTR_W = GSTR
!
! ----- Extraction of subfields from the string
!
        CALL INANG_TAT ( GSTR_W, IG, IM, RS, IUER_INANG )
!
! ----- Transforming ot to radians
!
        RAD=(( RS/60.D0 +IM )/60.D0+IG )*PI180
!
! ----- Fixing the sign
        IF(GSTR_W(1:1).EQ.'-') RAD=-1.D0*RAD

!
!       IF ( INDEX ( GSTR_W(1:I_LEN(GSTR_W)), '-' ).NE.0 ) RAD=-1.D0*RAD
        RETURN
        END
