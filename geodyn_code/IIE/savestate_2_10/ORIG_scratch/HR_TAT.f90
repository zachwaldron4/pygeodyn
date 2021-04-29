        SUBROUTINE HR_TAT ( HSTR, RAD, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine HR_TAT  transforms the input string in format            *
! *   HH_MM_SS.FFFF  from time of seconds to radians.                    *
! *                                                                      *
! * ____________________ Input parameters: _____________________________ *
! *                                                                      *
! *     HSTR  ( CHARACTER )  --  string which contains the angle in      *
! *                              time in seconds in the form             *
! *                              HH_MM_SS.FFFF ( where FFFF fractional   *
! *                              part can have from 0 to 16 characters). *
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
! *  ### 05-JUL-1991     HR_TAT    v1.0 (c)  L. Petrov  17-SEP-2001 ###  *
! *                                                                      *
! ************************************************************************
        CHARACTER HSTR_W*40
        CHARACTER  HSTR*(*)
        INTEGER  IUER_INANG
        DOUBLE PRECISION RAD, RS, PI, PI12
        PARAMETER ( PI=3.141592653589793D0 )
        PARAMETER ( PI12=PI/12.D0 )
!
!
! ----- Extraction of subfields from the string  HSTR_W
!
        IUER_INANG = IUER
        HSTR_W=HSTR
        CALL INANG_TAT ( HSTR_W, IH, IM, RS, IUER_INANG )
!
! ----- Transforming the angle to radians
!
        RAD=(( RS/60.D0 +IM )/60.D0 + IH )*PI12
!
! ----- Set sign
!
!       IF ( INDEX ( HSTR_W(1:I_LEN(HSTR_W)), '-' ).NE.0 ) RAD=-1.D0*RAD
!       CALL ERR_LOG ( 0, IUER )
        RETURN
        END
