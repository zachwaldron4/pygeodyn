        SUBROUTINE TAI_TO_TDB ( MJD, TAI, TDB )
! ************************************************************************
! *                                                                      *
! *     Routine TAI_TO_TDB  computes the argument TDB on the moment of   *
! *   time TAI. Computation is done according to the formula 2.222-1     *
! *   in page 42 Explanatory Supplement to the Astronomical Almanac      *
! *   / edited by P.K.Seidelmann / University Science Book, 1992.        *
! *   Pecision of computaiton: --  20 microsec.                          *
! *                                                                      *
! * ________________________ Input Patameters: _________________________ *
! *                                                                      *
! *  MJD ( INTEGER*4 ) -- Modified Julian data on the midnight.          *
! *                       Units: days.                                   *
! *  TAI ( REAL*8    ) -- Moment of time. Unnits: sec.                   *
! *                                                                      *
! * ________________________ Output Patameters: ________________________ *
! *                                                                      *
! *  TDB ( REAL*8    ) -- Argument TDB.                                  *
! *                                                                      *
! *  ###  23-OCT-1990  TAI_TO_TDB  v 1.1 (c) L. Petrov  08-DEC-2003 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT    NONE
        INTEGER   MJD
        DOUBLE PRECISION TAI, TDB, G, BT, DT
        INTEGER    J2000__MJD
        PARAMETER  ( J2000__MJD  = 51544       ) ! 2000.01.01_00:00:00

      INCLUDE 'COMMON_DECL.inc'
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY

!
! ----- Time elapsed since J2000 in days
!
        DT = MJD - J2000__MJD - 0.5D0 + TAI/86400.D0
!
! ----- G  --   Earth mean anomaly
!
        G=( 357.53D0  +  0.9856003D0 *DT ) * DEGRAD
!
! ----- BT  --  The difference TDB-TDT
!
        BT  = 0.001658D0 *SIN(G) + 0.000014D0 *SIN(2.D0*G)
        TDB = TAI + 32.184D0 + BT
        RETURN
        END
