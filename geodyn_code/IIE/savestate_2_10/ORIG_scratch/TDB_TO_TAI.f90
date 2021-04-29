!
        SUBROUTINE TDB_TO_TAI ( MJD, TDB, TAI )
! ************************************************************************
! *                                                                      *
! *     Routine TDB_TO_TAU computes the moment of time TAI whcih         *
! *   corresponds to the argument TDB. Computation is done according     *
! *   to the formula 2.222-1 in page 42 Explanatory Supplement to the    *
! *   Astronomical Almanac / edited by P.K.Seidelmann / University       *
! *   Science Book, 1992. Pecision of computaiton: --  20 microsec.      *
! *                                                                      *
! * ________________________ Input Patameters: _________________________ *
! *                                                                      *
! *  MJD ( INTEGER*4 ) -- Modified Julian data on the midnight.          *
! *                       Units: days.                                   *
! *  TDB ( REAL*8    ) -- Argument TDB. Units: sec                       *
! *                                                                      *
! * ________________________ Output Patameters: ________________________ *
! *                                                                      *
! *  TAI ( REAL*8    ) -- Moment of time. Units: sec.                    *
! *                                                                      *
! *  ###  23-OCT-1990  TDB_TO_TAI v 1.1 (c) L. Petrov  08-DEC-2003 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT    NONE
        INTEGER   MJD
        DOUBLE PRECISION TAI, TDB,  G, BT, DT
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
        G=( 357.53D0   +   0.9856003D0 *DT )* DEGRAD
!
! ----- BT  --  The difference TDB-TDT
!
        BT = 0.001658D0 *SIN(G)   +   0.000014D0 *SIN(2.D0*G)
        TAI = TDB - 32.184D0 - BT
!
        RETURN
        END
