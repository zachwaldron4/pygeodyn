!$IERSMP
      SUBROUTINE IERSMP(MJDSEC, FSEC, Djxp, Djyp)
!********1*********2*********3*********4*********5*********6*********7**
! IERSMP             00/00/00            8805.0    PGMR - JTW
!
! FUNCTION: COMPUTE THE MEAN POLE USING EITHER THE LINEAR IERS 2003
!           MODEL OR THE CUBIC/LINEAR IERS 2010 MODEL
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I         CURRENT INTEGRATION STEP TIME IN MJDS SECONDS
!   FSEC     I         FRACTIONAL PART OF CURRENT STEP TIME
!   Djxp     O
!   Djyp     O
!
! COMMENTS:
!
!*********1*********2*********3*********4*********5*********6*********7
      IMPLICIT DOUBLE PRECISION(A-H,O-Z), LOGICAL(L)
      SAVE

      COMMON/CLGVTM/LGVTM,LDPM,LGVTPM,LOPT,LCSDPM,L_MP_IERS2003,        &
     &              L_CS_IERS2003,L_MP_IERS2010,L_CS_IERS2010
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/DYNPOL/DPMEP ,DPMXP ,DPMYP ,DPMXDT,DPMYDT,DPMOPT,DPMXPC,   &
     &              DPMYPC,DPMKF ,DPMC21,DPMS21,DPMPD ,DPMUDK,DPMVDK,   &
     &              DPMOCP,DPMNLD,DPMERS,XDYNPL

      INTEGER, INTENT(IN) :: MJDSEC
      DOUBLE PRECISION, INTENT(IN) :: FSEC
      DOUBLE PRECISION, INTENT(OUT) :: Djxp, Djyp

      DOUBLE PRECISION, PARAMETER :: SQRT3 = SQRT(3.0D0)

      DOUBLE PRECISION :: t2diff, t3diff, tdiff, TIME, YEAR
      DOUBLE PRECISION :: xp1(4), yp1(4), xp2(4), yp2(4)

      ! IERS2010 data
      DATA xp1/  55.974D0,  1.8243D0,  0.18413D0,  0.007024D0/
      DATA yp1/ 346.346D0,  1.7896D0, -0.10729D0, -0.000908D0/
      DATA xp2/  23.513D0,  7.6141D0,  0.00000D0,  0.000000D0/
      DATA yp2/ 358.891D0, -0.6287D0,  0.00000D0,  0.000000D0/

!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************

      TIME = DBLE(MJDSEC) + FSEC

      IF (L_MP_IERS2003) THEN
          ! VERIFY THAT ALL REQUIRED VALUES ARE SET IF USING THE IERS2003
          ! (LINEAR) MODEL FOR THE MEAN POLE
          IF (DPMXP == 0.0D0 .OR. DPMYP == 0.0D0 .OR. DPMXDT == 0.0D0   &
                  .OR. DPMYDT == 0.0D0) THEN
              WRITE(*,'(8(/,A))')                                       &
                      'ERROR: MUST PROVIDE VALUES FOR ALL 4',           &
                      'QUANTITIES IN COLUMNS 25-80 OF THE POLDYN CARD', &
                      'WHEN USING THE LINEAR (IERS2003) MEAN POLE',     &
                      'MODEL.  IF YOU WANT A VALUE OF ZERO FOR ANY OF', &
                      'THEM, SUBSTITUTE A VERY SMALL NUMBER,',          &
                      'E.G. 1.0E-99, INSTEAD.',                         &
                      'IF YOU HAVE A POLTID CARD, YOU MUST ALSO',       &
                      'PROVIDE A POLDYN CARD.'
              STOP 16
          END IF

          ! USE THE LINEAR (IERS 2003) MODEL FOR THE MEAN POLE.
!         Djxp = DPMXP - DPMXDT*(TIME-DPMEP)
!         Djyp = DPMYP - DPMYDT*(TIME-DPMEP)
          Djxp = DPMXP + DPMXDT*(TIME-DPMEP)
          Djyp = DPMYP + DPMYDT*(TIME-DPMEP)
      ELSE IF (L_MP_IERS2010) THEN
          ! USE THE CUBIC/LINEAR (IERS 2010) MODEL FOR THE MEAN POLE.
          ! ... COMPUTE THE YEAR AND FRACTION OF THE YEAR
          CALL FYEAR(MJDSEC, FSEC, YEAR)
          tdiff = YEAR - 2000.0D0
          t2diff = tdiff * tdiff
          t3diff = t2diff * tdiff

          ! ... 2177020800 IS YYYY-MM-DD=2010-01-01
          IF (TIME < 2177020800.D0) THEN
              Djxp = xp1(1) + xp1(2)*tdiff + xp1(3)*t2diff              &
     &             + xp1(4)*t3diff
              Djyp = yp1(1) + yp1(2)*tdiff + yp1(3)*t2diff              &
     &             + yp1(4)*t3diff
          ELSE
              Djxp = xp2(1) + xp2(2)*tdiff
              Djyp = yp2(1) + yp2(2)*tdiff
          END IF

          Djxp = Djxp * 1.0D-3 * SECRAD
          Djyp = Djyp * 1.0D-3 * SECRAD
      ELSE
          WRITE(*,'(4(/,A))') 'ERROR: IERSMP: EITHER L_MP_IERS2003 OR', &
                          'L_MP_IERS2010 MUST BE TRUE.  THE PROBLEM',   &
                          'IS LIKELY AN INVALID VALUE IN COLUMN 14 OF', &
                          'THE POLDYN CARD.'
          STOP 16
      END IF

      END SUBROUTINE
