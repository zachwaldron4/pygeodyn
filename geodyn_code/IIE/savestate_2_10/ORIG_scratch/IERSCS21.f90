!$IERSCS21
      SUBROUTINE IERSCS21(Djxp, Djyp, C5, C6, C7, S6, S7, LSKIP)
!********1*********2*********3*********4*********5*********6*********7**
! IERSCS21           00/00/00            8805.0    PGMR -
!
! FUNCTION: MODIFIES GRAVITATIONAL C&S COEFFICIENTS FOR TIME DEPENDENT
!           GRAVITY AND FOR DYNAMIC POLAR MOTION, FOLLOWING EITHER THE
!           IERS 2003 OR IERS 2010 CONVENTIONS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   Djxp     I
!   Djyp     I
!   C5       I         GRAVITY COEFFICIENT C(2,0)
!   C6      I/O        GRAVITY COEFFICIENT C(2,1)
!   C7       I         GRAVITY COEFFICIENT C(2,2)
!   S6      I/O        GRAVITY COEFFICIENT S(2,1)
!   S7       I         GRAVITY COEFFICIENT S(2,2)
!   LSKIP    I         TRUE if the user is defining C21 S21
!
! COMMENTS:
!
!*********1*********2*********3*********4*********5*********6*********7
      IMPLICIT DOUBLE PRECISION(A-H,O-Z), LOGICAL(L)
      SAVE

      DOUBLE PRECISION, INTENT(IN) :: Djxp, Djyp, C5, C7, S7
      DOUBLE PRECISION, INTENT(INOUT) :: C6, S6
      LOGICAL, INTENT(IN) :: LSKIP

      COMMON/CLGVTM/LGVTM,LDPM,LGVTPM,LOPT,LCSDPM,L_MP_IERS2003,        &
     &              L_CS_IERS2003,L_MP_IERS2010,L_CS_IERS2010
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/DYNPOL/DPMEP ,DPMXP ,DPMYP ,DPMXDT,DPMYDT,DPMOPT,DPMXPC,   &
     &              DPMYPC,DPMKF ,DPMC21,DPMS21,DPMPD ,DPMUDK,DPMVDK,   &
     &              DPMOCP,DPMNLD,DPMERS,XDYNPL

      DOUBLE PRECISION, PARAMETER :: SQRT3 = SQRT(3.0D0)

      DOUBLE PRECISION :: CDLTA, SDLTA, DMIERS1, DMIERS2

!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************

      ! ADD DYNAMIC POLAR MOTION CONTRIBUTION TO C(2,1) AND S(2,1)

      IF (L_CS_IERS2003) THEN
          ! USE THE IERS 2003 MODEL
          ! C(2,1) IS IN C(6); S(2,1) IS IN S(6); C(2,0) IS IN
          ! C(5) AND THE SQUARE ROOT OF 3 IS NEEDED BECAUSE THE C,S
          ! COEFS ARE NORMALIZED.
          ! CONTRIBUTION FROM THE "SECOND PATH" (SEE POLDYN CARD
          ! DOCUMENTATION)
          C6 = C6 + DPMKF*DPMUDK*C5*SQRT3
          S6 = S6 - DPMKF*DPMVDK*C5*SQRT3
      ELSE IF (L_CS_IERS2010) THEN
          ! IERS2010 MODEL
          IF (.NOT. LSKIP) THEN
              ! CONTRIBUTION FROM THE "FIRST PATH" (SEE POLDYN CARD
              ! DOCUMENTATION)
              C6 =  Djxp*C5*SQRT3 - Djxp*C7 + Djyp*S7
              S6 = -Djyp*C5*SQRT3 - Djyp*C7 - Djxp*S7
          END IF

          ! ADD SOLID EARTH TIDE
          ! THE 8 LINES OF CODE BELOW ADD THE SOLID EARTH POLE TIDE
          ! AND "SECOND PATH" (SEE POLDYN CARD DOCUMENTATION)
          ! CONTRIBUTION TO C21/S21 (FROM IERS2010 EQUATION 6.22)
          DMIERS1 =   DPMXPC - Djxp
          DMIERS2 = -(DPMYPC - Djyp)
          DMIERS1 = DMIERS1 / SECRAD
          DMIERS2 = DMIERS2 / SECRAD
          CDLTA = -1.333D-9 * (DMIERS1+0.0115D0*DMIERS2)
          SDLTA = -1.333D-9 * (DMIERS2-0.0115D0*DMIERS1)
          C6 = C6 + CDLTA
          S6 = S6 + SDLTA
      ELSE
          WRITE(*,'(2A)') 'ERROR: IERSCS21: EITHER L_CS_IERS2003 OR ',  &
     &                    'L_CS_IERS2010 MUST BE TRUE'
          STOP 16
      END IF

      END SUBROUTINE
