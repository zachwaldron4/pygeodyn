!$PHSPED
      SUBROUTINE PHSPED(PHASE,SPEED,DNUM,CNUM,BETA,BETAP,U,K)
!********1*********2*********3*********4*********5*********6*********7**
! PHSPED           07/08/94            9408.0    PGMR - N. PAVLIS
!
!  FUNCTION  :
!  COMPUTE AMPLITUDE AND PHASE OF THE ANGULAR ARGUMENT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PHASE   O    S    PHASE
!   SPEED   O    S    ANGULAR VELOCITY
!   DNUM    I    A    DOODSON NUMBERS
!   CNUM    I    A    ASSOCIATED INDICES
!  BETA     I    A    DOODSON VARIABLES ........RADIANS
!  BETAP    I    A    RATES ....................RADIANS/JULIAN CENTURY
!                     (1 JULIAN CENTURY = 36525 DAYS)
!  U        I    A    NODAL ANGLE ARGUMENTS.....RADIANS
!
! COMMENTS
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      COMMON/CIOCON/NUMCON(15),NESCON(15),NUMIND,NXCON
      DIMENSION DNUM(6,NUMIND),CNUM(6,NUMIND)
      DIMENSION BETA(6),BETAP(6),U(6)
      PHASE=0.D0
      SPEED=0.D0
      DO 100 I=1,6
      PHASE=PHASE+DNUM(I,K)* BETA(I)+CNUM(I,K)*U(I)
      SPEED=SPEED+DNUM(I,K)*BETAP(I)
  100 END DO
      RETURN
      END
