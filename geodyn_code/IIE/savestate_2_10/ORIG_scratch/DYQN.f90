!$DYQN
      SUBROUTINE DYQN(MJDSC,FSEC,RPN1,RPN2,RPN3,RPN4,                   &
     &                RPN5,RPN6,RPN7,RPN8,RPN9,NM)
!********1*********2*********3*********4*********5*********6*********7**
! DYQN             86/12/24            8701.0    PGMR - D. ROWLANDS
!
!  FUNCTION:  PRECESION (NUTATION) MATRIX IS CALCULATED.
!             FOR A PLANET OTHER THAN EARTH. UNLIKE DEQN (EARTH)
!             THIS IS DONE IN A STRAIGHTFORWARD MANNER.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSC    I    S    INTEGRAL EPHEMERIS SECOND TIME
!   FSEC     I    A    FRACTIONAL REMAINING SECONDS
!   RPN1     O    A    1,1 POSITION OF NP MATRX
!   RPN2     O    A    2,1 POSITION OF NP MATRX
!   RPN3     O    A    3,1 POSITION OF NP MATRX
!   RPN4     O    A    1,2 POSITION OF NP MATRX
!   RPN5     O    A    2,2 POSITION OF NP MATRX
!   RPN6     O    A    3,2 POSITION OF NP MATRX
!   RPN7     O    A    1,3 POSITION OF NP MATRX
!   RPN8     O    A    2,3 POSITION OF NP MATRX
!   RPN9     O    A    3,3 POSITION OF NP MATRX
!   SCRTCH  I/O   A    MUST BE 4 TIMES THE NUMBER OF OBSERVATIONS.
!                      WORKING ARRAY
!   NM       I    S    NUMBER OF OBSERVATIONS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/YODER /RFXM2E(9),XI0MR,XI0MRD,XI00MR,XIMR(9),              &
     &                        TSI0MR,TSIMRD,TSIMR(9),                   &
     &              ALFMAR(9),THTMAR(9),XNPMAR,XMAMR0,XS2000,XRATE0,    &
     &              FI0MR,FI0MRD,FICMR(4),FISMR(4)
      DIMENSION FSEC(NM)
      DIMENSION SCRTCH(NM,4),RPN1(NM),RPN2(NM),RPN3(NM)
      DIMENSION RPN4(NM),RPN5(NM),RPN6(NM),RPN7(NM),RPN8(NM),RPN9(NM)
      DIMENSION SMAT1(3,3),SMAT2(3,3),SMAT3(3,3),SMAT4(9)
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      SMAT1(1,1)=RFXM2E(1)
      SMAT1(1,2)=RFXM2E(2)
      SMAT1(1,3)=RFXM2E(3)
      SMAT1(2,1)=RFXM2E(4)
      SMAT1(2,2)=RFXM2E(5)
      SMAT1(2,3)=RFXM2E(6)
      SMAT1(3,1)=RFXM2E(7)
      SMAT1(3,2)=RFXM2E(8)
      SMAT1(3,3)=RFXM2E(9)
!
      DIFF0=DBLE(MJDSC)-XS2000
!
!
!
      DO 1000 I=1,NM
      TIME=DIFF0+FSEC(I)
      XINUT=XI00MR
      TSINUT=0.D0
      DO J=1,9
        ANG=ALFMAR(J)*TIME+THTMAR(J)
        XINUT=XINUT+XIMR(J)*COS(ANG)
        TSINUT=TSINUT+TSIMR(J)*SIN(ANG)
      ENDDO
      XI= XI0MR +TIME*XI0MRD+XINUT
      TSI=TSI0MR+TIME*TSIMRD+TSINUT
!
      ANG=TSI
      CALL R3(ANG,SMAT2)
      CALL MMULT3(SMAT2,SMAT1,SMAT3)
      ANG=XI
      CALL R1(ANG,SMAT2)
      CALL MMULT3(SMAT2,SMAT3,SMAT4)
!
      RPN1(I)=SMAT4(1)
      RPN2(I)=SMAT4(2)
      RPN3(I)=SMAT4(3)
      RPN4(I)=SMAT4(4)
      RPN5(I)=SMAT4(5)
      RPN6(I)=SMAT4(6)
      RPN7(I)=SMAT4(7)
      RPN8(I)=SMAT4(8)
      RPN9(I)=SMAT4(9)
 1000 CONTINUE
      RETURN
      END
