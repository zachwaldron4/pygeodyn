!$DMQN
      SUBROUTINE DMQN(MJDSC,FSEC,PDPSI,PEPSM,PEPST,EPST,DPSI,RPN1,      &
     & RPN2,RPN3,RPN4,RPN5,RPN6,RPN7,RPN8,RPN9,EQN,SCRTCH,NM,LONLEQ)
!********1*********2*********3*********4*********5*********6*********7**
! DMQN             93/03/31                      PGMR - S. LUO
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
!   DPSI     I    A    NUTATION IN LONGITUDE
!   EPST     I    A    TRUE OBLIQUITY AT T
!   PDPSI    I    A    MARS PRECESSION ELEMENT IN LONGITUDE
!   PEPSM    I    A    MEAN OBLIQUITY AT J2000
!   DEPST    I    A    MEAN OBLIQUITY AT T
!   EQN      O    A
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
      DIMENSION FSEC(NM),EPST(NM),PDPSI(NM),PEPST(NM),PEPSM(NM),        &
     & DPSI(NM)
      DIMENSION SCRTCH(NM,5),EQN(NM),RPN1(NM),RPN2(NM),RPN3(NM),        &
     & RPN4(NM),RPN5(NM),RPN6(NM),RPN7(NM),RPN8(NM),RPN9(NM)
      DIMENSION A(3,3),B(3,3),C(3,3),PM(3,3),PXM(3,3),PNM(3,3)
      DATA ZERO/0.D0/
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!  CALCULATE MARS PRECESSION MATRIX
      do 50 i=1,nm
      a1=pepsm(i)/degrad
      a2=pdpsi(i)/degrad
      a3=pepst(i)/degrad
      write(6,*)' dbg pepsm pdpsi pepst ',a1,a2,a3,i
   50 continue
!
      DO 100 I=1,NM
      CALL ROTAT(PEPSM(I),A,1)
      CALL ROTAT(-PDPSI(I),B,3)
      CALL MULTI(B,A,C,3,3,3)
      CALL ROTAT(-PEPST(I),A,1)
      CALL MULTI(A,C,PM,3,3,3)
      WRITE(6,200)I,((PM(K,J),J=1,3),K=1,3)
!
!  CALCULATE MARS NUTATION MATRIX
!
      CALL ROTAT(PEPST(I),A,1)
      CALL ROTAT(-DPSI(I),B,3)
      CALL MULTI(B,A,C,3,3,3)
      CALL ROTAT(-EPST(I),A,1)
      CALL MULTI(A,C,PXM,3,3,3)
!     do 6161 k=1,3
!     do 6161 j=1,3
!     pxm(k,j)=0.d0
!6161  continue
!     PXM(1,1)=1.D0
!     PXM(2,2)=1.D0
!     PXM(3,3)=1.D0
!
!  CALCULATE MARS PRECESSION AND NUTATION MATRIX - PNM
!
      CALL MULTI(PXM,PM,PNM,3,3,3)
!
!     RPN1(I)=PNM(1,1)
!     RPN2(I)=PNM(2,1)
!     RPN3(I)=PNM(3,1)
!     RPN4(I)=PNM(1,2)
!     RPN5(I)=PNM(2,2)
!     RPN6(I)=PNM(3,2)
!     RPN7(I)=PNM(1,3)
!     RPN8(I)=PNM(2,3)
!     RPN9(I)=PNM(3,3)
      RPN1(I)=PNM(1,1)
      RPN2(I)=-PNM(1,2)
      RPN3(I)=-PNM(1,3)
      RPN4(I)=-PNM(2,1)
      RPN5(I)=PNM(2,2)
      RPN6(I)=PNM(2,3)
      RPN7(I)=-PNM(3,1)
      RPN8(I)=PNM(3,2)
      RPN9(I)=PNM(3,3)
!
!  EQUATION OF EQUINOX OF MARS IF LONLEQ=.TRUE.
!
      IF(LONLEQ)EQN(I) =DPSI(I)*COS(EPST(I))
!     IF(LONLEQ)EQN(I) =ZERO
!C    WRITE(6,*)'EQN=',EQN(I)
!
      write(6,*)' date ',mjdsc,fsec(i),i
      WRITE(6,200)I,((PNM(K,J),J=1,3),K=1,3)
!C    WRITE(6,200)I,RPN1(I),RPN2(I),RPN3(I),RPN4(I),RPN5(I),RPN6(I),
!C   *RPN7(I),RPN8(I),RPN9(I)
  200 FORMAT(I6/3(3D20.6/))
  100 END DO
!
      RETURN
      END
