!$DXQN
      SUBROUTINE DXQN(MJDSC,FSEC,RAP,DCP,RPN1,RPN2,RPN3,RPN4,           &
     &                RPN5,RPN6,RPN7,RPN8,RPN9,SCRTCH,NM)
!********1*********2*********3*********4*********5*********6*********7**
! DXQN             86/12/24            8701.0    PGMR - D. ROWLANDS
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
!   RAP      I    A    PRECESION ELEMENTS
!   DCP      I    A    PRECESION ELEMENTS
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
      DIMENSION FSEC(NM),RAP(NM),DCP(NM)
      DIMENSION SCRTCH(NM,4),RPN1(NM),RPN2(NM),RPN3(NM)
      DIMENSION RPN4(NM),RPN5(NM),RPN6(NM),RPN7(NM),RPN8(NM),RPN9(NM)
      COMMON/DNUTAT/RAPXTR(1),DCPXTR(1),DNUTXR(8),SMDNUT(30)
      COMMON/EPHMPT/NTOT,NTOTS,NNPDPR,NUTORD,IBODDG(8),IGRPDG(4),       &
     &              NENPD,INRCD(2),KPLAN(11),NINEP,NEPHMR,NEPHEM,       &
     &              IBHINT,NINTBH,NXEPHP
      COMMON/EPHSET/EMFACT,DTENPD,DTINPD,FSC1EN,FSCENP(2),FSCINP(4),    &
     &   FSDINP(4),XEPHST
      COMMON/XTRAOR/T0XTRO,T0XTRO2,CVENUS,CLIBMN,CMARS,CMERC,XXTRAO
      DATA ZERO/0.D0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!
!
      IF(CMARS.GT.1.9D0.AND.CMARS.LT.2.1D0) THEN
      CALL DYQN(MJDSC,FSEC,RPN1,RPN2,RPN3,RPN4,                   &
     &          RPN5,RPN6,RPN7,RPN8,RPN9,NM)
      RETURN
      ENDIF
!
!
!
!
!
!
      DO 100 I=1,NM
      SCRTCH(I,1)=SIN(RAP(I))
      SCRTCH(I,2)=COS(RAP(I))
      SCRTCH(I,3)=SIN(DCP(I))
      SCRTCH(I,4)=COS(DCP(I))
  100 END DO
      DO 200 I=1,NM
      RPN1(I)=-SCRTCH(I,1)
      RPN2(I)=-SCRTCH(I,2)*SCRTCH(I,3)
      RPN3(I)=SCRTCH(I,2)*SCRTCH(I,4)
  200 END DO
      DO 300 I=1,NM
      RPN4(I)=SCRTCH(I,2)
      RPN5(I)=-SCRTCH(I,1)*SCRTCH(I,3)
      RPN6(I)=SCRTCH(I,1)*SCRTCH(I,4)
  300 END DO
      DO 400 I=1,NM
      RPN7(I)=ZERO
      RPN8(I)=SCRTCH(I,4)
      RPN9(I)=SCRTCH(I,3)
  400 END DO
      RETURN
      END
