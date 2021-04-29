!$CORELM
      SUBROUTINE CORELM(N,V)
!********1*********2*********3*********4*********5*********6*********7**
! CORELM           85/02/14            0000.0    PGMR - GEODYN 1
!
! FUNCTION:  PRINT CORRELATION COEFFICIENTS FOR ADJUSTED SATELLITE
!            ELEMENTS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   N        I    S     TYPE OF ELEMENTS
!                       1-CARTESIAN; 2-KEPLER; 3-NON SINGULAR KEPL
!   V        I    A     VARIANCE/COVARIANCE MATRIX
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!
      INTEGER, INTENT(IN) :: N
      DIMENSION V(6,6)
!
      INTEGER :: I, J
      CHARACTER(6), DIMENSION(6) :: TTL
      CHARACTER(6), DIMENSION(6), PARAMETER ::                          &
     &     TTLANS=(/'  A   ',' ECWN ',' ESWN ',' SISN ',                &
     &     ' SICN ','  MWN '/)
      CHARACTER(6), DIMENSION(6), PARAMETER ::                          &
     &     TTLA=(/'  A   ','  E   ','  I   ',' NODE ',                  &
     &     ' PERG ',' MEAN '/)
      CHARACTER(6), DIMENSION(6), PARAMETER ::                          &
     &     TTLX=(/'  X   ','  Y   ','  Z   ',' XDOT ',                  &
     &     ' YDOT ',' ZDOT '/)
      CHARACTER(9), PARAMETER :: XYZ='CARTESIAN'
      CHARACTER(9), PARAMETER :: AEI='KEPLERIAN'
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
! DIVIDE OFF DIAGONAL TERMS BY SQRT OF ROW AND COLUMN DIAGONAL TERMS
      DO 150 I=1,5
      VII=SQRT(ABS(V(I,I)))
      I1=I+1
      DO 100 J=I1,6
      V(I,J)=V(I,J)/VII
  100 END DO
  150 END DO
!
      DO 250 I=2,6
      VII=SQRT(ABS(V(I,I)))
      I1=I-1
      DO 200 J=1,I1
      V(J,I)=V(J,I)/VII
  200 END DO
  250 END DO
!
! PRINT TITLES AND CORRELATION MATRIX
      SELECT CASE (N)
        CASE (1)
           DO J=1,6
              TTL(J)=TTLX(J)
           ENDDO
           WRITE(IOUT6, 81000) XYZ
        CASE (2)
           DO J=1,6
              TTL(J)=TTLA(J)
           ENDDO
           WRITE(IOUT6, 81000) AEI
        CASE (3)
           DO J=1,6
              TTL(J)=TTLANS(J)
           ENDDO
           WRITE(IOUT6, 81001)
      END SELECT
      WRITE(IOUT6,82000) (TTL(K),K=2,6)
      WRITE(IOUT6,84000) (V(1,J),J=2,6), TTL(1)
      WRITE(IOUT6,84100) (V(2,J),J=3,6), TTL(2)
      WRITE(IOUT6,84200) (V(3,J),J=4,6), TTL(3)
      WRITE(IOUT6,84300) (V(4,J),J=5,6), TTL(4)
      WRITE(IOUT6,84400) V(5,6), TTL(5)
      WRITE(IOUT6,83000)
      RETURN
81000 FORMAT('0',3X,'CORRELATION COEFFICIENTS FOR ADJUSTED ',A,         &
     &  ' EPOCH ELEMENTS')
81001 FORMAT('0',3X,                                                    &
     & 'CORRELATION COEFFICIENTS FOR ADJUSTED NON SINGULAR',            &
     & ' KEPLERIAN EPOCH ELEMENTS')
82000 FORMAT('0',4X,5(A6,2X))
83000 FORMAT('0')
84000 FORMAT('0',3X,5(F7.4,1X),A6)
84100 FORMAT('0',11X,4(F7.4,1X),A6)
84200 FORMAT('0',19X,3(F7.4,1X),A6)
84300 FORMAT('0',27X,2(F7.4,1X),A6)
84400 FORMAT('0',35X,1(F7.4,1X),A6)
      END
