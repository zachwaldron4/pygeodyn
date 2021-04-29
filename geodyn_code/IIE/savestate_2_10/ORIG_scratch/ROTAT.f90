!$ROTAT
      SUBROUTINE ROTAT(THETA,A,N)
!********1*********2*********3*********4*********5*********6*********7**
! ROTAT           12/02/92            0000.0    PGMR - S.LUO
!
!
!  N = 1 : PERFORM ANGLE ROTATION WITH X-AXIS
!
!        1         0         0
!   A =  0    COS(THETA)  SIN(THETA)
!        0   -SIN(THETA)  COS(THETA)
!
!  N = 2 : PERFORM ANGLE ROTATION WITH Y-AXIS
!
!        COS(THETA)   0    -SIN(THETA)
!   A =  0            1         0
!        SIN(THETA)   0     COS(THETA)
!
!  N = 3 : PERFORM ANGLE ROTATION WITH Z-AXIS
!
!        COS(THETA)  SIN(THETA)   0
!   A = -SIN(THETA)  COS(THETA)   0
!        0              0         1
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A        O    A    MATRIX OF DIM 3X3
!   THETA    I    S    ROTATION ANGLE (IN RADIAN)
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
          IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION A(3,3)
      DATA ZERO/0.D0/,ONE/1.D0/
      DO 10 K=1,3
      DO 20 J=1,3
      A(K,J)=ZERO
   20 END DO
   10 END DO
      C =COS(THETA)
      S =SIN(THETA)
!
      IF(N.EQ.1) THEN
      A(1,1)=ONE
      A(2,2)=C
      A(2,3)=S
      A(3,2)=-S
      A(3,3)=C
      GO TO 99
      ENDIF
!
      IF(N.EQ.2) THEN
      A(1,1)=C
      A(2,2)=ONE
      A(1,3)=-S
      A(3,1)=S
      A(3,3)=C
      GO TO 99
      ENDIF
!
      IF(N.EQ.3) THEN
      A(1,1)=C
      A(1,2)=S
      A(2,1)=-S
      A(2,2)=C
      A(3,3)=ONE
      GO TO 99
      ENDIF
!
! IF N IS NOT 1,2,3 THEN STOP
!
      WRITE(6,999)
  999 FORMAT('NO THIS AXIS EXISTING')
      STOP 999
! ... DEBUG OUTPUT...
   99 PI = 3.141592615
!     THETAD = THETA * 180.D0/PI
!     WRITE(6,100) THETAD,A
  100 FORMAT(1X,'THETA =',F7.2,'A=',9F7.3)
      RETURN
      END
