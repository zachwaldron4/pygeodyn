!$MOVEI
      SUBROUTINE MOVEI(IA,IB,NIA,L12)
!********1*********2*********3*********4*********5*********6*********7**
! MOVEI            00/00/00            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  MOVE INTEGER VALUES FROM ONE ARRAY TO ANOTHER
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IA      I/O   A    ARRAY OF INTEGER VALUES TO BE MOVED AS INPUT
!                      ARRAY RECEIVING VALUES AS INDICATED BY L12 AS
!                      OUTPUT
!   IB      I/O   A    ARRAY OF INTEGER VALUES TO BE MOVED AS INPUT
!                      ARRAY RECEIVING VALUES AS INDICATED BY L12 AS
!                      OUTPUT
!   NIA      I    S    NUMBER OF VALUES TO MOVE
!   L12      I    S    FLAG INDICATING DIRECTION OF DATA MOVEMENT
!                            =T - MOVEMENT FROM IA TO IB
!                            =F - MOVEMENT FROM IB TO IA
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION IA(1),IB(1)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      IF(NIA.LE.0) RETURN
      IF(.NOT. L12) GO TO 220
      DO 200 I=1,NIA
      IB(I)=IA(I)
  200 END DO
      RETURN
  220 DO 240 I=1,NIA
      IA(I)=IB(I)
  240 END DO
      RETURN
      END
