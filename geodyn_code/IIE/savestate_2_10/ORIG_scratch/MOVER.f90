!$MOVER
      SUBROUTINE MOVER(A,B,NA,L12)
!********1*********2*********3*********4*********5*********6*********7**
! MOVER            00/00/00            0000.0    PGMR - BILL EDDY
!
! FUNCTION:   MOVE FLOATING POINT VALUES FROM ONE ARRAY TO
!             ANOTHER ARRAY
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A       I/O   A    ARRAY OF REAL VALUES TO BE MOVED
!                      AS INDICATED BY L12 AS INPUT
!                      ARRAY RECEIVING VALUES AS INDICATED BY L12 AS
!                      OUTPUT
!   B       I/O   A    ARRAY OF REAL VALUES TO BE MOVED
!                      AS INDICATED BY L12 AS INPUT
!                      ARRAY RECEIVING VALUES AS INDICATED BY L12 AS
!                      OUTPUT
!   NA       I    S    NUMBER OF VALUES TO MOVE
!   L12      I    S    FLAG INDICATING DIRECTION OF DATA MOVEMENT
!                            =T - MOVEMENT FROM A TO B
!                            =F - MOVEMENT FROM B TO A
!
! COMMENTS:
!
! ENTRY POINTS:
!
! NAME  MOVEI
! FUNCTION          MOVE INTEGER VALUES FROM ONE ARRAY TO ANOTHER
!
! NAME  MOVEL
! FUNCTION          MOVE LOGICAL VALUES FROM ONE ARRAY TO ANOTHER
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION A(1),B(1)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      IF(NA.LE.0) RETURN
      IF(.NOT. L12) GO TO 120
      DO 100 I=1,NA
      B(I)=A(I)
  100 END DO
      RETURN
  120 DO 140 I=1,NA
      A(I)=B(I)
  140 END DO
      RETURN
      END
