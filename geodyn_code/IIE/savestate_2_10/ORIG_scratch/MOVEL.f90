!$MOVEL
      SUBROUTINE MOVEL(LA,LB,NLA,L12)
!********1*********2*********3*********4*********5*********6*********7**
! MOVEL            00/00/00            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  MOVE LOGICAL VALUES FROM ONE ARRAY TO ANOTHER
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   LA      I/O   A    ARRAY OF LOGICAL VALUES TO BE MOVED AS INPUT
!                      ARRAY RECEIVING VALUES AS INDICATED BY L12 AS
!                      OUTPUT
!   LB      I/O   A    ARRAY OF LOGICAL VALUES TO BE MOVED AS INPUT
!                      ARRAY RECEIVING VALUES AS INDICATED BY L12 AS
!                      OUTPUT
!   NLA      I    S    NUMBER OF VALUES TO MOVE
!   L12      I    S    FLAG INDICATING DIRECTION OF DATA MOVEMENT
!                            =T - MOVEMENT FROM LA TO LB
!                            =F - MOVEMENT FROM LB TO LA
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION LA(1),LB(1)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      IF(NLA.LE.0) RETURN
      IF(.NOT. L12) GO TO 320
      DO 300 I=1,NLA
      LB(I)=LA(I)
  300 END DO
      RETURN
  320 DO 340 I=1,NLA
      LA(I)=LB(I)
  340 END DO
      RETURN
      END
