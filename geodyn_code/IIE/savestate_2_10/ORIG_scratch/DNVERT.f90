!$DNVERT
      SUBROUTINE DNVERT(N,AXMX,NT,NR)
!********1*********2*********3*********4*********5*********6*********7**
! DNVERT           00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  TO INVERT A MATRIX USING GAUSS-JORDAN METHOD OF
!            CONDENSATION WITH PARTIAL (COLUMN) PIVOTING
!            CALLING SEQUENCE  CALL DNVERT(N,AXMX,NT,NR)
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   N        I         NUMBER OF ROWS OR COLUMNS OF THE MATRIX
!                      AXMX WHICH ARE ACTUALLY UTILIZED IN THE
!                      CALLING PROGRAM
!
!   AXMX    I/O        MATRIX TO BE INVERTED
!  (NT,NT)             INVERSE OF THE INPUT MATRIX AXMX AS
!                      OUTPUT
!   NT       I         DIMENSION OF THE MATRIX AXMX AS DEFINED BY
!                      THE CALLING PROGRAM
!
!   NR       I         TEMPORARY STORAGE USED BY PROGRAM TO STORE
!   (N)                THE INDICES OF THE PIVOTAL COLUMNS
!                      THE DIMENSION OF THE ARRAY IN THE CALLING
!                      PROGRAM EQUIVALENT TO NR MUST BE AT LEAST
!                      EQUAL TO N
!
! COMMENTS:  SUBROUTINES USED  NONE
!            COMMON BLOCKS     NONE
!            INPUT FILES       NONE
!            OUTPUT FILES      NONE
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DIMENSION NR(N), AXMX(NT,NT)
      DATA ZERO/0.D0/,ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! SEARCH THE LTH ROW STARTING WITH THE DIAGONAL ELEMENT AND WORKING
! RIGHT SAVE THE INDEX NUMBER OF THE JTH COLUMN CONTAINING LARGEST
! NUMBER IN ABSOLUTE MAGNITUDE
      DO 285 L=1,N
      P=ZERO
      DO 272 J=L,N
      IF(.NOT.P.LT. ABS(AXMX(L,J))) GO TO 272
      P= ABS(AXMX(L,J))
      NR(L)=J
  272 END DO
! INTERCHANGE LTH AND JTH COLUMNS TO PLACE PIVOTAL ELEMENT ON MAIN
! DIAGONAL
      J=NR(L)
      DO 273 K=1,N
      C=AXMX(K,J)
      AXMX(K,J)=AXMX(K,L)
  273 AXMX(K,L)=C
! SAVE  INVERSE OF PIVOTAL ELEMENT 1./AXMX(L,L)
      C=ONE/AXMX(L,L)
! SET DIAGONAL ELEMENT AXMX(L,L) = 1.
      AXMX(L,L)=ONE
! MULTIPLY LTH ROW BY INVERSE OF PIVOTAL ELEMENT
      DO 282 J=1,N
  282 AXMX(L,J)=C*AXMX(L,J)
! MULTIPLY KTH ROW BY VALUE IN DIAGONAL POSITION
! SUBTRACTING CONSECUTIVE ROW VALUES
      DO 285 K=1,N
      IF(L.EQ.K) GO TO 285
      C=AXMX(K,L)
      AXMX(K,L)=ZERO
      DO 284 J=1,N
  284 AXMX(K,J)=AXMX(K,J)-C*AXMX(L,J)
! INVERSE CAN NOW BE FOUND BY ADJUSTING FOR PREVIOUS COLUMN OPERATIONS
  285 CONTINUE
      DO 289 I=1,N
      L=N+1-I
      K=NR(L)
      DO 289 J=1,N
      C=AXMX(L,J)
      AXMX(L,J)=AXMX(K,J)
  289 AXMX(K,J)=C
      RETURN
      END
