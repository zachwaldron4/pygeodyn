!$NODELT
      SUBROUTINE NODELT(NM,XI,MAXM,M3,DNLT,SCRTCH)
!********1*********2*********3*********4*********5*********6*********7**
! NODELT           89/02/03            8901.6    PGMR - DAVE ROWLANDS
!
! FUNCTION:  ALTER THE S/C ORBIT AT MULTIPLE TIMES FOR
!            THE LENSE-THIRRING EFFECT
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NM       I    S     NUMBER OF TIMES FOR WHICH INTERPOLATION NEEDED
!   XI       O    A     INTERPOLATED ORBIT POSITIONS AND VELOCITIES
!                       ALTERED FOR LENSE-THIRRING
!   MAXM     I    S     MAX NUMBER OF SATELLITES WHICH CAN BE
!                       INTERPLOATED TOGETHER TIMES 3
!   M3       I    S     NUMBER OF SATELLITES TIMES 3 TO BE STORED
!                       IN THE OUTPUT ARRAY
!   DNLT     I          ACCUMLATED NODE SHIFT DUE TO LENSE-THIRRING
!   SCRATCH       A
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION XI(MAXM,M3,2),SCRTCH(NM,2)
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      COSDLT=COS(DNLT)
      SINDLT=SIN(DNLT)
!
!
      DO 100 N=1,NM
      SCRTCH(N,1)=XI(N,1,1)*COSDLT-XI(N,2,1)*SINDLT
      SCRTCH(N,2)=XI(N,1,1)*SINDLT+XI(N,2,1)*COSDLT
      XI(N,1,1)=SCRTCH(N,1)
      XI(N,2,1)=SCRTCH(N,2)
      SCRTCH(N,1)=XI(N,1,2)*COSDLT-XI(N,2,2)*SINDLT
      SCRTCH(N,2)=XI(N,1,2)*SINDLT+XI(N,2,2)*COSDLT
      XI(N,1,2)=SCRTCH(N,1)
      XI(N,2,2)=SCRTCH(N,2)
  100 END DO
      RETURN
      END
