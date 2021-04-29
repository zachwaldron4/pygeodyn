!$INTSS
      SUBROUTINE INTSS(IBACK,IORBAK,N3,NH,NSTEPS,XDDOT,SUMR,SUMX)
!********1*********2*********3*********4*********5*********6*********7**
! INTSS
!
! FUNCTION: UPDATE INTEGRATION SUMS
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IBACK   I/O   S    POINTER IN SUMX ARRAY TO CURRENT INTEGRATION STEP
!   IORBAK   I    S    POINTER IN XDDOT ARRAY TO CURRENT INTEGRATION STE
!   N3       I    S    NUMBER OF SATELLITES TIMES 3
!   NH       I    S    NUMBER OF STEPS THAT CAN BE HELD IN SUMX
!   NSTEPS   I    S    NUMBER OF STEPS THAT CAN BE HELD IN XDDOT
!   SUMX    I/O   A    INTEGRATION SUMS
!   XDDOT    I    A    ACCELERATION BACK VALUES
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION SUMX(N3,2,NH),XDDOT(N3,NSTEPS),SUMR(N3,2)
      SAVE
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IBACK=IBACK+1
      IBACK1=IBACK+1
! UPDATE SUMS
      DO 3200 J=1,N3
      SUMX(J,2,IBACK1)=SUMR(J,2)                                        &
     &                +XDDOT(J,IORBAK)
      SUMX(J,1,IBACK1)=SUMX(J,2,IBACK1)                                 &
     &                +SUMR(J,1)
 3200 END DO
      RETURN
      END
