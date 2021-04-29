!$DBAREA
      SUBROUTINE DBAREA(NSBDY,nsbdy1,MSBDY,LBSHD,                       &
     &                  XSAT,AS,AX,AEBOD,ISHDPT,                        &
     &                  AREADD)
!********1*********2*********3*********4*********5*********6*********7**
! DBAREA           00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  CALCULATE THE AREA THAT IS DOUBLE COUNTED BY SUBROUTINE
!            SOLRAT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NSBDY    I         NUMBER OF BODIES THAT SHADOW THE SUN
!   MSBDY    I         MAXIMUM NUMBER SOLRAT CAN HANDLE (USED
!                      IN DIMENSIONS OF LBSHD)
!   LBSHD    I        AN ARRAY SUCH THAT LBSHD(I,J)=.TRUE. IFF
!                     THE ITH & JTH BODIES INTERSECT
!   XSAT     I        POSITION OF THE SATELLITE
!   AS       I        APPARENT RADIUS OF SUN
!   AX       I        AARAY OF APPARENT RADII OF BODIES (SUN NOT
!                     INCLUDED)
!   AEBOD    I        ARRAY OF ACTUAL RADII OF BODIES(INCLUDING SUN 1ST)
!   ISHDPT   I        ARRAY OF POINTERS TO BDTRUE (SUN=8  NOT INCLUDED)
!   AREADD   O        PERCENT OF AREA TO BE ADDED BACK ON
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CBDTRU/BDTRUE(7,999)
      DIMENSION LBSHD(MSBDY,MSBDY),XSAT(3),AX(NSBDY1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      RETURN
      END
