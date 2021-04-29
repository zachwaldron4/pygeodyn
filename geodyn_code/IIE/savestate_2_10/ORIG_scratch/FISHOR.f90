      SUBROUTINE FISHOR(ANTORB,NOFFST,IQPP,ANTOR)
!********1*********2*********3*********4*********5*********6*********7**
! FISHOR                                         PGMR - D. ROWLANDS
!
!
! FUNCTION:  FISH THE GPS ANTENNA ORIENTATION UNIT VECTORS OUT OF
!            THE BIG ANTTENNA ORIENTATION ARRAY PASSED FROM IIS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ANTORB   I    A    THE ARRAY CONTAINING THE ORIENTATION OF ALL OF
!                      THE GPS ANTENNAE FOR LOW EARTH ORBITING
!                      SATELLITES
!   NOFFST   I    S    NUMBER OF OFFSETS (ANTENNAE) IN THE RUN
!   IQPP     I    A    ANTENNA NUMBER FOR WHICH ORIENTATION IS TO BE
!                      FISHED OUT
!   ANTOR    O    A    ANTOR(I,J) IS THE FISHED OUT ORIENTATION
!                      J=1 IS THE X VECTOR. J=2 IS THE Y VECTOR,  ETC.
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION ANTORB(NOFFST,3,3),ANTOR(3,3)
      DO I=1,3
      DO J=1,3
      ANTOR(I,J)=ANTORB(IQPP,J,I)
      ENDDO
      ENDDO
      RETURN
      END
