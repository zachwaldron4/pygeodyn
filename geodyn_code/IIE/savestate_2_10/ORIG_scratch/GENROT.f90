!$GENROT
        SUBROUTINE GENROT(INVEC,AXIS,ANGLE,OUTVEC)
!*******************************************************************
!  ROUTINE NAME:   GENROT   DATE: 01/24/97      PGMR: A. MARSHALL
!
!   FUNCTION - This subroutine rotates the vector "in_vec" "angle"
!              degrees around the axis "axis". This subroutine can
!              safely be called with the same vector as "in_vec"
!              and "out_vec", the result being that the input
!              vector will be modified to contain the rotated vector.
!              out_vec=COS(angle)*in_vec+SIN(angle)*(A x in_vec)+
!                      A*A'*in_vec*(1-COS(angle))
!              where A is the normalized rotation axis, "axis".
!
!              MODIFIED CODE ORIGINALLY WRITTEN FOR GIPSY BY
!              YOAZ BAR-SEVER (JPL:  yeb@cobra.jpl.nasa.gov)
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   INVEC    A      I    INPUT VECTOR TO BE ROTATED
!   AXIS     A      I    AXIS ABOUT WHICH INVEC IS ROTATED
!   ANGLE    S      I    ANGLE OF ROTATION
!   OUTVEC   A      O    ROTATED VECTOR
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DOUBLE PRECISION INVEC(3)
      SAVE
!
!
       DIMENSION CINVEC(3),AXIS(3),AXISN(3),OUTVEC(3)
!
       DATA ONE/1.0D0/,C90P0/90.0D0/
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
! Copy the input vector.
       CINVEC(1)=INVEC(1)
       CINVEC(2)=INVEC(2)
       CINVEC(3)=INVEC(3)
!
! Normalize the rotation axis.
       c=SQRT(axis(1)**2+axis(2)**2+axis(3)**2)
       if (c .eq. 0) then
          write(6,*) 'Error in subroutine genrot'
          write(6,*) 'Rotation axis,',axis,', is zero.'
          write(6,*) 'Program stopped.'
          stop
       endif
       c=ONE/c
       AXISN(1)=axis(1)*c
       AXISN(2)=axis(2)*c
       AXISN(3)=axis(3)*c
!
! Compute the cosine of the angle
       c=COS(angle*ASIN(ONE)/C90P0)
       s=SIN(angle*ASIN(ONE)/C90P0)
!
!** Compute the rotated vector.
       d=AXISN(1)*CINVEC(1)+AXISN(2)*CINVEC(2)+AXISN(3)*CINVEC(3)
       OUTVEC(1) = AXISN(2)*CINVEC(3)-AXISN(3)*CINVEC(2)
       OUTVEC(2) = AXISN(3)*CINVEC(1)-AXISN(1)*CINVEC(3)
       OUTVEC(3) = AXISN(1)*CINVEC(2)-AXISN(2)*CINVEC(1)
       OUTVEC(1)=c*CINVEC(1)+s*OUTVEC(1)+(ONE-c)*d*AXISN(1)
       OUTVEC(2)=c*CINVEC(2)+s*OUTVEC(2)+(ONE-c)*d*AXISN(2)
       OUTVEC(3)=c*CINVEC(3)+s*OUTVEC(3)+(ONE-c)*d*AXISN(3)
!
       return
      END
