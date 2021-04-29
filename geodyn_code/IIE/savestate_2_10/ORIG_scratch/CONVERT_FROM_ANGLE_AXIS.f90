


!$CONVERT_FROM_ANGLE_AXIS
! Creating the quaternion for rotation
      SUBROUTINE CONVERT_FROM_ANGLE_AXIS(theta,axis,quat)
      IMPLICIT NONE
      DOUBLE PRECISION theta, axis(3), quat(0:3)
      DOUBLE PRECISION sint2
      INTEGER i

      quat(0) = COS(theta/2.D0)
      sint2 = SIN(theta/2.D0)
      DO i=1,3
        quat(i) = axis(i)*sint2
      ENDDO

      RETURN
      END SUBROUTINE CONVERT_FROM_ANGLE_AXIS
