!
      SUBROUTINE QUATERNION_ROTATE(cart_in, q, cart_out)
      IMPLICIT NONE
      DOUBLE PRECISION cart_in(3), cart_out(3)
      DOUBLE PRECISION q(0:3),q_inv(0:3)
      DOUBLE PRECISION temp(0:3), temp2(0:3), temp3(0:3)
!
      IF(q(0) .NE. 1.D0) THEN
! Put cart_in into a quaternion with delta=0
        temp(0) = 0.D0
        temp(1) = cart_in(1)
        temp(2) = cart_in(2)
        temp(3) = cart_in(3)
! Compute the inverse of q
        q_inv(0) =  q(0)
        q_inv(1) = -q(1)
        q_inv(2) = -q(2)
        q_inv(3) = -q(3)
! Multiply Cartesian with q_inv
        CALL QUATERNION_MULT(temp,q_inv,temp2)
! Multiply q with above
        CALL QUATERNION_MULT(q,temp2,temp3)
! Final output is cartesian stripped from quaternion
        cart_out(1) = temp3(1)
        cart_out(2) = temp3(2)
        cart_out(3) = temp3(3)
      ELSE
! If q(4)=0, no rotation
        cart_out(1) = cart_in(1)
        cart_out(2) = cart_in(2)
        cart_out(3) = cart_in(3)
      ENDIF
!
      RETURN
      END SUBROUTINE QUATERNION_ROTATE
