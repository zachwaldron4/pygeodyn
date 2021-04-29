!$QUATERNION_MULT
      SUBROUTINE QUATERNION_MULT(q1,q2,q3)
      IMPLICIT NONE
      DOUBLE PRECISION q1(0:3),q2(0:3),q3(0:3)
      DOUBLE PRECISION cart_q1(3), cart_q2(3)
      DOUBLE PRECISION tempscale_q1(3),tempscale_q2(3)
      DOUBLE PRECISION tempcross_prod(3)
      DOUBLE PRECISION dot_prod
      INTEGER i
!
      DO i=1,3
        cart_q1(i) = q1(i)
        cart_q2(i) = q2(i)
      ENDDO
!
! output delta is product of 2 input deltas -  dot product
      dot_prod = cart_q1(1)*cart_q2(1)+cart_q1(2)*cart_q2(2)+ &
     &           cart_q1(3)*cart_q2(3)
      q3(0) = q1(0)*q2(0) - dot_prod
!
! Scale q1 vector by q2 delta
! Scale q2 vector by q1 delta
      DO i=1,3
        tempscale_q1(i) = cart_q1(i)*q2(0)
        tempscale_q2(i) = cart_q2(i)*q1(0)
      ENDDO
!
! Compute the cross product of 2 input vectors
      CALL CROPDT(cart_q1,cart_q2,tempcross_prod)
!
! Output vector = scaled q1 and q2 vectors
      DO i=1,3
        q3(i) = tempscale_q1(i) + tempscale_q2(i)
      ENDDO
! Plus the cross_prod vector
      DO i=1,3
        q3(i) = q3(i) + tempcross_prod(i)
      ENDDO
      END SUBROUTINE QUATERNION_MULT
