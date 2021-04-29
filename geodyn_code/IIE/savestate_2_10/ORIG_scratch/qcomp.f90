!$qcomp
      SUBROUTINE qcomp(q2,q1,q)
!********1*********2*********3*********4*********5*********6*********7**
! qcomp            10/26/95            9510.0    PGMR - S.B. Luthcke
!
! FUNCTION:     perform quaternion composition.
!
!               q=q2(x)q1
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   q2       I    A    second rotation.
!   q1       I    A    first rotation.
!   q        O    A    resulting rotation.
!
!**********1*********2*********3*********4*********5*********6*********7
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
!
      dimension q2(4),q1(4),q(4)
!
      data zero/0.0D0/,one/1.0D0/,tol/1.0D-3/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      q(1)=q2(4)*q1(1)+q2(3)*q1(2)-q2(2)*q1(3)+                         &
     &     q2(1)*q1(4)
      q(2)=-q2(3)*q1(1)+q2(4)*q1(2)+q2(1)*q1(3)+                        &
     &      q2(2)*q1(4)
      q(3)=q2(2)*q1(1)-q2(1)*q1(2)+q2(4)*q1(3)+                         &
     &     q2(3)*q1(4)
      q(4)=-q2(1)*q1(1)-q2(2)*q1(2)-q2(3)*q1(3)+                        &
     &      q2(4)*q1(4)
      qmag=zero
      do 100 i=1,4
      qmag=qmag+q(i)*q(i)
  100 continue
! test and normalize quaternions
      qmag=SQRT(qmag)
      qmagdf=ABS(qmag-one)
      if(qmagdf.gt.tol) then
       write(6,*) '***** WARNING in qcomp *****'
       write(6,*) 'Magnitude of q is not equal to one'
       write(6,*) 'qmag = ',qmag,' tol = ',tol
       write(6,*) 'The quaternion will be normalized'
      endif
!
      do 200 i=1,4
      q(i)=q(i)/qmag
  200 continue
!
      return
      END
