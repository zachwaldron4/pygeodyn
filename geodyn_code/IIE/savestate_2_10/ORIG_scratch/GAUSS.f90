!$GAUSS
      SUBROUTINE GAUSS (R,X)
!********1*********2*********3*********4*********5*********6*********7**
! GAUSS            06/17/93            9305.3    PGMR -
!
!
! FUNCTION:
!             compute gaussian distribution for input random numbers
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   R        I    S    RANDOM NUMBER INPUT
!   X        O    S    GAUSSIAN OUTPUT
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
      iflag=0
      if (r .gt. 0.5D+0) go to 10
      p=r
      iflag=1
       go to 20
   10 p=1.0D+0 - r
   20 if (p .gt. 1.486719515D-06) go to 30
      x=5.0D+0
      go to 40
   30 t=SQRT(LOG(1.0D+0/p/p))
      t2=t*t
      x=t - (2.515517D+0 + 0.802853D+0*t + 0.010328D+0*t2)              &
     &  /(1.0D+0 + 1.432788D+0*t  + 0.189269D+0*t2 + 0.001308D+0*t*t2)
   40 if (iflag .eq. 1) x=-x
      return
!
!...end gauss
      END
