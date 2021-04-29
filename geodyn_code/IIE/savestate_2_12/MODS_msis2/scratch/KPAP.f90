!$KPAP
      subroutine KPAP(XKP,icnt,xkptmp)
!********1*********2*********3*********4*********5*********6*********7**
! KPAP             00/00/00            0000.0    PGMR - J. RIDGEWAY
!
! FUNCTION: THIS SUBROUTINE OUTPUTS AP, GIVEN AN INPUT KP VALUE.
!           IF KP IS AN INTEGRAL MULTIPLE OF 1/3, THEN THE AP OUTPUT
!           IS NEARLY EXACT.  IF KP IS NOT A MULTIPLE OF 1/3, THEN THE
!           AP OUTPUT IS LINEARLY INTERPOLATED BETWEEN TWO VALUES.
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XKP      I    A    array of Kp values
!   ICNT     I    S    number of Kp values input
!   XKPTMP   O    A    array of computed Ap values
!
! COMMENTS:
!
! REFERENCES:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION AP(28), xkp(*), xkptmp(*)
!
      DATA AP/0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,                          &
     &     9.D0,12.D0,15.D0,18.D0,22.D0,27.D0,32.D0,39.D0,              &
     &     48.D0,56.D0,67.D0,80.D0,94.D0,                               &
     &     111.D0,132.D0,154.D0,179.D0,207.D0,236.D0,                   &
     &     300.D0,400.D0/
      DATA THIRD/0.3333333333D0/
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!
      do 2000 k=1,icnt
      XKPX = xkp(k)
!
! THE FOLLOWING LINE HAS BEEN MODIFIED AS CHRIS COX REPORTED
! AND DAVE ROWLANDS APPROVED.
!     N1 = INT( XKPX*3.0D0 + 0.01D0 ) + 1
!
      N1 = INT( XKPX*3.0D0 + 0.1D0 ) + 1
      N2 = N1 + 1
      IF(N1 .GT. 28 ) THEN
        write(6,701) XKPX,N1
        RETURN
  701   FORMAT(2X,'** ERROR IN KPAP. XKP=',F15.1,1X,'N1 = ',I5)
      ENDIF
!
      IF(N1 .EQ. 28) N2 = N1
!
      XX = DBLE(N1-1)*THIRD
      SLOPE = 3.0D0*(AP(N2)  - AP(N1))
      if( ABS( xkpx - xx ) .gt. 0.01D0 ) then
         xkptmp(k) =  (XKPX - XX)*SLOPE + AP(N1)
      else
         xkptmp(k) = AP(N1)
      endif
!
!cc   write(6,610) XKPX,xkptmp(k)
  610 FORMAT(2X,'** XKP = ',F10.2,2X,'xkptmp(k) ',F10.2)
!
 2000 continue
      RETURN
      END
