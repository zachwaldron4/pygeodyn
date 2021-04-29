!$VTD_ROTMAT
! ------------------------------------------------------------------------
!
        SUBROUTINE VTD_ROTMAT ( IAX, ANGLE, MAT )
! ************************************************************************
! *                                                                      *
! *     Auxiliary routine  VTD_ROTMAT  computes the rotation matrix      *
! *   3x3 with respet to the axis IAX at the angle ANGLE (in rad).       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    IAX ( INTEGER*4 ) -- index of the axis: one of 1 (X), 2 (Y),      *
! *                         or 3 (Z)                                     *
! *  ANGLE ( REAL*8    ) -- Angle. Units: rad.                           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    MAT ( REAL*8    ) -- Rotation matrix 3,3                          *
! *                                                                      *
! * ###  23-JUN-1992  VTD_ROTMAT    v2.0 (c) L. Petrov 08-DEC-2003  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  IAX
      DOUBLE PRECISION ANGLE, MAT(3,3), DC, DS
!
      DC = COS(ANGLE)
      DS = SIN(ANGLE)
!
      IF ( IAX .EQ. 1 ) THEN
           MAT(1,1) = 1.D0
           MAT(1,2) = 0.D0
           MAT(1,3) = 0.D0
!
           MAT(2,1) = 0.D0
           MAT(2,2) = DC
           MAT(2,3) = DS
!
           MAT(3,1) =  0.D0
           MAT(3,2) = -1.D0*DS
           MAT(3,3) = DC
        ELSE IF ( IAX .EQ. 2 ) THEN
           MAT(1,1) = DC
           MAT(1,2) = 0.D0
           MAT(1,3) = -1.D0*DS
!
           MAT(2,1) = 0.D0
           MAT(2,2) = 1.D0
           MAT(2,3) = 0.D0
!
           MAT(3,1) = DS
           MAT(3,2) = 0.D0
           MAT(3,3) = DC
        ELSE IF ( IAX .EQ. 3 ) THEN
           MAT(1,1) = DC
           MAT(1,2) = DS
           MAT(1,3) = 0.D0
!
           MAT(2,1) = -1.D0*DS
           MAT(2,2) = DC
           MAT(2,3) = 0.D0
!
           MAT(3,1) = 0.D0
           MAT(3,2) = 0.D0
           MAT(3,3) = 1.D0
      END IF
!
      RETURN
      END  SUBROUTINE  VTD_ROTMAT
