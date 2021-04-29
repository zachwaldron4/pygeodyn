      subroutine getvec(px,kmat,D,mmfl,CTR, vecout)
! Input:
!   px: Pixel coordinates
!   kmat: Defines the camera CCD sensor (pixel size and orientation)
!   D: Camera distortion
!   mmfl: Focal length in millimeters
!   CTR: Coordinates of center pixel
! Output:
!   vecout: unit vector in direction of pixel

      implicit none
!     real*8 intent(in) :: px(2), kmat(6), D(4), mmfl, CTR(2)
!     real*8  intent(out) :: vecout(3)
      DOUBLE PRECISION px(2), kmat(6), D(4), mmfl, CTR(2)
      DOUBLE PRECISION vecout(3)

      INTEGER  ii
      LOGICAL  lconverged
      DOUBLE PRECISION   dtmp, rsq, xtmp, xtmp1, xtmp2, xtmp3, ytmp
      DOUBLE PRECISION mm(2), px0(2), z(2), ztmp(2)
      DOUBLE PRECISION VQ1,VQ2,VQ3
      DOUBLE PRECISION V1(3),V1H(3),V2H(3),ANG

!     real*8 conv_tol = 1.0d-8 ! convergence tolerance
      DOUBLE PRECISION conv_tol
      data conv_tol/1.0D-8/

! kmat=reshape(kmat,3,2)' in the Matlab version of this code

      mm(1) = 0.0D0
      mm(2) = 0.0D0
      px0(1:2) = CTR(1:2)
      lconverged = .false.

      do ii = 1, 20
        mm(1) = mm(1) + (px(1)-px0(1))/kmat(1)
        mm(2) = mm(2) + (px(2)-px0(2))/kmat(5)
        vecout(1) = mm(1)
        vecout(2) = mm(2)
        vecout(3) = mmfl

! getmm
        ztmp(1) = (1.0D0+D(1)) * mmfl * vecout(1)/vecout(3)
        ztmp(2) = (1.0D0+D(1)) * mmfl * vecout(2)/vecout(3)
        xtmp1 = ztmp(1)**2 + ztmp(2)**2
        xtmp2 = xtmp1**2
        xtmp3 = xtmp1 * xtmp2
        z(1) = ztmp(1)*(1.0D0+D(2)*xtmp1+D(3)*xtmp2+D(4)*xtmp3)
        z(2) = ztmp(2)*(1.0D0+D(2)*xtmp1+D(3)*xtmp2+D(4)*xtmp3)

! MMPX
        xtmp = z(1) * (1.0D0+D(1))
        ytmp = z(2) * (1.0D0+D(1))

        rsq = xtmp**2 + ytmp**2
        ztmp(1) = 1.0D0 + D(2)*rsq + D(3)*ytmp + D(4)*xtmp
        xtmp = xtmp * ztmp(1)
        ytmp = ytmp * ztmp(1)

        px0(1) = CTR(1) + kmat(1)*xtmp + kmat(2)*ytmp &
                + kmat(3)*xtmp*ytmp
        px0(2) = CTR(2) + kmat(4)*xtmp + kmat(5)*ytmp &
                + kmat(6)*xtmp*ytmp

! Check for convergence
        dtmp = SQRT(sum((px(1:2)-px0(1:2))**2))
        if (dtmp < conv_tol) then
            lconverged = .true.
            exit
        end if
      end do

      if (.not. lconverged) then
        write(*,'(A)') 'ERROR IN GETVEC: DID NOT CONVERGE.'
        stop
      end if

      vecout(1:3) = vecout(1:3) / SQRT(sum(vecout(1:3)**2))
!    VQ1=VECOUT(3)
!    VQ2=-VECOUT(2)
!    VQ3=VECOUT(1)
!    VECOUT(1)=VQ1
!    VECOUT(2)=VQ2
!    VECOUT(3)=VQ3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      V1(1)=VECOUT(1)
!      V1(2)=VECOUT(2)
!      V1(3)=VECOUT(3)
!      ANG=(180.D0-0.0277221)*DACOS(-1.D0)/180.D0
!      V1H(1)=V1(1)*DCOS(ANG)+V1(2)*DSIN(ANG)
!      V1H(2)=-V1(1)*DSIN(ANG)+V1(2)*DCOS(ANG)
!      V1H(3)=V1(3)
!      ANG=89.917365D0*DACOS(-1.D0)/180.D0
!      V2H(1)=V1H(1)*DCOS(ANG)+V1H(3)*DSIN(ANG)
!      V2H(2)=V1H(2)
!      V2H(3)=-V1H(1)*DSIN(ANG)+V1H(3)*DCOS(ANG)
!      VECOUT(1)=V2H(1)
!      VECOUT(2)=V2H(2)
!      VECOUT(3)=V2H(3)


       end subroutine getvec
