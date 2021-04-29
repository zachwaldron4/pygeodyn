!$GETPX
      SUBROUTINE GETPX(px,kmat,D,mmfl,CTR, vecin)
    ! Input:
    !   kmat: Defines the camera CCD sensor (pixel size and orientation)
    !   D: Camera distortion
    !   mmfl: Focal length in millimeters
    !   CTR: Coordinates of center pixel
    !   vecin: unit vector in direction of pixel
    ! Output:
    !   px: Pixel coordinates

      implicit none
      DOUBLE PRECISION , intent(in) ::  kmat(6), D(4), mmfl, CTR(2),vecin(3)
      DOUBLE PRECISION , intent(out) :: px(2)

      INTEGER :: ii
          LOGICAL :: lconverged
          DOUBLE PRECISION :: dtmp, rsq, xtmp, xtmp1, xtmp2, xtmp3, ytmp
          DOUBLE PRECISION :: mm(2), px0(2), z(2), ztmp(2)

      DOUBLE PRECISION :: conv_tol = 1.0d-8 ! convergence tolerance

    ! kmat=reshape(kmat,3,2)' in the Matlab version of this code

!!    mm(1) = 0.0d0
!!    mm(2) = 0.0d0
      px0(1:2) = CTR(1:2)
      lconverged = .false.

      do ii = 1, 20
!!        mm(1) = mm(1) + (px(1)-px0(1))/kmat(1)
!!        mm(2) = mm(2) + (px(2)-px0(2))/kmat(5)
!!        vecin(1) = mm(1)
!!        vecin(2) = mm(2)
!!        vecin(3) = mmfl
!
! getmm
        ztmp(1) = (1.0D0+D(1)) * mmfl * vecin(1)/vecin(3)
        ztmp(2) = (1.0D0+D(1)) * mmfl * vecin(2)/vecin(3)
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

        px(1) = CTR(1) + kmat(1)*xtmp + kmat(2)*ytmp &
                + kmat(3)*xtmp*ytmp
        px(2) = CTR(2) + kmat(4)*xtmp + kmat(5)*ytmp &
                + kmat(6)*xtmp*ytmp

        ! Check for convergence
        dtmp = SQRT(sum((px(1:2)-px0(1:2))**2))
        if (dtmp < conv_tol) then
            lconverged = .true.
            exit
        end if
        px0(1)=px(1)
        px0(2)=px(2)
      end do

      if (.not. lconverged) then
        write(*,'(A)') 'ERROR IN GETPX: DID NOT CONVERGE.'
        stop
      end if

      end subroutine
