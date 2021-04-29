!$DPERTH
      SUBROUTINE DPERTH( XLAT,XLON,TIME,TIDE,ISDATA,MODEL,BUFFER)
!********1*********2*********3*********4*********5*********6*********7**
! DPERTH           97/08/27            9708.0    PGMR -
!
! FUNCTION:
!  Driver for PERTH2a.  Has second entry point 'prthip'.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XLAT
!   XLON
!   TIME
!   TIDE
!   ISDATA
!   MODEL
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
!
!
      LOGICAL           isdata
      character*26      filename(5)
      INTEGER           lu(5)
!
!  local varbs
!     parameter        (mg=10, mbuff=6000000)
!     parameter        (mbuff=6000000)
      DOUBLE PRECISION              latmin(5),latmax(5),lonmin(5),lonmax(5)
      LOGICAL           wrap(5)
      INTEGER           pointer(5),nx(5),ny(5)
!     dimension         buffer(mbuff)
      dimension         buffer(1)
!     save              buffer,latmin,latmax,lonmin,lonmax,wrap,pointer
!     save              nx,ny
      dimension         h12(2,26)
      parameter        (undef=99999.)
      LOGICAL           select(8),isdata0
!     save              select,xlat0,xlon0,h12
      data  select/8*.true./, xlat0,xlon0/2*999./
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!     Determine tidal constants at this location for model M
!     ------------------------------------------------------
      m = model
!     if (m.lt.1 .or. m.gt.mg) stop 333

      if (xlat.ne.xlat0 .or. xlon.ne.xlon0) then
          call grsint(buffer(pointer(m)),nx(m),ny(m),16,nx(m),ny(m),    &
     &                undef, latmin(m), latmax(m), lonmin(m), lonmax(m),&
     &                wrap(m), xlat, xlon, h12, isdata )
          xlat0 = xlat
          xlon0 = xlon
          isdata0 = isdata
!
!         infer minor tides at this location   !! See perth2a for tide n
!         ----------------------------------
          H12(1, 9) = 0.263 *H12(1,1) - 0.0252*H12(1,2)
          H12(2, 9) = 0.263 *H12(2,1) - 0.0252*H12(2,2)
          H12(1,10) = 0.297 *H12(1,1) - 0.0264*H12(1,2)
          H12(2,10) = 0.297 *H12(2,1) - 0.0264*H12(2,2)
          H12(1,11) = 0.164 *H12(1,1) + 0.0048*H12(1,2)
          H12(2,11) = 0.164 *H12(2,1) + 0.0048*H12(2,2)
          H12(1,12) = 0.0140*H12(1,2) + 0.0101*H12(1,4)
          H12(2,12) = 0.0140*H12(2,2) + 0.0101*H12(2,4)
          H12(1,13) = 0.0389*H12(1,2) + 0.0282*H12(1,4)
          H12(2,13) = 0.0389*H12(2,2) + 0.0282*H12(2,4)
          H12(1,14) = 0.0064*H12(1,2) + 0.0060*H12(1,4)
          H12(2,14) = 0.0064*H12(2,2) + 0.0060*H12(2,4)
          H12(1,15) = 0.0030*H12(1,2) + 0.0171*H12(1,4)
          H12(2,15) = 0.0030*H12(2,2) + 0.0171*H12(2,4)
          H12(1,16) =-0.0015*H12(1,2) + 0.0152*H12(1,4)
          H12(2,16) =-0.0015*H12(2,2) + 0.0152*H12(2,4)
          H12(1,17) =-0.0065*H12(1,2) + 0.0155*H12(1,4)
          H12(2,17) =-0.0065*H12(2,2) + 0.0155*H12(2,4)
          H12(1,18) =-0.0389*H12(1,2) + 0.0836*H12(1,4)
          H12(2,18) =-0.0389*H12(2,2) + 0.0836*H12(2,4)
          H12(1,19) =-0.0431*H12(1,2) + 0.0613*H12(1,4)
          H12(2,19) =-0.0431*H12(2,2) + 0.0613*H12(2,4)
          H12(1,20) = 0.264 *H12(1,5) - 0.0253*H12(1,6)
          H12(2,20) = 0.264 *H12(2,5) - 0.0253*H12(2,6)
          H12(1,21) = 0.298 *H12(1,5) - 0.0264*H12(1,6)
          H12(2,21) = 0.298 *H12(2,5) - 0.0264*H12(2,6)
          H12(1,22) = 0.165 *H12(1,5) + 0.00487*H12(1,6)
          H12(2,22) = 0.165 *H12(2,5) + 0.00487*H12(2,6)
          H12(1,23) = 0.0040*H12(1,6) + 0.0074*H12(1,7)
          H12(2,23) = 0.0040*H12(2,6) + 0.0074*H12(2,7)
          H12(1,24) = 0.0131*H12(1,6) + 0.0326*H12(1,7)
          H12(2,24) = 0.0131*H12(2,6) + 0.0326*H12(2,7)
          H12(1,25) = 0.0033*H12(1,6) + 0.0082*H12(1,7)
          H12(2,25) = 0.0033*H12(2,6) + 0.0082*H12(2,7)
          H12(1,26) = 0.0585*H12(1,7)
          H12(2,26) = 0.0585*H12(2,7)
      else
          isdata = isdata0
      endif
      if (.not.isdata) return

!
!     Call PERTH for model M
!     ----------------------
      call perth2a( time,tide,h12 )

      return

!
!     Entry for reading models and setting initial pointers
!     -----------------------------------------------------
      entry prthip(buffer,lu, filename, models )
      m = models
!     if (m.gt.mg) then
!        write(6,*) 'PRTHIP: Increase MG ',models
!        stop 214
!     endif

      k = 1
      do 500 m=1,models
!     write(6,905)filename(m),lu(m)
!905   format(a53,i2)

!         open( lu(m), file=filename(m), status='old' ) ! original
         open( lu(m), file=filename(m), status='old' ,                  &
     &         BLANK='ZERO' )

!
!        Read first few records of model, to get grid sizes
!        --------------------------------------------------
         read(lu(m),*)
         read(lu(m),*)
         read(lu(m),'(16X,I5,16X,I5)')  iy, ix
         rewind lu(m)

!        Set pointer & load model into buffer
!        ------------------------------------
         pointer(m) = k
         k = k + ix*iy*16
!        if (k-1.gt.mbuff) then
!           write(6,*) 'Buffer overflow. Increase MBUFF ',k
!           stop 411
!        endif
         call tload(buffer(pointer(m)),ix,iy,select,lu(m),undef,        &
     &               nx(m), ny(m), latmin(m), latmax(m), lonmin(m),     &
     &               lonmax(m),ii)
         if (ii.ne.8) stop 413
         dx = (lonmax(m) - lonmin(m))/REAL(nx(m) - 1)
         wrap(m) = ABS(lonmax(m) - lonmin(m) - 360.) .lt. 2.0*dx
         close( lu(m) )

  500 continue
!     write(6,*) 'DPERTH: buffer size: ',mbuff,'; ',k,' needed.'

      return
      END
