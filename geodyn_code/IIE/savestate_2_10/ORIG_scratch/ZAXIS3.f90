!$ZAXIS3
      SUBROUTINE ZAXIS3( xecf, vecf, bdir, FLAMDA )
!********1*********2*********3*********4*********5*********6*********7**
! ZAXIS3           00/00/00            0000.0    PGMR -
!
! FUNCTION:  compute attitude of magnetically orientated satellite
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   LL      I/O   A    LOGICAL DYNAMIC ARRAY
!   TPARTL  I/O   A    PARTIALS OF OBSERVATIONS W.R.T TIME
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( ONE    = 1.0D0 )
      PARAMETER ( IZERO  = 0     )
!     ....mean radius of earth
      PARAMETER ( REARTH = 6.3712D6 )
!
!C      COMMON/MAGCOF/AAA(9),GH(8,7),NMAX
!C      COMMON/XYZ/XYZ(6),RS,RSQ,ISAT,IFORCE(9)
      COMMON/MAGCOF/COEFT(8,8),DUM1,NMXGH1
!C      COMMON/XYZ/
      COMMON/PRTCTL/IPRINT
!
!C      DIMENSION AA(*),II(*),LL(*)
!
      DIMENSION xyz(3)
      DIMENSION xecf(3),vecf(3)
      DIMENSION bdir(3),COSMLA(7),SINMLA(7)
!CC      DIMENSION PP(10),P(8,8)
      DIMENSION PP(10),P(0:7,0:7)
                             ! correct??
      DIMENSION GH(0:7,0:6)
!      DIMENSION GH(8,7)
!
      EQUIVALENCE(PP(10),P(1,1))
      EQUIVALENCE(GH(1,1),COEFT(2,2))
!
      data kentry/0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      kentry = kentry + 1
!

      nmax = NMXGH1

      do 10 I=1,3
         xyz(i) = xecf(i)
   10 continue
      do 11 I=4,6
         xyz(i) = vecf(i)
   11 continue


!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: nmax ', nmax
!         write(6,*) 'zaxis3: xecf, vecf ', xecf, vecf
!         write(6,*) 'zaxis3: xyz ', xyz
!      endif

!T    XYSQ=XYZ(1)**2+XYZ(2)**2
      XYSQ=(XYZ(1)**2)+XYZ(2)**2
      RSATSQ=XYSQ+XYZ(3)**2
      RTXYSQ=SQRT(XYSQ)
      RSAT=SQRT(RSATSQ)
      RR=REARTH/RSAT
      SINLAM=SIN(FLAMDA)
      COSLAM=COS(FLAMDA)
      SINPHI=XYZ(3)/RSAT
      COSPHI=RTXYSQ/RSAT
      TANPHI=SINPHI/COSPHI
!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: rearth,rsat, rr ', rearth,rsat, rr
!         write(6,*) 'zaxis3: flamda , sinlam, coslam ',
!     &                       flamda , sinlam, coslam
!         write(6,*) 'zaxis3: sinphi, cosphi, tanphi  ',
!     &                       sinphi, cosphi, tanphi
!      endif
!
!C      CALL CLEAR(PP,8,18)   # 18 correct ?
      CALL CLEARA( PP, 10 )
      CALL CLEARA(  P, 64 )
!
      P(IZERO,IZERO)=ONE
      P(1,IZERO)=SINPHI
      P(1,1)=COSPHI
!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: p(izero, izero) ',p(izero, izero)
!         write(6,*) 'zaxis3: p(1, izero) ',p(1, izero)
!         write(6,*) 'zaxis3: p(1, 1) ',p(1, 1)
!      endif
      COSMLA(1)=COSLAM
      SINMLA(1)=SINLAM
!
!T    DO 10 M=2,NMAX
      DO 10999 M=2,MAX(2,NMAX)
         COSMLA(M)=COSLAM*COSMLA(M-1)-SINLAM*SINMLA(M-1)
         SINMLA(M)=SINLAM*COSMLA(M-1)+COSLAM*SINMLA(M-1)
10999 END DO
!
!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: cosmla ', (cosmla(iii), iii=1,nmax)
!         write(6,*) 'zaxis3: sinmla ', (sinmla(iii), iii=1,nmax)
!      endif
      FN1=0.0D0
      F2N1=1.0D0
!
!T    DO 30 N=2,NMAX
      DO 30999 N=2,MAX(2,NMAX)
         FN1=FN1+1.0D0
         F2N1=F2N1+2.0D0
         P(N,IZERO)=(F2N1*SINPHI*P(N-1,IZERO)-FN1*P(N-2,IZERO))         &
     &              /DBLE(N)
!
!T    DO 30 M=1,N
      DO 30999 M=1,MAX(1,N)
   30    P(N,M)=P(N-2,M)+F2N1*COSPHI*P(N-1,M-1)
30999 CONTINUE
!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: P  ',
!     &             ((P(iii,jjj), jjj=1,iii),iii=1,nmax)
!      endif
!
      DVR=0.0D0
      DVP=0.0D0
      DVL=0.0D0
      N1=6
      RFACN1=RR**2
      RFACN2=RFACN1*RR
!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: rr, rfacn1 ', rr, rfacn1
!         write(6,*) 'zaxis3: rfacn2 ', rfacn2
!      endif
!
!T    DO 500 N=1,NMAX
      DO 50099 N=1,MAX(1,NMAX)
!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: N, P(N,IZERO) ', N, P(N,IZERO)
!         write(6,*) 'zaxis3: N, GH(N,IZERO) ', N, GH(N,IZERO)
!         write(6,*) 'zaxis3: N, DVR, DVP    ', N, DVR, DVP
!      endif
         DVR=DVR-(N+1)*RFACN2*GH(N,IZERO)*P(N,IZERO)
         DVP=DVP+REARTH*RFACN1*GH(N,IZERO)*P(N,1)


!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3:aft N, DVR, DVP    ', N, DVR, DVP
!      endif
         DVRM=0.0D0
         DVPM=0.0D0
         DVLM=0.0D0
         M1=7
!
!T       DO 400 M=1,N
         DO 40099 M=1,MAX(1,N)
!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: N,M,P(N,M) ', N,M,P(N,M)
!         write(6,*) 'zaxis3: N,M,GH(N,M) ', N,M,GH(N,M)
!         write(6,*) 'zaxis3: N1,M1, GH(N1,M1) ', N1,M1, GH(N1,M1)
!         write(6,*) 'zaxis3: N,M,cosmla(m),sinmla(m) ',
!     &                       N,M,cosmla(m),sinmla(m)
!         write(6,*) 'zaxis3:TANPHI    ',TANPHI
!         write(6,*) 'zaxis3:aft N, DVR, DVP    ', N, DVR, DVP
!      endif
            DVRM=DVRM+(GH(N,M)*COSMLA(M)+GH(N1,M1)*SINMLA(M))*P(N,M)
            DVPM=DVPM+(GH(N,M)*COSMLA(M)+GH(N1,M1)*SINMLA(M))*(P(N,M+1)-&
     &      M*TANPHI*P(N,M))
            DVLM=DVLM+(GH(N,M)*SINMLA(M)-GH(N1,M1)*COSMLA(M))*M*P(N,M)
  400       M1=M1-1
40099    CONTINUE

!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: N, DVRM, DVPM, DVLM ',N,DVRM,DVPM,DVLM
!         write(6,*) 'zaxis3: N, DVR, DVP, DVL    ', N, DVR, DVP, DVL
!      endif
!
         DVR=DVR-(N+1)*RFACN2*DVRM
         DVP=DVP+REARTH*RFACN1*DVPM
         DVL=DVL-REARTH*RFACN1*DVLM


         RFACN1=RFACN1*RR
         RFACN2=RFACN1*RR
  500    N1=N1-1
50099 END DO
!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: dvr, dvp, dvl ',
!     &                       dvr, dvp, dvl
!      endif
!cc      write(20,'(1x,f10.2,3D20.10)') ang, dvr, dvp, dvl
!
!     ....radial partials
      DRX=XYZ(1)/RSAT
      DRY=XYZ(2)/RSAT
      DRZ=XYZ(3)/RSAT
!
!     ....latitude partials
      DPX=-XYZ(3)*XYZ(1)/(RSATSQ*RTXYSQ)
      DPY=-XYZ(3)*XYZ(2)/(RSATSQ*RTXYSQ)
      DPZ=RTXYSQ/RSATSQ
!
!     ....longitude partials
      DLX=-XYZ(2)/XYSQ
      DLY=XYZ(1)/XYSQ
      DLZ=0.0D0
!
!     ....vector of magnetic field in ECI (ECF?)
      bdir(1)=-(DVR*DRX+DVP*DPX+DVL*DLX)
      bdir(2)=-(DVR*DRY+DVP*DPY+DVL*DLY)
      bdir(3)=-(DVR*DRZ+DVP*DPZ+DVL*DLZ)
!
!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: drx, dry, drz ',
!     &                       drx, dry, drz
!         write(6,*) 'zaxis3: dpx, dpy, dpz ',
!     &                       dpx, dpy, dpz
!         write(6,*) 'zaxis3: dlx, dly, dlz ',
!     &                       dlx, dly, dlz
!         write(6,*) 'zaxis3: bdir ', bdir
!      endif

!     ....make bdir a unit vector

      zmag = SQRT( bdir(1)**2 + bdir(2)**2 + bdir(3)**2 )
!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: zmag ', zmag
!      endif
      bdir(1)= bdir(1) / zmag
      bdir(2)= bdir(2) / zmag
      bdir(3)= bdir(3) / zmag
!      if( kentry .le. iprint .and. iprint.gt.0 ) then
!         write(6,*) 'zaxis3: zmag ', zmag
!         write(6,*) 'zaxis3: bdir ', bdir
!      endif
!
      RETURN
      END
