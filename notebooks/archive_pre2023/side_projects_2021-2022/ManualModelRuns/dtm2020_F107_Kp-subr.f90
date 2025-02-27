!***********************************************************************
subroutine dtm3(day,f,fbar,akp,alti,hl,alat,xlon,tz,tinf,ro,d,wmm)
  !***********************************************************************
  ! CNES DTM2020 operational: F10.7 and Kp       
  ! ver 21/10/2020
  ! calculation of temperature and density with DTM2020_Oper
  !  
  !*par ** INPUT **
  !     day = day of year [1-366]
  !     f   = f(1)=instantaneous flux at (t - 24hr)    /   f(2)=0.
  !     fbar= fbar(1)=mean flux of last 81 days at t   /   fbar(2)=0.
  !     akp = akp(1)= kp delayed by 3 hours, akp(3)=mean of last 24 hours
  !           akp(2) & akp(4)=0.
  !     alti= altitude (in km) greater than 120 km
  !     hl  = local time (in radian: 0-24hr = 0-2pi)
  !     alat= latitude (in radian)
  !     xlon= longitude (in radian)
  !
  !*par ** OUTPUT **
  !     tz      = temperature at altitude -> alti
  !     tinf    = exospheric temperature
  !     d(1)    = partial density of atomic hydrogen (in gram/cm3)
  !     d(2)    = partial density of helium
  !     d(3)    = partial density of atomic oxygen
  !     d(4)    = partial density of molecular nitrogen
  !     d(5)    = partial density of molecular oxygen
  !     d(6)    = partial density of atomic nitrogen
  !     ro      = total density (in gram/cm3)
  !     wmm     = mean molecular mass (in gram)
  !***********************************************************************
  !
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nlatm = 96
  ! 
  !.. Formal Arguments .. 
  real, intent(in) :: alat,alti,day,hl,xlon
  real, intent(out) :: ro,tinf,tz,wmm
  real, dimension(2), intent(in) :: f,fbar
  real, dimension(4), intent(in) :: akp
  real, dimension(6), intent(out) :: d
  ! 
  !.. Local Scalars .. 
  integer :: i,n
  integer :: ityp = 0
  real :: c,c2,c4,s,s2,dt120,dtinf,clmlmg,sp,cmg,cmg2,cmg4, &
          dtp120,gamma,gdelaz,gdelh,gdelo,gdelt0,gdeltp,tinftz,ts,zeta
  real :: cpmg = .19081,re = 6356.77,rgas = 831.4
  real :: dzeta,dzeta2,expsz,gdelaz2,gdelhe,gdelo2,gdelt,glb,secl2,sigma, &
          sigzeta,t,t120sd,t120tt,t120tz,upapg,zlb
  real :: zlb0 = 120.,xlmg = -1.2392
  real :: t120 = 0.0,tp120 = 0.0,xlog = 0.0
  real :: cose = .9175,gsurf = 980.665,sine = .3978,spmg = .98163,zero = 0.
  real :: crd,dlat,dlon,gmlatd,gmlond,gmlatr,gmlonr
  ! 
  !.. Local Arrays .. 
  integer, dimension(6) :: ma
  real, dimension(6) :: alefa
  real, dimension(6) :: cc = 0.0,dbase = 0.0,fz = 0.0 
  real, dimension(6) :: vma
  real, dimension(nlatm) :: daz,daz2,dh,dhe,do2,dox,dt0,dtp,dtt
  !
  !.. Common Blocks .. 
  common /cons/ pi,deupi,cdr,sard
  real :: cdr,deupi,pi,sard
  common /hlocal/ hl0,ch,sh,c2h,s2h,c3h,s3h
  real :: c2h,c3h,ch,hl0,s2h,s3h,sh
  common /pardtm/ tt,h,he,o,az2,o2,az,t0,tp
  real, dimension(nlatm) :: az,az2,h,he,o,o2,t0,tp,tt
  common /plgdtm/ p10,p20,p30,p40,p50,p60,p11,p21,p31,p41,p51,p22,p32,p42,p52, &
                  p62,p33,p10mg,p20mg,p40mg,p11mg,p22mg,p31mg,p50mg,p60mg
  real :: p10,p10mg,p11,p20,p20mg,p21,p22,p30,p31,p32,p33,p40,p40mg,p41,p42, &
          p50,p51,p52,p60,p62,p11mg,p22mg,p31mg,p50mg,p60mg
  ! 
  !.. Data Declarations .. 
  data alefa/-0.40,-0.38,0.,0.,0.,0./
  data ma/1,4,16,28,32,14/
  data vma/1.6606e-24,6.6423e-24,26.569e-24,46.4958e-24,53.1381e-24,23.2479e-24/
  ! 
  ! ... Executable Statements ...
  ! 
  zlb = zlb0 
  !
  ro = 0.
  dtinf = 0.
  dt120 = 0.
  dtp120 = 0.
  fz(1) = 0.
  fz(2) = 0.
  fz(3) = 0.
  fz(4) = 0.
  fz(5) = 0.
  fz(6) = 0.
  !
  do i = 1,nlatm
    dtt(i) = 0.
    dh(i) = 0.
    dhe(i) = 0.
    dox(i) = 0.
    daz2(i) = 0.
    do2(i) = 0.
    daz(i) = 0.
    dt0(i) = 0.
    dtp(i) = 0.
  end do
  !
  c = sin(alat)
  c2 = c * c
  c4 = c2 * c2
  s = cos(alat)
  s2 = s * s
  p10 = c
  p20 = 1.5*c2 - 0.5
  p30 = c * (2.5*c2-1.5)
  p40 = 4.375*c4 - 3.75*c2 + 0.375
  p50 = c * (7.875*c4-8.75*c2+1.875)
  p60 = (5.5*c*p50-2.5*p40) / 3.
  p11 = s
  p21 = 3. * c * s
  p31 = s * (7.5*c2-1.5)
  p41 = c * s * (17.5*c2-7.5)
  p51 = s * (39.375*c4-26.25*c2+1.875)
  p22 = 3. * s2
  p32 = 15. * c * s2
  p42 = s2 * (52.5*c2-7.5)
  p52 = 3.*c*p42 - 2.*p32
  p62 = 2.75*c*p52 - 1.75*p42
  p33 = 15. * s * s2
  !
    clmlmg = cos(xlon-xlmg)
    sp = s*cpmg*clmlmg + c*spmg
    !
    cmg = sp ! pole magnetique
    cmg2 = cmg * cmg
    cmg4 = cmg2 * cmg2
    p10mg = cmg
    p20mg = 1.5*cmg2 - 0.5
    p40mg = 4.375*cmg4 - 3.75*cmg2 + 0.375
  !
  hl0 = hl
  ch = cos(hl0)
  sh = sin(hl0)
  c2h = ch*ch - sh*sh
  s2h = 2. * ch * sh
  c3h = c2h*ch - s2h*sh
  s3h = s2h*ch + c2h*sh
  !
  call gldtm(f,fbar,akp,day,tt,dtt,gdelt,1.,xlon)
  dtt(1) = 1. + gdelt
  tinf = tt(1) * dtt(1)
  call gldtm(f,fbar,akp,day,t0,dt0,gdelt0,1.,xlon)
  dt0(1) = 1. + gdelt0
  t120 = t0(1) * dt0(1)
  call gldtm(f,fbar,akp,day,tp,dtp,gdeltp,1.,xlon)
  dtp(1) = 1. + gdeltp
  tp120 = tp(1) * dtp(1)  
  !-----------------------------------------------------------------------------
  sigma = tp120 / (tinf-t120)
  dzeta = (re+zlb) / (re+alti)
  zeta = (alti-zlb) * dzeta
  dzeta2 = dzeta * dzeta
  sigzeta = sigma * zeta
  expsz = exp(-sigzeta)
  tz = tinf - (tinf-t120)*expsz
  !
    call gldtm(f,fbar,akp,day,h,dh,gdelh,0.,xlon)
    dh(1)=exp(gdelh)
    dbase(1)=h(1)*dh(1)
!
    call gldtm(f,fbar,akp,day,he,dhe,gdelhe,0.,xlon)
    dhe(1)=exp(gdelhe)
    dbase(2)=he(1)*dhe(1)
!
    call gldtm(f,fbar,akp,day,o,dox,gdelo,1.,xlon)
    dox(1)=exp(gdelo)
    dbase(3)=o(1)*dox(1)
!
    call gldtm(f,fbar,akp,day,az2,daz2,gdelaz2,1.,xlon)
    daz2(1)=exp(gdelaz2)
    dbase(4)=az2(1)*daz2(1)
!
    call gldtm(f,fbar,akp,day,o2,do2,gdelo2,1.,xlon)
    do2(1)=exp(gdelo2)
    dbase(5)=o2(1)*do2(1)
!
    call gldtm(f,fbar,akp,day,az,daz,gdelaz,1.,xlon)
    daz(1)=exp(gdelaz)
    dbase(6)=az(1)*daz(1)
    !
    glb = gsurf / (1.+zlb/re)**2
    glb = glb / (sigma*rgas*tinf)
    t120tz = t120 / tz
    xlog = LOG(t120tz)
    tinftz = tinf / tz
    t120tt = t120 / (tinf-t120)
!
    do i = 1,6
      gamma = ma(i) * glb
      upapg = 1. + alefa(i) + gamma
      fz(i) = t120tz**upapg * exp(-sigzeta*gamma)
      cc(i) = dbase(i) * fz(i)
      d(i) = cc(i) * vma(i)
      ro = ro + d(i)
    end do 
      wmm=ro/(vma(1)*(cc(1)+cc(2)+cc(3)+cc(4)+cc(5)+cc(6)))
!
end subroutine dtm3
!
!***********************************************************************
subroutine gldtm(f,fbar,akp,day,a,da,gdel,ff0,xlon)
  !***********************************************************************
  ! SB 21/10/2020
  ! Corrections et nouveaux inconnus
  !*rol calcul de la fonction g(l) evoluee pour dtm_2013 & 2020_Oper
  !
  !     gdel=resultat du calcul de g(l)
  !***********************************************************************
  !
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nlatm = 96
  ! 
  !.. Formal Arguments .. 
  real, intent(in) :: day,ff0,xlon
  real, intent(out) :: gdel
  real, dimension(2), intent(in) :: f,fbar
  real, dimension(4), intent(in) :: akp
  real, dimension(nlatm), intent(in) :: a
  real, dimension(nlatm), intent(inout) :: da
  ! 
  !.. Local Scalars .. 
  integer :: i,ikpm
  integer :: ikp = 0
  real :: a74,a77,a78,a88,f1f,fp
  real :: c2fi = 0.0
  real :: clfl,cos2te,coste,dakp,dakpm,dkp,dkpm,f0,fp1, &
          rsin2te,slfl,rsinte
  real :: rot = .017214206,rot2 = .034428412,roth = .261799387, &
          rots = 7.27220e-05
  ! 
  !.. Local Arrays .. 
  real, dimension(2) :: fbm150 = 0.0,fmfb = 0.0
  ! 
  common /hlocal/ hl,ch,sh,c2h,s2h,c3h,s3h
  real :: c2h,c3h,ch,hl,s2h,s3h,sh
  common /plgdtm/ p10,p20,p30,p40,p50,p60,p11,p21,p31,p41,p51,p22,p32,p42,p52, &
                  p62,p33,p10mg,p20mg,p40mg,p11mg,p22mg,p31mg,p50mg,p60mg
  real :: p10,p10mg,p11,p20,p20mg,p21,p22,p30,p31,p32,p33,p40,p40mg,p41,p42, &
          p50,p51,p52,p60,p62,p11mg,p22mg,p31mg,p50mg,p60mg
  ! 
  da(2) = p20
  da(3) = p40
  da(74) = p10
  da(77) = p30
  da(78) = p50
  da(79) = p60
  fmfb(1) = f(1) - fbar(1)
  fmfb(2) = f(2) - fbar(2)
  fbm150(1) = fbar(1) - 150.
  fbm150(2) = fbar(2)
  da(4) = fmfb(1)
  da(6) = fbm150(1)
  da(5) = da(4) * da(4)
  da(69) = da(6) * da(6)
  da(82) = da(4) * p10
  da(83) = da(4) * p20
  da(84) = da(4) * p30
  da(85) = da(6) * p20
  da(86) = da(6) * p30
  da(87) = da(6) * p40
    ikp = 62
    ikpm = 67
    c2fi = 1. - p10mg*p10mg
    dkp = akp(1) + (a(ikp)+c2fi*a(ikp+1))*akp(2)
    dakp = a(7) + a(8)*p20mg + a(68)*p40mg + &
           2.*dkp*(a(60)+a(61)*p20mg+a(75)*2.*dkp*dkp)
    da(ikp) = dakp * akp(2)
    da(ikp+1) = da(ikp) * c2fi
    dkpm = akp(3) + a(ikpm)*akp(4)
    dakpm = a(64) + a(65)*p20mg + a(72)*p40mg + &
            2.*dkpm*(a(66)+a(73)*p20mg+a(76)*2.*dkpm*dkpm)
    da(ikpm) = dakpm * akp(4)
  da(7) = dkp
  da(8) = p20mg * dkp
  da(68) = p40mg * dkp
  da(60) = dkp * dkp
  da(61) = p20mg * da(60)
  da(75) = da(60) * da(60)
  da(64) = dkpm
  da(65) = p20mg * dkpm
  da(72) = p40mg * dkpm
  da(66) = dkpm * dkpm
  da(73) = p20mg * da(66)
  da(76) = da(66) * da(66)
  f0 = a(4)*da(4) + a(5)*da(5) + a(6)*da(6) + a(69)*da(69) + a(82)*da(82) + &
       a(83)*da(83) + a(84)*da(84) + a(85)*da(85) + a(86)*da(86) + &
       a(87)*da(87)
  f1f = 1. + f0*ff0
  ! 
  f0 = f0 + a(2)*da(2) + a(3)*da(3) + a(74)*da(74) + a(77)*da(77) + a(7)*da(7) + &
       a(8)*da(8) + a(60)*da(60) + a(61)*da(61) + a(68)*da(68) + a(64)*da(64) &
       + a(65)*da(65) + a(66)*da(66) + a(72)*da(72) + a(73)*da(73) + &
       a(75)*da(75) + a(76)*da(76) + a(78)*da(78) + a(79)*da(79)
  da(9) = cos(rot*(day-a(11)))
  da(10) = p20 * da(9)
  da(12) = cos(rot2*(day-a(14)))
  da(13) = p20 * da(12)
  coste = cos(rot*(day-a(18)))
  da(15) = p10 * coste
  da(16) = p30 * coste
  da(17) = da(6) * da(15)
  cos2te = cos(rot2*(day-a(20)))
  da(19) = p10 * cos2te
  da(39) = p30 * cos2te
  da(59) = da(6) * da(19)
  da(21) = p11 * ch
  da(22) = p31 * ch
  da(23) = da(6) * da(21) 
  da(24) = da(21) * coste
  da(25) = p21 * ch * coste
  da(26) = p11 * sh
  da(27) = p31 * sh
  da(28) = da(6) * da(26)
  da(29) = da(26) * coste
  da(30) = p21 * sh * coste
  da(94)=p51*ch  
  da(95)=p51*sh  
  da(31) = p22 * c2h
  da(37) = p42 * c2h
  da(32) = p32 * c2h * coste
  da(33) = p22 * s2h
  da(38) = p42 * s2h
  da(34) = p32 * s2h * coste
  da(88) = p32 * c2h 
  da(89) = p32 * s2h
  da(90) = da(6) * da(31)
  da(91) = da(6) * da(33)
  da(92) = p62 * c2h
  da(93) = p62 * s2h
  da(35) = p33 * c3h
  da(36) = p33 * s3h
  fp = a(9)*da(9) + a(10)*da(10) + a(12)*da(12) + a(13)*da(13) + a(15)*da(15) &
       + a(16)*da(16) + a(17)*da(17) + a(19)*da(19) + a(21)*da(21) + &
       a(22)*da(22) + a(23)*da(23) + a(24)*da(24) + a(25)*da(25) + &
       a(26)*da(26) + a(27)*da(27) + a(28)*da(28) + a(29)*da(29) + &
       a(30)*da(30) + a(31)*da(31) + a(32)*da(32) + a(33)*da(33) + &
       a(34)*da(34) + a(35)*da(35) + a(36)*da(36) + a(37)*da(37) + &
       a(38)*da(38) + a(39)*da(39) + a(59)*da(59) + a(88)*da(88) + a(89)*da(89) + &
       a(90)*da(90) + a(91)*da(91) + a(92)*da(92) + a(93)*da(93) + &
       a(94)*da(94) + a(95)*da(95) 
  !
  da(40) = p10 * coste * dkp
  da(41) = p30 * coste * dkp
  da(42) = p50 * coste * dkp
  da(43) = p11 * ch * dkp
  da(44) = p31 * ch * dkp
  da(45) = p51 * ch * dkp
  da(46) = p11 * sh * dkp
  da(47) = p31 * sh * dkp
  da(48) = p51 * sh * dkp
  !
  fp = fp + a(40)*da(40) + a(41)*da(41) + a(42)*da(42) + a(43)*da(43) + &
       a(44)*da(44) + a(45)*da(45) + a(46)*da(46) + a(47)*da(47) + &
       a(48)*da(48)
  !
    dakp = (a(40)*p10+a(41)*p30+a(42)*p50)*coste + &
           (a(43)*p11+a(44)*p31+a(45)*p51)*ch + &
           (a(46)*p11+a(47)*p31+a(48)*p51)*sh
    da(ikp) = da(ikp) + dakp*akp(2)
    da(ikp+1) = da(ikp) + dakp*c2fi*akp(2)
    clfl = cos(xlon)
    da(49) = p11 * clfl
    da(50) = p21 * clfl
    da(51) = p31 * clfl
    da(52) = p41 * clfl
    da(53) = p51 * clfl
    slfl = sin(xlon)
    da(54) = p11 * slfl
    da(55) = p21 * slfl
    da(56) = p31 * slfl
    da(57) = p41 * slfl
    da(58) = p51 * slfl
    !
    fp = fp + a(49)*da(49) + a(50)*da(50) + a(51)*da(51) + a(52)*da(52) + &
         a(53)*da(53) + a(54)*da(54) + a(55)*da(55) + a(56)*da(56) + &
         a(57)*da(57) + a(58)*da(58)
  !
  gdel = f0 + fp*f1f
  !
end subroutine gldtm
!
!
!***********************************************************************
subroutine lecdtm
  ! SB 21/10/2020
  !
  !reading in the DTM model coefficients in vectors, per constituent
!************************************************************************
  !.. Parameters .. 
  integer, parameter :: nlatm = 96
  ! 
  !
  character titre*100,ct120*8,ctp120*8
  common /pardtm/ tt,h,he,o,az2,o2,az,t0,tp
  real, dimension(nlatm) :: az,az2,h,he,o,o2,t0,tp,tt
  integer :: npdtm,i,ni
  !
open(10, &
& file='/data/zach_work/side_projects/ManualModelRuns/DTM_2020_F107_Kp.dat', &
& status='old') 
  !
  read(10,530) titre
  read(10,540) npdtm
  write(*,600) titre
  !
  do i=1,npdtm
    read(10,540,end=20) ni,tt(i),dtt,h(i),dh,he(i),dhe,o(i),dox, &
            az2(i),daz2,o2(i),do2,az(i),daz,t0(i),dt0,tp(i),dtp
  end do
!
20 continue
!
530  format(a100)
540  format(i4,9(e13.6,e9.2))
600  format(a100/)
!
close (146)
end subroutine lecdtm 
!