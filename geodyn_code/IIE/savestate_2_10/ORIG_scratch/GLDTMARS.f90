      SUBROUTINE GLDTMARS(fbar,day_m,ac,da,gdel,ff0,xls)

!********1*********2*********3*********4*********5*********6*********7**
!
! GLDTMARS           08/23/13            0000.0    PGMR - ?
!
!
! FUNCTION: COMPUTE FUNCTION G(l) DTM-Mars (Bruinsma and Lemoine, 2001).
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   fbar    I          SOLAR MEAN FLUX
!   day_m   I          MARTIAN DAY-OF-YEAR
!   ac      I          MODEL COEFFICIENT MATRIX FOR TEMPERATURE AND
!                      CONSTITUENTS
!   da      I          MATRIX OF PARTIAL DERIVATIVES dG(l)/da
!   ff0     I          1 FOR CO2, OXYGEN, NITROGEN, HELIUM, TEMPERATURE,
!                      0 FOR HYDROGEN
!   xls     I          AREOCENTRIC SOLAR LONGITUDE, TO COMPUTE EFFECT
!                      DUST STORM
!   gdel    O          RESULT OF G(l)
!
!********1*********2*********3*********4*********5*********6*********7**

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)

!***********************************************************************
!  COMMON PARAMETERS INTO DTM SUBROUTINES*******************************
!***********************************************************************

      INCLUDE 'COMMON_DECL.inc'
      COMMON/PARDTM/tau_atm,taubg,xlsdeb,xlsfin
      COMMON/HLOCAL/c2h,c3h,ch,hl0,s2h,s3h,sh
      COMMON/PLGDTM/p10_atm,p10mg,p11_atm,p20_atm,p20mg,p21_atm,p22_atm,&
     &              p30_atm,p31_atm,p32_atm,p33_atm,p40_atm,p40mg,p41, &
     &              p42,p50_atm,p51_atm,p52,p60,p62

!      DOUBLE PRECISION, DIMENSION(96) :: az,az2,h,he,o,o2,t0_dtm,tp, &
!     &                                   tt,t0_atm,co2,co,ar,h2

!***********************************************************************
!  LOCAL PARAMETERS*****************************************************
!***********************************************************************

      DOUBLE PRECISION, INTENT(IN) :: day_m
      DOUBLE PRECISION, INTENT(IN) :: ff0
      DOUBLE PRECISION, INTENT(IN) :: xls
      DOUBLE PRECISION, DIMENSION(74), INTENT(IN) :: ac
      DOUBLE PRECISION, DIMENSION(74), INTENT(INOUT) :: da
      DOUBLE PRECISION, INTENT(OUT) :: gdel

      INTEGER :: i,ipow
      DOUBLE PRECISION :: c2fi,clfl,cos2te,coste,dakp,dakpm,dkp,dkpm,f0,&
     &                    f1f,fp0,fpp,rsin2te,rsinte,slfl
      DOUBLE PRECISION :: fbm150
      DOUBLE PRECISION :: dells,dectau,deuxse,correc,amr,taumax,xmult,  &
     &                    denom,fpds,q,qmax,xlsmaxd,xlsmaxf

      DOUBLE PRECISION, PARAMETER :: rot = .009397525D0
      DOUBLE PRECISION, PARAMETER :: rot2 =.018795051D0

!********1*********2*********3*********4*********5*********6*********7**
!  DUST STORM PARAMETERIZATION
!********1*********2*********3*********4*********5*********6*********7**

      dells  =15.D0                                                     !
      xlsmaxd=xlsdeb+6.D0                                               !
      dectau =12.D0                                                     !
      xlsmaxf=xlsfin-dectau                                             !
      deuxse =2.D0/2.71828D0
      correc =deuxse**dectau
      amr    =0.D0

!********1*********2*********3*********4*********5*********6*********7**
!  LATITUDE (NP)
!********1*********2*********3*********4*********5*********6*********7**

      da(2) =p20_atm
      da(3) =p30_atm
      da(4) =p40_atm
      da(59)=p60
      da(60)=p10_atm
      da(61)=p50_atm

!********1*********2*********3*********4*********5*********6*********7**
!  SOLAR FLUX (NP-PA)
!********1*********2*********3*********4*********5*********6*********7**

      fbm150=fbar-65.D0                                                 !
      da(5)=fbm150
      da(40)=da(5)*da(5)
      f0=ac(5)*da(5)+ac(40)*da(40)
      f1f=1.D0+f0*ff0                                                   !
      f0=f0+ac(2)*da(2)+ac(3)*da(3)+ac(4)*da(4)                         &
     &     +ac(59)*da(59)+ac(60)*da(60)+ac(61)*da(61)

!********1*********2*********3*********4*********5*********6*********7**
!  SYMMETRICAL ANNUAL (PA)
!********1*********2*********3*********4*********5*********6*********7**

      da(6)=COS(rot*day_m)
      da(7)=SIN(rot*day_m)
      da(8)=da(5)*COS(rot*day_m)
      da(9)=da(5)*SIN(rot*day_m)
      da(10)=p20_atm*da(6)
      da(11)=p20_atm*da(7)

!********1*********2*********3*********4*********5*********6*********7**
!  SYMMETRICAL SEMIANNUAL (PSA)
!********1*********2*********3*********4*********5*********6*********7**

      da(14)=COS(rot2*day_m)
      da(15)=SIN(rot2*day_m)
      da(16)=da(5)*COS(rot2*day_m)
      da(17)=da(5)*SIN(rot2*day_m)

!********1*********2*********3*********4*********5*********6*********7**
!  SEASONAL (PA-PSA)
!********1*********2*********3*********4*********5*********6*********7**

      da(12)=p10_atm*da(6)
      da(13)=p10_atm*da(7)
      da(18)=p30_atm*da(6)
      da(19)=p30_atm*da(7)
      da(62)=da(12)*da(5)
      da(63)=da(13)*da(5)
      da(64)=p50_atm*da(6)
      da(65)=p50_atm*da(7)
      da(20)=p10_atm*da(14)
      da(21)=p10_atm*da(15)
      da(66)=p20_atm*da(14)
      da(67)=p20_atm*da(15)
      da(68)=p30_atm*da(14)
      da(69)=p30_atm*da(15)

!********1*********2*********3*********4*********5*********6*********7**
!  DIURNAL (PD)
!********1*********2*********3*********4*********5*********6*********7**

      da(22)=p11_atm*ch
      da(23)=p11_atm*sh
      da(24)=p21_atm*ch
      da(25)=p21_atm*sh
      da(26)=p31_atm*ch
      da(27)=p31_atm*sh
      da(28)=da(5)*da(22)
      da(29)=da(5)*da(23)

!********1*********2*********3*********4*********5*********6*********7**
!  SEMI-DIURNAL (PSD)
!********1*********2*********3*********4*********5*********6*********7**

      da(31)=p22_atm*c2h
      da(32)=p22_atm*s2h
      da(33)=p32_atm*c2h
      da(34)=p32_atm*s2h
      da(35)=p42*c2h
      da(36)=p42*s2h
      da(37)=da(31)*da(5)
      da(38)=da(32)*da(5)

!********1*********2*********3*********4*********5*********6*********7**
!  DUST STORM (NP)
!********1*********2*********3*********4*********5*********6*********7**

      IF(xls.ge.xlsdeb .and. xlsdeb.ge.180.D0) THEN                     !
        IF(xls.lt.xlsmaxd) THEN
          q=EXP(-(xls-xlsdeb)/dells)
          da(41)= q*(1.D0-q**4.D0)*tau_atm
        END IF
        IF(xls.ge.xlsmaxd .and. xls.lt.xlsmaxf) THEN
          q=EXP(-(xlsmaxd-xls)/dells)
          da(41)= q*(1.D0-q**4.D0)*tau_atm
        END IF
        IF(xls.ge.xlsmaxf .and. xls.lt.xlsfin) THEN
          qmax=EXP(-(xlsmaxd-xls)/dells)
          taumax=qmax*(1.D0-qmax**4.D0)*tau_atm
          xmult=taumax/deuxse
          ipow=INT(xls-xlsmaxf)+1.D0
          denom=deuxse**ipow
          da(41)=xmult*(denom-correc)
        END IF
      END IF

!********1*********2*********3*********4*********5*********6*********7**
!  BACKGROUND DUST LEVEL
!********1*********2*********3*********4*********5*********6*********7**


      da(70)=taubg

      fpds=ac(41)*da(41)+ac(70)*da(70)
!       write(6,*)'fpds,a41,da41,a70,da70'
!       write(6,*)fpds,ac(41),da(41),ac(70),da(70)
      fp0 =0.D0

!********1*********2*********3*********4*********5*********6*********7**
!  PERIODIC TERMS
!********1*********2*********3*********4*********5*********6*********7**

      fpp=ac(6)*da(6)+ac(7)*da(7)+ac(8)*da(8)+ac(9)*da(9)+ac(10)*da(10)+&
          ac(11)*da(11)+ac(14)*da(14)+ac(15)*da(15)+ac(16)*da(16)+ &
          ac(17)*da(17)+ac(12)*da(12)+ac(13)*da(13)+ac(18)*da(18)+ &
          ac(19)*da(19)+ac(62)*da(62)+ac(63)*da(63)+ac(64)*da(64)+ &
          ac(65)*da(65)+ac(20)*da(20)+ac(21)*da(21)+ac(66)*da(66)+ &
          ac(67)*da(67)+ac(68)*da(68)+ac(69)*da(69)+ac(31)*da(31)+ &
          ac(32)*da(32)+ac(33)*da(33)+ac(34)*da(34)+ac(35)*da(35)+ &
          ac(36)*da(36)+ac(37)*da(37)+ac(38)*da(38)+ac(22)*da(22)+ &
          ac(23)*da(23)+ac(24)*da(24)+ac(25)*da(25)+ac(26)*da(26)+ &
          ac(27)*da(27)+ac(28)*da(28)+ac(29)*da(29)

!********1*********2*********3*********4*********5*********6*********7**
!  RESULT FUNCTION G(l)
!********1*********2*********3*********4*********5*********6*********7**

      gdel=f0+fpp+fp0*f1f+fpds


      END
