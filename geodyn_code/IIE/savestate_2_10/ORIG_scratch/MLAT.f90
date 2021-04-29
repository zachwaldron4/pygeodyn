!$MLAT
      SUBROUTINE MLAT (CBM,SBM)
!********1*********2*********3*********4*********5*********6*********7**
! MLAT                                           PGMR - R.WILLIAMSON
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
! calculates geomagnetic coordinate quantities
!
!     common/const/
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CTRIG/CHA,SHA,CBG,SBG,CLG,SLG,SMODIP
!
      data one/0./
!
!
      if(one.eq.1.) go to 10
      one=1.
!     c11p4=cos(11.4*umr)
!     s11p4=sin(11.4*umr)
!     c69p8=cos(69.8*umr)
!     s69p8=sin(69.8*umr)
      c11p4=COS(11.4*degrad)
      s11p4=SIN(11.4*degrad)
      c69p8=COS(69.8*degrad)
      s69p8=SIN(69.8*degrad)
!
   10 clgx=clg*c69p8-slg*s69p8
!     slgx=slg*c69p8+clg*s69p8
!
      sbm=sbg*c11p4+cbg*clgx*s11p4
      cbm=SQRT(one-sbm**2)
!
!     dmlat=asin(sbm)/umr
!     slm=(cbg*slgx)/cbm
!     clm=(-sbg*s11p4+cbg*clgx*c11p4)/cbm
!     dmlong=acos(clm)/umr
!     if(slm.lt..0) dmlong=360.-dmlong
!
      return
      END
