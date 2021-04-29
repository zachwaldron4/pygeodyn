!$EPRGET
      SUBROUTINE EPRGET (ARYC)
!********1*********2*********3*********4*********5*********6*********7**
! EPRGET                                         PGMR - R.WILLIAMSON
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
!
!
      COMMON/EPROF/HMF2,RNMF2,HMF1,RNMF1,B0P,B1,C1ION,HZ,T,HST,EION(4), &
     &        HME,RNME,HEF,HMD,RNMD,HDX,D1,XKK,FP30,FP3U,FP1,FP2,       &
     &        BETA,ETAION,DELTAI,ZETAIO,X0,EPTRC1,EPTRC2,               &
     &        ALNMF2,ALNME,ALNMD,RNIGHT
      dimension aryc(37),ary(37)
      equivalence (ary(1),hmf2)
!
!
      do 10 i=1,37
   10 aryc(i)=ary(i)
      END
