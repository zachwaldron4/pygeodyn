!$EPINIT
      SUBROUTINE EPINIT
!********1*********2*********3*********4*********5*********6*********7**
! EPINIT           85/03/05            8502.0    PGMR - D.ROWLANDS
!
! FUNCTION:  TO DECIDE WHICH PRECESSION CONSTANTS WILL BE USED IN
!            THIS RUN.THESE CONSTANTS INCLUDE THE COEFFCIENTS
!            IN POLYNOMIALS FOR THE THREE ARGUMENTS OF PRECESSION,
!            THE MEAN OBLIQUITY AND 0 HR GMST.FOR THE THREE ARGUMENTS
!            OF PRECESSION,THE METHOD OF CALCULATION IS SLIGHTLY
!            DIFFERENT.SO,THE ROUTINE DETERMINES WHICH PRECESSION
!            ROUTINE (PNCON OR NPNCON) WILL BE CALLED.
!
! COMMENTS:
!
!      NOTE - THE DECISION IS BASED ON THE VERSION OF EPHEMERIS USED
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/EPHMPT/NTOT,NTOTS,NNPDPR,NUTORD,IBODDG(8),IGRPDG(4),       &
     &              NENPD,INRCD(2),KPLAN(11),NINEP,NEPHMR,NEPHEM,       &
     &              IBHINT,NINTBH,NXEPHP
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/THETGC/THTGC(4)
      DIMENSION HTHTGC(4,2)
      DATA HTHTGC/+.1835753996233393D+01,+.1991063821496793D-06,        &
     &            +.6783813591770405D-24,+.0D0,                         &
     &            +.1835755989220523D+01,+.1991063841042646D-06,        &
     &            +.6799511389471967D-24,-.2434649839636326D-37/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      ION=2
      IF(NEPHEM.LT.200) ION=1
      THTGC(1)=HTHTGC(1,ION)
      THTGC(2)=HTHTGC(2,ION)
      THTGC(3)=HTHTGC(3,ION)
      THTGC(4)=HTHTGC(4,ION)
      RETURN
      END
