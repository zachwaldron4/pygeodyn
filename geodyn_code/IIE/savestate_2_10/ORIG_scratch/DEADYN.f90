!$DEADYN
      SUBROUTINE DEADYN
!********1*********2*********3*********4*********5*********6*********7**
! DEADYN           99/11/21            9910.0    PGMR - S.B. LUTHCKE
!
! FUNCTION: DEALLOCATE DYNAMICALLY ALLOCATED ARRAYS
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**

      USE MSSGFC_MOD

      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)

      COMMON/CALTOP/LADYNO,LAGEOO,LRGEOT,LRETDT,LROTDT,LRSSTT,LCOTRM,   &
     &              LFILOT,LATCOR,LFIRIT,LX1ALT,LX2ALT,LMSSWG,LATMOD,   &
     &              LG1BOT,LG1B,LONEG1,LXATTD,LXBEAM,NXALTO
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      SAVE
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
! UNLOAD GSFC MEAN SEA SURFACE GRID
!

      IF(LMSSWG) THEN
!       print *,'deadyn: unloading mssg'
       CALL ULMSSG
      ENDIF

      IF(LOTRM2) THEN
!...test point
       dlat=-42.0D0
       dlon=330.0D0
       time=49100
!       print *,'deadyn: unloading got99 grid'
       CALL PERTH2(dlat,dlon,time,tide,isdata,.true.)
      ENDIF

      RETURN
      END
