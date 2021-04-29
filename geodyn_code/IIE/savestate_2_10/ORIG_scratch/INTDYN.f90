!$INTDYN
      SUBROUTINE INTDYN(GDNAME,GRSIZE,GRCNT,GRDATA)
!********1*********2*********3*********4*********5*********6*********7**
! INTDYN           99/11/21            9910.0    PGMR - S.B. LUTHCKE
!
! FUNCTION: INITIALYZE AND SETUP DYNAMICALLY ALLOCATED ARRAYS
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**

      USE MSSGFC_MOD

      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
      CHARACTER*8 :: GDNAME
!
      COMMON/CALTOP/LADYNO,LAGEOO,LRGEOT,LRETDT,LROTDT,LRSSTT,LCOTRM,   &
     &              LFILOT,LATCOR,LFIRIT,LX1ALT,LX2ALT,LMSSWG,LATMOD,   &
     &              LG1BOT,LG1B,LONEG1,LXATTD,LXBEAM,NXALTO
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/NGRIDS/NCTGRD,NREFER(6),NCENTR(6),NXGRID
!
      DIMENSION GDNAME(1),GRSIZE(1),GRCNT(1),GRDATA(4,1)
      dimension tstlat(5),tstlon(5)
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
! LOAD GSFC MEAN SEA SURFACE GRID
!
!      print *,'intdyn: before ldmssg lmsswg: ',lmsswg
!      print *,'intdyn: dphi,dlam,new,xlonw: ',dphi,dlam,new,xlonw
!      print *,'intdyn: nnl,xlats: ',nnl,xlats
!
!     IF(LMSSWG) THEN
       DO I=1,NCTGRD
       CALL LDMSSG(90.0D0,GDNAME,GRSIZE,GRCNT,GRDATA,I,NREFER,NCENTR)
       ENDDO
!     ENDIF
!      print *,'mssgrd(300,300): ',mssgrd(300,300)

!      print *,'intdyn: after ldmssg lmsswg: ',lmsswg
!      print *,'intdyn: dphi,dlam,new,xlonw: ',dphi,dlam,new,xlonw
!      print *,'intdyn: nnl,xlats: ',nnl,xlats
!
! TEST MSSH COMPUTATION
!
      tstlat(1)=-0.026626
      tstlat(2)=-0.075664
      tstlat(3)=-0.124702
      tstlat(4)=-0.664117
      tstlat(5)=-1.007375

      tstlon(1)=265.758918
      tstlon(2)=265.776401
      tstlon(3)=265.793883
      tstlon(4)=265.986199
      tstlon(5)=266.108606

!       print *,'intdyn: xlats(1): ', xlats(1)
!       print *,'intdyn: xlonw(1): ', xlonw(1)
!       print *,'intdyn: dphi(1): ', dphi(1)
!       print *,'intdyn: nnl(1): ', nnl(1)
!       print *,'intdyn: new(1): ', new(1)

      DO I=1,5
       CALL GINTRP(4,5.D0,GRID1,XLATS(1),XLONW(1),DPHI(1),DPHI(1), &
     &      NNL(1),NEW(1),NNL(1),NEW(1),TSTLAT(i),TSTLON(i),       &
     &      HTMSS)
      print *,'tstlat,tstlon,htmss: ',tstlat(i),tstlon(i),htmss
      END DO

!
! LOAD GOT99 ocean + load tide grid
!
!      print *,'intdyn: lcotrm: ',lcotrm
      IF(LCOTRM) THEN

!...test point
       dtlat=-42.0D0
       dtlon=330.0D0
       ttime=49100.0D0

       CALL PERTH2(dtlat,dtlon,ttime,ttide,lsdata,.false.)
       CALL LPEQMT(ttime*86400.0D0,dtlat,ttidel)
        TTIDE = TTIDE + TTIDEL
        write(6,*) 'intdyn: ttide,ttidel: ',ttide,ttidel
        write(6,*) ''
        write(6,*) 'INTDYN: GOT99 TIDE LOADED'
        write(6,*) 'TEST: ttime,dtlat,dtlon,ttide,lsdata:'
        write(6,*) ttime,dtlat,dtlon,ttide,lsdata
        write(6,*) ''

      ENDIF

      RETURN
      END
