!$LINKFR
      SUBROUTINE LINKFR (SID,IX,FREF,BFPRAT,FRQI,FREQT,FREQN)
!********1*********2*********3*********4*********5*********6*********7**
! LINKFR           07/18/96            .0    PGMR -
!
! FUNCTION:
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      CHARACTER*3 ATYPE
      LOGICAL TDRSU
      LOGICAL NTFRST
      DIMENSION TRANSP(3,20)
      dimension relids(33)
      dimension ftdrs(6,2)
!
      DATA TDRSU /.FALSE./
      data ntfrst /.false./
      data lfirst /.true./
!
!
!  WSGT uplink and downlink frequencies: sa1,sa2,ma,ka1,ka2,cmnd(telem)
      data ftdrs / 14679.5D6, 14719.5D6, 14826.4D6,                     &
     &             14625.0D6, 15200.0D6 ,14785.9625D6,                  &
     &             13677.5D6, 13697.5D6, 13513.75D6,                    &
     &             13528.4D6, 13928.4D6 ,13731.0D6 /
!
      data relids/8302602.,8809102.,8902102.,9105402.,9300302.,9503502.,&
     &            7000001.,7000003.,7000004.,7000005.,7000006.,7000007.,&
     &            7000008.,7000009.,7000010.,8000001.,8000003.,8000004.,&
     &            8000005.,8000006.,8000007.,8000008.,8000009.,8000010.,&
     &            9000001.,9000003.,9000004.,9000005.,9000006.,9000007.,&
     &            9000008.,9000009.,9000010./
!
      data nrelay /33/
      data ntrans /0/
!
!
!**********************************************************************
! STARP OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      zero=0.
      one=1.
!
      do 10 i=1,nrelay
      if(sid.eq.relids(i)) go to 60
   10 continue
!
      if (tdrsu) then
        freqn=fref
        tdrsu=.false.
        return
      endif
!
      if (ntfrst) go to 30
      ntfrst=.true.
      ntrans=0
   20 read (51,1000,end=30) atype,ina,iouta,isat,k1,k2
 1000 format (a3,1x,i1,1x,i1,2x,i8,2x,2i5)
      ntrans=ntrans+1
      if (ntrans.gt.20) go to 110
      if (atype.eq.'STA') then
        ix=ina*10+iouta+100
      else if (atype.eq.'SAT') then
        ix=ina*10+iouta
      else
        ix=0
      endif
      transp(1,ntrans)=ix
      transp(2,ntrans)=isat
      xk1=k1
      xk2=k2
      transp(3,ntrans)=xk1/xk2
      go to 20
!
   30 freqn=0.
      if (ntrans.le.0) return
!
      xant=ix
      do 40 i=1,ntrans
      if (sid.eq.transp(2,i).and.xant.eq.transp(1,i)) go to 50
   40 continue
!
      return
!
   50 freqn=freqt*transp(3,i)
      return
!
! TDRS   4 is ground antenna number
!
   60 iant=ix/10
      if (iant.lt.4) then
        freqn=ftdrs(iant,2)
        if(iant.lt.3.and.bfprat.le.zero) then
          freqn=freqt*(one+bfprat)
        endif
      else
        iant=MOD(ix,10)
        if (iant.lt.4) then
          if (iant.lt.3.and.bfprat.lt.zero) then
            iant=iant+3
            xk=1600.D0/1469.D0
            fref=15003.4D6
          else
            xk=240.D0/221.D0
            fref=2287.5D6
            if (iant.lt.3) then
              if(bfprat.ne.zero) then
              fref= ftdrs(iant,2)/(one+bfprat)
              else
                if(lfirst)write(6,4000)
                lfirst=.false.
                fref=2250.0D6
              endif
            endif
          endif
          freqn=fref/xk
          tdrsu=.true.
        else if (iant.eq.4) then
          iant=6
          freqn=ftdrs(iant,2)
        else
          go to 100
        endif
!
      frqi=ftdrs(iant,1)
!
      endif
 4000 format (/'** Warning - Ratios were not included in the data ',&
     &'header records. Frequencies',/,' calculated for the ionospheric',&
     &'model will be set to a default value. **',/)
      return
!
  100 write (6,2000) ix
 2000 format (' Antenna numbering not as expected for TDRS -',          &
     &' terminating.'/' Data antenna "ix" =',i2)
!
      stop 12
!
  110 write (6,3000)
 3000 format (' Only have capacity for 20 transponders - terminating.')
!
      stop 12
      END
