!$FRQGET
      SUBROUTINE FRQGET(FRQ)
!********1*********2*********3*********4*********5*********6*********7**
! BOD              85/04/02            8504.0    PGMR - D. ROWLANDS
!
! FUNCTION:  PRINT OUT OBSERVATION DATA HEADER RECORD FOR BINRES
!            FILE AND  OBSRVATION DATA RECORDS
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
!
!
      LOGICAL ONEWAY,LSW
      INCLUDE 'COMMON_DECL.inc'
      COMMON/ANTID/ISATAN(3),ISTAN1(3),ISATN1(3),ISTAN2(3),ISATN2(3),   &
     &             IANNB1(3),IANNB2(3)
      COMMON/BRESDAT/ALEN(20),ALOC(20),ADAT(80)
      DIMENSION FRQ(20)
      DIMENSION BHD(6)
      DIMENSION JJ(8,20),NEIGH(3,6),INDA(6),IADJ(6)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!
      zero=0.
!
      mtype=INT(alen(4))
      if(mtype.ge.37.and.mtype.le.60) go to 10
      if (mtype.eq.99) go to 10
      write(6,1000) mtype
 1000 format (' ionosphere correction not available for ',              &
     &        'measurment type =',i4)
      return
!
   10 fref=adat(16)
      if (fref.ge.4.D14) return
!
      LSW=MOD(mtype,2).eq.0
      lrelay=mtype.ge.55.and.mtype.le.60
      lrlayd=LSW.and.lrelay
      oneway=mtype.eq.56
!
      isat1=INT(adat(21))
      ista1=INT(adat(20))
!
      nstab=INT(alen(9))
      nsatb=INT(alen(10))
      nhdr=MAX(nstab,nsatb)
!
      if(lrelay.and.nhdr.gt.2.and.adat(36).eq.zero) adat(36)=adat(26)
!
      isize=10*nhdr+13
!     write (6,1120) (i,alen(i),aloc(i),adat(i),i=1,20)
!1120  format(20x,'alen',20x,'aloc',20x,'adat'/
!     .       (i3,3g24.15))
!      if (isize.gt.20)
!     .write (6,1121) (i,adat(i),i=21,isize)
! 1121 format(i3,48x,g24.15)
      do 15 i=1,nhdr
      ii=i+i
      neigh(1,ii-1)=0
      neigh(2,ii-1)=0
      neigh(3,ii-1)=0
      neigh(1,ii)=0
      neigh(2,ii)=iannb1(i)
      neigh(3,ii)=iannb2(i)
   15 continue
!
!   The following code is to get the correct linkage into
!   neigh.  The aloc does NOT correspond to the measurement type
!   description, NOR do the satellite/station settings correspond to
!   the #N block header descriptions.
!
      do 20 i=1,6
      inda(i)=0
   20 bhd(i)=zero
      bhd(2)=adat(21)
      bhd(4)=adat(31)
      bhd(6)=adat(41)
      imax=nhdr+nhdr
      do 25 i=2,imax,2
      if (alen(i+10).eq.zero) then
        inda(i)=0
      elseif (alen(i+10).eq.bhd(6)) then
        bhd(6)=-1.
        inda(i)=6
      elseif (alen(i+10).eq.bhd(4)) then
        bhd(4)=-1.
        inda(i)=4
      elseif (alen(i+10).eq.bhd(2)) then
        bhd(2)=-1.
        inda(i)=2
      else
        stop 8
      endif
   25 continue
!
      nx=0
      je=-6
      ie=0
      jesat=0
      j=0
      ind=0
      lend=.false.
!
!   The iadj array is to correct ind to its proper definition according
!   the block header words.
!
      iadj(1)=0
      iadj(2)=0
      iadj(3)=0
      iadj(4)=0
      iadj(5)=0
      iadj(6)=0
!
! order in aloc array is tracing backwards in time along signal path
! two way relay doppler has 2 consecutive station records at end;
! one way relay doppler has no station record other than the first one
! preceeding a satellite record
!
      do 30 i=1,20
      if (aloc(i).le.zero) go to 30
      indp=ind
      ind=INT(aloc(i)-10)
!
      if (je.le.0) then
        je=1
      else
        jp=je
        je=je+nx
        if (MOD(ind,2).eq.0) then
          jl=indp
          if (MOD(indp,2).eq.0) jl=MIN(ind,indp)
        else
         if (lrlayd.and.                                                &
     &       (oneway.and.nx.eq.7).or.(.not.oneway.and.nx.eq.9) ) then
            jp=je
            je=jesat
            ind=indjes
            iadj(ind)=0
            lend=.true.
            ie=ie+2
          endif
          jl=ind
          if (MOD(indp,2).ne.0) jl=MIN(ind,indp)
        endif
!
        indadj=iadj(ind)
        iadj(ind)=iadj(ind)+2
        ind=ind+indadj
!
        ix=0
        if (MOD(ind,2).ne.0) ix=100
        ii=inda(ind)
        if (ii.ne.0) then
            in1=neigh(2,ii)
             in2=neigh(3,ii)
! Because antennas are specified incorrectly in prepro -
! i.e., transmit/receive instead of receive/transmit -
! in1 and in2 have been swapped in the following two statements.
          if (lend) in2=in1
          ix=ix+in2*10+in1
         endif
!
        j=j+1
        jj(1,j)=jp
        jj(2,j)=je
        jj(3,j)=jp+1
        if (MOD(indp,2).ne.0) jj(3,j)=jj(3,j)+2
        jj(4,j)=je+1
        if (MOD(ind,2).ne.0) jj(4,j)=jj(4,j)+2
        jj(5,j)=indp
        jj(6,j)=ind
        jj(7,j)=jl
        jj(8,j)=ix
      endif
!
      if (MOD(ind,2).eq.0) then
        nx=7
        if (jesat.eq.0) then
          jesat=je
          indjes=ind
        endif
      else
        nx=9
      endif
      ie=ie+nx
   30 continue
!
      jsize=j
!
      j=jsize
      if (lrlayd) j=j-1
      lend=.false.
!
      fref=adat(16)
!
   40 ind=jj(6,j)
      n=(ind+1)/2
      freqt=adat(n*10+6)
      jstrt=j
      frq(j)=freqt
      j=j-1
      if (j.le.0) go to 80
!
   50 ind=jj(6,j)
      n=(ind+1)/2
      sid=alen(10+ind)
      ix=jj(8,j)
      bfprat=adat(n*10+6)
      call linkfr (sid,ix,fref,bfprat,frq(jstrt),freqt,freqn)
      freqt=freqn
      frq(j)=freqt
      if (lend) go to 80
      j=j-1
      if (j.gt.0) then
      if (freqt.gt.0.) go to 50
      go to 40
      elseif (lrlayd) then
      j=jsize
      lend=.true.
      go to 50
      endif
!
   80 continue
!
      return
      END
