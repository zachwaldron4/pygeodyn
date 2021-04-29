!$GETATP
      SUBROUTINE GETATP(PMATT,X2PATT,X2VPA,NREDUC,NUCON,K,N)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
!
      DIMENSION PMATT(NUCON,3,20)
      DIMENSION X2PATT(3,20,NREDUC)
      DIMENSION X2VPA(NUCON,3,20,2)
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      DO 10 I=1,3
      DO 10 J=1,20
      X2VPA(N,I,J,1)=X2PATT(I,J,K)
      X2VPA(N,I,J,2)=PMATT(N,I,J)
!     write(6,*)' dbg X2VPA GETATP ',X2VPA(N,I,J,1),N,I,J,1
!     write(6,*)' dbg X2VPA GETATP 2 ',X2VPA(N,I,J,2),N,I,J,2
!     write(6,*)' dbg GETATP NOT LNPNM FIRST ',X2PATT(I,J,K),I,J,K
!     write(6,*)' dbg SECOND             ',PMATT(N,I,J),N,I,J
   10 CONTINUE
      RETURN
      END
