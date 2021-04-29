!$SAVATP
      SUBROUTINE SAVATP(PMATT,X2PATT,NUCON,NREDUC,KXBPA,I)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
!
      DIMENSION PMATT(NUCON,3,20)
      DIMENSION X2PATT(3,20,NREDUC)
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      DO 10 K=1,3
      DO 10 J=1,20
      X2PATT(K,J,KXBPA)=PMATT(I,K,J)
!     write(6,*)' dbg SAVATP PMATT ',PMATT(I,K,J),I,K,J
!     write(6,*)' dbg SAVATP ',X2PATT(K,J,KXBUP),K,J,KXBUP
   10 CONTINUE
      RETURN
      END
