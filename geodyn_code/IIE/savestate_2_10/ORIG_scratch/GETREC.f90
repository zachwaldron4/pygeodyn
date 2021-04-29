      SUBROUTINE GETREC(NPAR,NPART,ISG,RESID,WT,PART,LEND)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION PART(NPART)
!
!
      DO IQP=1,NPART
        PART(IQP)=0.D0
      ENDDO
      NGLB=NPAR
!
!
      LEND=.TRUE.
      READ(63,END=100) RESID,WT,                                        &
     &                 (PART(ISG-1+JQP),JQP=1,NGLB)
      LEND=.FALSE.
      RETURN
  100  CONTINUE
      RETURN
      END
