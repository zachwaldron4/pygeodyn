      SUBROUTINE DUMPLB(IOUT,SUMXOS,SUMPOS,XDDTOS,PDDTOS,I,KNSTEPS,NEQNI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION SUMXOS(3,2,1)
      DIMENSION SUMPOS(NEQNI,3,2,1)
      DIMENSION XDDTOS(3,KNSTEPS,1)
      DIMENSION PDDTOS(NEQNI,3,KNSTEPS,1)

!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
       WRITE(IOUT) (SUMXOS(IP,1,1),IP=1,6)
      DO IQP=1,I
        WRITE(IOUT) XDDTOS(1,IQP,1),XDDTOS(2,IQP,1),XDDTOS(3,IQP,1)
      ENDDO
      NN=NEQNI*3
      NN2=NEQNI*6
      WRITE(IOUT) (SUMPOS(IQP,1,1,1),IQP=1,NN2)
      do jj=1,nn2
!     write(6,*)' dbg write out SUMPOS  2 ',SUMPOS(jj,1,1,1)
      enddo

      DO IQP=1,I
        WRITE(IOUT) (PDDTOS(JQP,1,IQP,1),JQP=1,NN)
      ENDDO

      RETURN
      END
