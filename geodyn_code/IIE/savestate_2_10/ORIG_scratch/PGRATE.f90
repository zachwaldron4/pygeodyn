      SUBROUTINE PGRATE(NEQN,IOL1,NINTVL,INDH,SUMXX,SUMPOS,PDDTOS ,  &
     &           KNSTEPS,NEQNI,NHV,ISAT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      COMMON/IOS/ISTOS1,ISTOS2,ISTOS3,ISTLST,INTSUB(3),JNTSUB(3),      &
     &        ILSTSG(80),INTOSP,MAPSAT(3,4)
      COMMON/INTSAV/ILSTEP,NSEG,NSCHED(2,50,10)
      COMMON/LSNAPX/LSNAP
      COMMON/NSEGSC/NSEGM(80)
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
      DIMENSION INDH(NINTVL)
!     DIMENSION SUMXX(NEQN,3,2,NHV)
      DIMENSION SUMXX(8000,3,2,200)
      DIMENSION SUMPOS(NEQNI,3,2,1)
      DIMENSION PDDTOS(NEQNI,3,KNSTEPS,1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      NSTEP=INDH(1)-1
      IF(INDH(1).GE.JNTSUB(INTOSP)) THEN
         ISTART=1+NSTEP-(INDH(1)-JNTSUB(INTOSP))
      ELSE
         IDIF=JNTSUB(INTOSP)-1-NSTEP

      ILASTS=NSCHED(2,ISAT,NSEGM(ISAT))

       DO JJ=1,NSETA
       IF(ISAT.EQ.JJ.AND.JNTSUB(INTOSP).EQ.ILASTS.AND.LSNAP) THEN
       IDIF=2147483647
       ENDIF
       ENDDO


         IF(NSTEP.LE.IDIF) THEN
            DO I=1,6
               DO J=1,NEQNI
                 SUMXX(J,I,1,1)=SUMPOS(J,I,1,1)
               ENDDO
            ENDDO
           ISTART=1
         ELSE
           IDX=IOL1+JNTSUB(INTOSP)-1
           DO I=1,IDIF
             DO J=1,3
               DO K=1,NEQNI
                   SUMXX(K,J,1,1)=SUMXX(K,J,1,1)                        &
     &            -SUMXX(K,J,2,1)
                   SUMXX(K,J,2,1)=SUMXX(K,J,2,1)                        &
     &            -PDDTOS(K,J,IDX,1)
               ENDDO
             ENDDO
             IDX=IDX-1
           ENDDO
           GO TO 160
         ENDIF
      ENDIF
!
      IF(ISTART.GT.NSTEP) GO TO 160
      IF(NSTEP.LE.0) GO TO 160
!
! UPDATE SUMS
      DO 150 I=ISTART,NSTEP
      DO 100 J=1,3
      DO  50 K=1,NEQNI
      SUMXX(K,J,2,1)=SUMXX(K,J,2,1)                              &
     &              +PDDTOS(K,J,IOL1+I,1)
      SUMXX(K,J,1,1)=SUMXX(K,J,2,1)                             &
     &              +SUMXX(K,J,1,1)
  50  CONTINUE
 100  CONTINUE
 150  CONTINUE
 160  CONTINUE
!
      IF(NINTVL.LE.1) RETURN
      DO 500 K=2,NINTVL
      DO I=1,6
         DO J=1,NEQNI
           SUMXX(J,I,1,K)=SUMXX(J,I,1,K-1)
         ENDDO
      ENDDO
      NSTEP=INDH(K)-INDH(K-1)
      IF(NSTEP.LE.0) THEN
         WRITE(6,6000)
         WRITE(6,6001) NINTVL,K,INDH(K-1),INDH(K)
         STOP
      ENDIF
      ISTEP1=INDH(K-1)
      ISTEP2=INDH(K)-1
!
! UPDATE SUMS
      DO 250 I=ISTEP1,ISTEP2
      DO 200 J=1,3
      DO 190 KK=1,NEQNI
      SUMXX(KK,J,2,K)=SUMXX(KK,J,2,K)                            &
     &               +PDDTOS(KK,J,IOL1+I,1)
      SUMXX(KK,J,1,K)=SUMXX(KK,J,2,K)                           &
     &            +SUMXX(KK,J,1,K)
 190  CONTINUE
 200  CONTINUE
 250  CONTINUE
!
 500  CONTINUE
      RETURN
 6000 FORMAT(' EXECUTION TERMINATING IN PGRATE')
 6001 FORMAT(' NINTVL',I10,' K ',I10,' INDH(K-1)& INDH(K) ',2I10)
      END
