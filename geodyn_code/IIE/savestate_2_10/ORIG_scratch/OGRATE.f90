      SUBROUTINE OGRATE(IOL1,NINTVL,INDH,SUMXX,SUMXOS,XDDTOS,KNSTEPS, &
     & ISAT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION INDH(NINTVL)
      DIMENSION SUMXX(3,2,11)
      DIMENSION SUMXOS(3,2,1),XDDTOS(3,KNSTEPS,1)
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
      COMMON/IOS/ISTOS1,ISTOS2,ISTOS3,ISTLST,INTSUB(3),JNTSUB(3),      &
     &        ILSTSG(80),INTOSP,MAPSAT(3,4)
      COMMON/INTSAV/ILSTEP,NSEG,NSCHED(2,50,10)
      COMMON/LSNAPX/LSNAP
      COMMON/NSEGSC/NSEGM(80)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      NSTEP=INDH(1)-1
      IF(INDH(1).GE.INTSUB(INTOSP)) THEN
         ISTART=1+NSTEP-(INDH(1)-INTSUB(INTOSP))
      ELSE
         IDIF=INTSUB(INTOSP)-1-NSTEP
!        IF(INTSUB(INTOSP).EQ.70001) IDIF=2147483647
!
      ILASTS=NSCHED(2,ISAT,NSEGM(ISAT))

       DO JJ=1,NSETA
       IF(ISAT.EQ.JJ.AND.INTSUB(INTOSP).EQ.ILASTS.AND.LSNAP) THEN
       IDIF=2147483647
       ENDIF
       ENDDO

         IF(NSTEP.LE.IDIF) THEN
            DO I=1,6
              SUMXX(I,1,1)=SUMXOS(I,1,1)
            ENDDO
            ISTART=1
         ELSE
            IDX=IOL1+INTSUB(INTOSP)-1
            DO I=1,IDIF
              DO J=1,3
                 SUMXX(J,1,1)=SUMXX(J,1,1)                              &
     &          -SUMXX(J,2,1)
                 SUMXX(J,2,1)=SUMXX(J,2,1)                              &
     &          -XDDTOS(J,IDX,1)
              ENDDO
              IDX=IDX-1
            ENDDO
            GO TO 160
         ENDIF
      ENDIF
      IF(NSTEP.LE.0) GO TO 160
      IF(ISTART.GT.NSTEP) GO TO 160
!
! UPDATE SUMS
      DO 150 I=ISTART,NSTEP
      DO 100 J=1,3
!     SUMX(J,2,IBACK1)=SUMX(J,2,IBACK)                                  &
!    &                +XDDOT(J,IORBAK)
!     SUMX(J,1,IBACK1)=SUMX(J,2,IBACK1)                                 &
!    &                +SUMX(J,1,IBACK)
      SUMXX(J,2,1)=SUMXX(J,2,1)                                  &
     &                +XDDTOS(J,IOL1+I,1)
      SUMXX(J,1,1)=SUMXX(J,2,1)                                 &
     &            +SUMXX(J,1,1)
 100  CONTINUE
 150  CONTINUE
 160  CONTINUE
!
      IF(NINTVL.LE.1) RETURN
      DO 500 K=2,NINTVL
      DO I=1,6
        SUMXX(I,1,K)=SUMXX(I,1,K-1)
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
      SUMXX(J,2,K)=SUMXX(J,2,K)                                  &
     &                +XDDTOS(J,IOL1+I,1)
      SUMXX(J,1,K)=SUMXX(J,2,K)                                 &
     &            +SUMXX(J,1,K)
 200  CONTINUE
 250  CONTINUE
!
 500  CONTINUE
      RETURN
 6000 FORMAT(' EXECUTION TERMINATING IN OGRATE')
 6001 FORMAT(' NINTVL',I10,' K ',I10,' INDH(K-1)& INDH(K) ',2I10)
      END
