      SUBROUTINE FILDST(PXPF,ND1,ND2,ND3,NM,LNPNM,IPXST,NXSTAT,TXSTAT,  &
     &                  MJDSEC,FSEC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION PXPF(ND1,ND2,ND3),TXSTAT(NXSTAT),FSEC(NM)
!
      IF(IPXST.LE.0.OR.NXSTAT.LE.0) RETURN
!
!
      IQS=IPXST
      TOBS0=DBLE(MJDSEC)
!
      IF(LNPNM) THEN
         DO J=1,NXSTAT
           DO I=1,NM
             TOBS=TOBS0+FSEC(I)
             IF(TOBS.LE.TXSTAT(J)) THEN
                PXPF(IQS,1,I)=0.D0
                PXPF(IQS,2,I)=0.D0
                PXPF(IQS,3,I)=0.D0
                PXPF(IQS+1,1,I)=0.D0
                PXPF(IQS+1,2,I)=0.D0
                PXPF(IQS+1,3,I)=0.D0
                PXPF(IQS+2,1,I)=0.D0
                PXPF(IQS+2,2,I)=0.D0
                PXPF(IQS+2,3,I)=0.D0
                PXPF(IQS+3,1,I)=0.D0
                PXPF(IQS+3,2,I)=0.D0
                PXPF(IQS+3,3,I)=0.D0
                PXPF(IQS+4,1,I)=0.D0
                PXPF(IQS+4,2,I)=0.D0
                PXPF(IQS+4,3,I)=0.D0
                PXPF(IQS+5,1,I)=0.D0
                PXPF(IQS+5,2,I)=0.D0
                PXPF(IQS+5,3,I)=0.D0
             ENDIF
           ENDDO
           IQS=IQS+6
         ENDDO
      ELSE
         DO J=1,NXSTAT
           DO I=1,NM
             TOBS=TOBS0+FSEC(I)
             IF(TOBS.LE.TXSTAT(J)) THEN
                PXPF(I,IQS,1)=0.D0
                PXPF(I,IQS,2)=0.D0
                PXPF(I,IQS,3)=0.D0
                PXPF(I,IQS+1,1)=0.D0
                PXPF(I,IQS+1,2)=0.D0
                PXPF(I,IQS+1,3)=0.D0
                PXPF(I,IQS+2,1)=0.D0
                PXPF(I,IQS+2,2)=0.D0
                PXPF(I,IQS+2,3)=0.D0
                PXPF(I,IQS+3,1)=0.D0
                PXPF(I,IQS+3,2)=0.D0
                PXPF(I,IQS+3,3)=0.D0
                PXPF(I,IQS+4,1)=0.D0
                PXPF(I,IQS+4,2)=0.D0
                PXPF(I,IQS+4,3)=0.D0
                PXPF(I,IQS+5,1)=0.D0
                PXPF(I,IQS+5,2)=0.D0
                PXPF(I,IQS+5,3)=0.D0
             ENDIF
           ENDDO
           IQS=IQS+6
         ENDDO
      ENDIF
      RETURN
      END
