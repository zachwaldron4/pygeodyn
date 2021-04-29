!$MOVCOR
      SUBROUTINE MOVCOR(MTYPE,LMOV,LOBS,ICORR,LOBSCK)
!********1*********2*********3*********4*********5*********6*********7**
! MOVCOR           12/30/85            8512.0    PGMR - TOM MARTIN
!                  02/28/86            8512.1    PGMR - TOM MARTIN
!                  04/01/86            8512.2    PGMR - TOM MARTIN
!
! FUNCTION         SET DEFAULT MOVE/CORRECT/OBS CONTROL ARRAYS FOR A
!                  GIVEN MEASUREMENT TYPE. THIS INVOLVES DECODING A
!                  COMPRESSED ARRAY.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MTYPE    I    S    MEASUREMENT TYPE
!   LMOV     O    A    PARITITION MOVE ARRAY
!   LOBS     O    A    F=OBS SUM #1, T=OBS SUM #2
!   ICORR    O    A    1=CORRECT, 0=DO NOTHING, -1=REMOVE CORR
!                      ** UPDATE ** ICORR NOT DETERMINED BY TABLE
!                      BUT INITALIZED TO 0
!   LOBSCK   O    S    TRUE IF LOBS EVER TRUE
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      COMMON/IOBTYP/NDXST(2,35),NDXSAT(2,35),                           &
     &      NDRSTA(50),NDRSAT(50),NDRST2(50),NDRSA2(50),                &
     &      ITMBIT(999),IOETR(999),MTYPED(11),NTYPES,                   &
     &      NM0112(4),NM1314(4),NM1522(4),NM2330(4),                    &
     &      NM3138(4),NM3997(4),NM4098(4),NM9900(4),NMT199(4),NMT203(4),&
     &      NS0112(4),NS1314(4),NS1522(4),NS2330(4),                    &
     &      NS3138(4),NS3997(4),NS4098(4),NS9900(4),NST199(4),NST203(4),&
     &      ITYPD(12,2),ITYPDD(7,2),ILINKF(5),ISTAFN(5,3),              &
     &      ISATFN(5,3),ILINKT(5),ISTATN(5,3),ISATTN(5,3),              &
     &      MGOTO(12),MENTER(12),MEXIT(12),                             &
     &      NDST60(3,24),NDST82(6,12),NDST92(12,7),NTDRSS,              &
     &      NDSA60(3,24),NDSA82(6,12),NDSA92(12,7),                     &
     &      MTDRSS(7),KTDRSS(7),ITDRSS(4,7),JTDRSS(4,7),                &
     &      NLLC60(5,24),NLLC82(10,12),NLLC98(20,7),NNBIA,              &
     &      NXMTYP
      COMMON/LOBTYP/L2WAYS(31),LODD(11),NXLTYP
      COMMON/ROBTYP/CONVRT(999),OBSCAL(999),RESCAL(999),                &
     &              XMTYP
!
      DIMENSION LMOV(86),LOBS(86),ICORR(86)
      DIMENSION NMOVE(4,9),NSUM2(4,9)
      EQUIVALENCE (NMOVE(1,1),NM0112(1)),(NMOVE(1,2),NM1314(1)),        &
     &            (NMOVE(1,3),NM1522(1)),(NMOVE(1,4),NM2330(1)),        &
     &            (NMOVE(1,5),NM3138(1)),(NMOVE(1,6),NM3997(1)),        &
     &            (NMOVE(1,7),NM4098(1)),(NMOVE(1,8),NM9900(1)),        &
     &            (NMOVE(1,9),NMT199(1))
      EQUIVALENCE (NSUM2(1,1),NS0112(1)),(NSUM2(1,2),NS1314(1)),        &
     &            (NSUM2(1,3),NS1522(1)),(NSUM2(1,4),NS2330(1)),        &
     &            (NSUM2(1,5),NS3138(1)),(NSUM2(1,6),NS3997(1)),        &
     &            (NSUM2(1,7),NS4098(1)),(NSUM2(1,8),NS9900(1)),        &
     &            (NSUM2(1,9),NST199(1))
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      DO 1000 N=1,NTYPES
      IF(MTYPE.GT.MTYPED(N)) GO TO 1000
      IND=N
      GO TO 2000
 1000 END DO
      IND=NTYPES
 2000 CONTINUE
      IF(LODD(IND)) GO TO 3000
      INCR=1-MOD(MTYPE,2)
      IND=IND+INCR
 3000 CONTINUE
      DO 5000 K=1,4
      I1=(K-1)*24+1
      I2= MIN(I1+23,86)
      NM=NMOVE(K,IND)
      NS=NSUM2(K,IND)
      DO 4000 I=I1,I2
      M=NM/2
      LMOV(I)=(NM-M*2).EQ.1
      NM=M
      M=NS/2
      LOBS(I)=(NS-M*2).EQ.1
      NS=M
 4000 END DO
 5000 END DO
      LOBSCK=MOD(NS,2).EQ.1
!
!     SET CORRECTION ARRAY TO PERFORM NO CORRECTIONS FOR PARTITIONS
!     CONTAINING THE OBSERVATION AND CONTAINING THE OBSERVATION
!     CORRECTION RECORD - PREPRO CARDS WILL INDICATE THESE
!
      DO 6000 I=1,86
      ICORR(I)=0
 6000 END DO
      RETURN
      END
