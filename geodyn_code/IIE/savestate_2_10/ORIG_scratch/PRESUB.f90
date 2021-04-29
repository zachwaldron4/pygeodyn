!$PRESUB
      SUBROUTINE PRESUB(NTYPE ,SUMCOR,AA    ,NUMOB,TROPP )
!********1*********2*********3*********4*********5*********6*********7**
! PRESUB           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  INITIALIZE NORMAL POINT OR SIMULATED DATA. OUTPUT FILE
!            BUFFER (ONE OBSERVATION AT A TIME)
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NTYPE    I    S    MEASUREMENT TYPE
!   SUMCOR   I    S    SUM OF OBSERVATION CORRECTIONS FOR THIS
!                      OBSERVATION
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   NUMOB    I    S    INDEX FOR ONE OBSERVATION IN THE BLOCK
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
! /CBLOKI/ INTEGER INFORMATION ABOUT DATA BLOCK
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CLPRNR/LPR9NR(24),LPRNRM(24,3)
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/COPARM/LOP1
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
      COMMON/TROPX/LOUTRP
!
      DIMENSION AA(1)
      DIMENSION TROPP(3)
      DIMENSION HCORR(10,3)
!
      DATA ZERO/0.0D0/
      DATA METRIX/37/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      DO 100 I=1,30
      HCORR(I,1)=0.D0
  100 END DO
!CC      write(6,*) 'presub: at entry sumcor ', sumcor
!
      IF(MTYPE.LT.13) RETURN
      ISUB=0
      I1=2
      I2=9
      INCR=1
      IF(MTYPE.GE.METRIX) GO TO 2000
      IF(.NOT.LOP1) ISUB=1
      I1=NTYPE-MTYPE+1
      IF(MTYPE.GT.14) I1=I1+2
      I2=8
      INCR=2
 2000 CONTINUE
      NCORR=0
      DO 3000 NH=1,NHEADB
         NHP1=NH+1
         DO 3000 I=I1,I2,INCR
            IF(.NOT.LSWTCH(I,NHP1)) GO TO 3000
            NCORR=NCORR+1
 3000 CONTINUE
      IF(NCORR.LE.0) RETURN
      LFIRST=.TRUE.
      NC=0
      IF(MTYPE.GT.14) GO TO 5000
      IF(.NOT.LSWTCH(9,2)) GO TO 5000
      INDEX=KOBCOR(9,1)+NUMOB-1
      IF(AA(INDEX).EQ.ZERO) GO TO 5000
      NC=1
!     IF(.NOT.LPRE(9,1)) G(NC)=GEODYN
      IF(LPRNRM(9,1)) GO TO 5000
!CC          write(6,*) 'presub: index, aa(index) sumcor ',
!CC        1                     index, aa(index),sumcor
      SUMCOR=SUMCOR-AA(INDEX)
      AA(INDEX)=ZERO
 5000 CONTINUE
      DO 6000 NH=1,NHEADB
!CC         write(6,*) 'presub: nh ', nh
         NHP1=NH+1
         DO 6000 I=I1,I2,INCR
!CC            write(6,*) 'presub: i,lswtch(i,nhp1) ', i,lswtch(i,nhp1)
            IF(.NOT.LSWTCH(I,NHP1)) GO TO 6000
            NC=NC+1
!           IF(.NOT.LPRE(I,NH)) G(NC)=GEODYN
!CC            write(6,*) 'presub: i,lprnrm(i-isub,nh) ',
!CC     1                          i,lprnrm(i-isub,nh)
            INDEX=KOBCOR(I,NH)+NUMOB-1
            IF(LPRNRM(I-ISUB,NH)) GO TO 5400
!CC            write(6,*) 'presub: i, index, aa(index) sumcor ',
!CC     1                          i, index, aa(index),sumcor
            SUMCOR=SUMCOR-AA(INDEX)
            IF(LOUTRP) THEN
               IF(I.EQ.3.OR.I.EQ.4) THEN
                HCORR(I,NH)=AA(INDEX)*(1.D0+TROPP(NH))
                AA(INDEX)=HCORR(I,NH)
                GO TO 5500
               ENDIF
            ENDIF
            AA(INDEX)=ZERO
 5400       CONTINUE
            HCORR(I,NH)=AA(INDEX)
 5500       CONTINUE
            IF(NC.LT.5) GO TO 6000
            LFIRST=.FALSE.
            NC=0
 6000 CONTINUE
!CC      write(6,*) 'presub: at exit sumcor ', sumcor
      SUMCOR=HCORR(3,1)+HCORR(4,1)+HCORR(6,1)                           &
     &      +HCORR(3,2)+HCORR(4,2)+HCORR(6,2)                           &
     &      +HCORR(3,3)+HCORR(4,3)+HCORR(6,3)
      RETURN
      END
