!$ROBSDD
      SUBROUTINE ROBSDD(AA,II,LL,TPARTL,INDSTA,INDSAT,INDSET,           &
     &   INPSTA,INPSAT,LELEVS,MTYPEX,ISEQ,MSEQ,LADD,ILCORR,SAVEC1,      &
     &   SAVEC2,NM3,ILOOP,XMM,UMV,UM1,UM2, cg_par_array )

!********1*********2*********3*********4*********5*********6*********7**
! ROBSDD           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   LL      I/O   A    LOGICAL DYNAMIC ARRAY
!   TPARTL  I/O   A    PARTIALS OF OBSERVATIONS W.R.T TIME
!   INDSTA   I    A    FOR A GIVEN INTERNAL STA NO (1,2,3) INDSTA
!                      GIVES THE ACTUAL LOCATION IN THE STATION
!                      RELATED ARRAYS FOR THIS STATION.
!                      (EG.   STA NO=ISTANO(INDSTA(1,2,OR 3))
!   INDSAT   I    A    FOR A GIVEN INTERNAL SAT NO (1,2,3) INDSAT
!                      GIVES THE ACTUAL LOCATION IN THE SATELLITE
!                      RELATED ARRAYS FOR THIS SATELLITE.
!                      (EG.   SAT ID=ISATNO(INDSAT(1,2,OR 3))
!   INDSET   I    A    FOR A GIVEN INTERNAL SATELLITE NUMBER INDSET TELL
!                      WHICH SAT SET THAT THE GIVEN SAT BELONGS TO.
!   INPSTA   I    A    POINTER ARRAY THAT RELATES THE INTERNAL STATION
!                      NUMBER (1,2,3) TO THE STATIONS DEFINED IN THE
!                      DATA HEADER RECORDS (N=1,2,3). (EG.
!                      ISTAP=INPSTA(1) WOULD TELL WHICH OF THE THREE
!                      POSSIBLE STATIONS DEFINED IN THE DATA HEADER
!                      RECORDS PERTAINED TO INTERNAL STATION NO. 1)
!   INPSAT   I    A    POINTER ARRAY THAT RELATES THE INTERNAL SATELLITE
!                      NUMBER (1,2,3) TO THE SATELLITES DEFINED IN THE
!                      DATA HEADER RECORDS (N=1,2,3). (EG.
!                      ISATP=INPSAT(1) WOULD TELL WHICH OF THE THREE
!                      POSSIBLE SATELLITES DEFINED IN THE DATA HEADER
!                      RECORDS PERTAINED TO INTERNAL SATELLITE NO. 1)
!   LELEVS   I    A    TELLS WHICH STATION  ELEVATIONS NEED TO BE
!                      COMPUTED FOR MULTI-STATION MEASUREMENTS.(.TRUE.
!                      MEANS COMPUTE THE ELEVATION FOR THIS STATION)
!   MTYPEX
!   ISEQ
!   MSEQ
!   LADD     I    S    CONTROLS WHETHER THE GIVEN SIDE OF A DIFFERENCE
!                      MEASUREMENT IS HANDLED AS POSITIVE(=.TRUE) OR
!                      NEGATIVE(=.FALSE.)
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      use cgmass_module

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
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
      COMMON/ROBTYP/CONVRT(999),OBSCAL(999),RESCAL(999),                &
     &              XMTYP
!
      DIMENSION TPARTL(NM,2)
      DIMENSION AA(1),II(1),LL(1),INDSTA(3,4),INDSAT(3,4),INDSET(3,4),  &
     &   LELEVS(3,4),INPSTA(3,4),INPSAT(3,4)
      DIMENSION SAVEC1(1),SAVEC2(1),ILCORR(5)
      DIMENSION XMM(3)
      DIMENSION UM1(NM,3),UM2(NM,3),UMV(NM,3)

      double precision, dimension( ndim_cgmass, NM )  :: cg_par_array


!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************

!
!WRITE(6,'(A)') 'robsdd: at entry'
! DETERMINE WHICH COMPONENTS MAKE DOUBLE DIFFERENCE
      JTYPE=(MTYPEX-83)/2
! DETERMINE IF RECEIVERS ARE DIFFERENT
      J =1
      J2=1
      L1RCVR=.FALSE.
      MTYPE2=(MTYPEX+1)/2
      J2STA=NDRST2(MTYPE2)
      IF(J2STA.LE.0) GO TO 1000
!...There is a secondary receiving station
      J1STA=NDRSTA(MTYPE2)
      IF(J1STA.EQ.0) GO TO 2000
!...There is a  primary  receiving station
!***************************************************************
!   A pointer error in array JSTANO demonstrated that this code is
!   not correct.  All I*STA and I*SAT values have been changed to
!   J*... variables.   JAM  10/28/91
!     I1STA=INDSTA(J1STA,1)
!     I2STA=INDSTA(J2STA,3)
      IF(J1STA.EQ.J2STA) GO TO 1000
!...They index different block header records.
!        Verify if  station  numbers are really different.
      IF(JSTANO(J1STA).NE.JSTANO(J2STA)) GO TO 2000
!***************************************************************
!...Station numbers are the same or no secondary receiving station
 1000 CONTINUE
      J2SAT=NDRSA2(MTYPE2)
      IF(J2SAT.LE.0) GO TO 3000
!...There is a secondary receiving satellite
      J1SAT=NDRSAT(MTYPE2)
      IF(J1SAT.EQ.0) GO TO 2000
!...There is a  primary  receiving satellite
!***************************************************************
!   A pointer error in array JSTANO demonstrated that this code is
!   not correct.  All I*STA and I*SAT values have been changed to
!   J*... variables.   JAM  10/28/91
!     I1SAT=INDSAT(J1SAT,1)
!     I2SAT=INDSAT(J2SAT,3)
      IF(J1SAT.EQ.J2SAT) GO TO 3000
!...They index different block header records.
!        Verify if satellite numbers are really different.
      IF(JSATNO(J1SAT).NE.JSATNO(J2SAT)) GO TO 2000
!***************************************************************
!...A secondary receiver exists. Sum observation time tag partials
!     into separate arrays.
 2000 CONTINUE
      J2=2
      L1RCVR=.TRUE.
 3000 CONTINUE

! LOOP THRU POSITIVE AND NEGATIVE SIDES OF DIFFERENCE

      DO 10000 I=1,2

      I2L1=2*I-1
! SET COMPONENT SPECIFIER
      ITYPE=ITYPDD(JTYPE,I)

! COMPUTE AND SUM LINK OBSERVATIONS, PARTIALS, AND CORRECTIONS

!WRITE(6,'(A,1x,I4, 3x,L1)') 'robsdd:  CALL TO ROBSD LELEVS(1,I2L1)  ',&
!                                               I2L1,LELEVS(1,I2L1)
!WRITE(6,'(A,2(1x,I4))') 'robsdd:  INDSTA AND INDSAT  ',&
!                                  INDSTA(1,I2L1),INDSAT(1,I2L1)
!WRITE(6,'(A,2(1x,I4))') 'robsdd:  INPSTA AND INPSAT  ',&
!                                  INPSTA(1,I2L1),INPSAT(1,I2L1)

      CALL ROBSD (AA,II,LL,TPARTL(1,1),INDSTA(1,I2L1),INDSAT(1,I2L1),   &
     &   INDSET(1,I2L1),INPSTA(1,I2L1),INPSAT(1,I2L1),LELEVS(1,I2L1),   &
     &   ITYPE,ISEQ,MSEQ,LADD,L1RCVR,ILCORR,SAVEC1,SAVEC2,NM3,ILOOP,    &
     &   XMM,UMV,UM1,UM2, cg_par_array )

! REVERSE SIGN OF NEXT COMPONENT
      LADD=.NOT.LADD
      J=J2
10000 END DO
      RETURN
      END
