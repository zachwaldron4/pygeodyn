!$RESON
      SUBROUTINE RESON(PARAM,XS,XD,FS,GRPAR,ITACCX,NEQN,STACC,VMAT,     &
     &                 IACCP)
!********1*********2*********3*********4*********5*********6*********7**
! RESON            00/00/00            0000.0    PGMR - OSCAR COLOMBO
!
! FUNCTION:  CALCULATE RESONANT ACCELERATIONS AND PARTIALS
!            ASSOCIATED WITH GRAVITY, RADIATION PRESSURE, ETC.
!            ACCORDING TO THEORY GIVEN IN: "THE DYNAMICS OF
!            GPS ORBITS AND THE DETERMINATION OF PRECISE
!            EPHEMERIDES" BY OSCAR L. COLOMBO, J.GEOPHYS.RES.
!            (RED) PAPER NO. 89JB00352, 1989.
!
!
! PROGRAMMED BY: OSCAR L. COLOMBO. UNIVERSITY OF MARYLAND. JUNE, 1989.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PARAM    I    A    VECTOR RESONANT PARAMETERS (CURRENT VALUES)
!                      PARAM IS ORGANIZED AS FOLLOWS:
!                             PARAMETERS ARE THE AMPLITUDES OF SINE,
!                             COSINE, AND CONSTANT TERM ALONG, ACROSS,
!                             AND RADIAL:
!                             PARAM(1,2,3) = COS, SIN, CTE--ALONG;
!                             PARAM(4,5,6) = COS, SIN, CTE--ACROSS;
!                             PARAM(7,8,9) = COS, SIN, CTE--RADIAL.
!   XS       I    A    SATELLITE POSITION VECTOR
!   XD       I    A    SATELLITE VELOCITY VECTOR
!                      (XS, XD ARE IN INERTIAL,EQUATORIAL COORDS)
!   FS       O    A    TOTAL ACCELERATION VECTOR ON SATELLITE
!   GRPAR    O    A    ARRAY OF PARTIALS FOR RESONANT MODEL
!                             GRPAR IS ORGANIZED AS FOLLOWS (SEE ALSO
!                             ARRAY "PARAM" ABOVE).
!                             GRPAR(SIN   ALONG TRACK; X,Y,Z)
!                             GRPAR(COS   ALONG TRACK; X,Y,Z)
!                             GRPAR(CONST ALONG TRACK; X,Y,Z)
!
!                             GRPAR(SIN   CROSS TRACK; X,Y,Z)
!                             GRPAR(COS   CROSS TRACK; X,Y,Z)
!                             GRPAR(CONST CROSS TRACK; X,Y,Z)
!
!                             GRPAR(SIN   RADIAL TRACK; X,Y,Z)
!                             GRPAR(COS   RADIAL TRACK; X,Y,Z)
!                             GRPAR(CONST RADIAL TRACK; X,Y,Z)
!   ITACCX   I    A    ARRAY CONTAINING INFORMATION ON PARAMETER
!                      ADJUSTMENT (>100 MEANS ADJ.)
!   NEQN     I    S    NUMBER OF FORCE MODEL EQUATIONS
!   STACC              STOP TIMES OF GENACC PERIODS (SECONDS SINCE
!                      GEODYN REFERENCE TIME)
!
! OTHER SUBPROGRAMS CALLED BY THIS SUBROUTINE
!
!   VNORM                     NORMALIZES A VECTOR
!   SPROD                     COMPUTES SCALE FACTOR OF TWO VECTORS
!   VPROD                     COMPUTES THE VECTOR PRODUCT OF TWO
!                             VECTORS.
!
! *************** NOTICE: ALL DISTANCES ARE IN METERS; ALL VELOCITIES
!                 ARE IN METERS PER SECOND; ALL ACCELERATIONS ARE IN
!                 METER PER SECONDS SQUARED ************************
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z),LOGICAL (L)
      SAVE
      COMMON/CBDTRU/BDTRUE(7,999)
      COMMON/CINTL/LORBIT,LORBVE,LNDRAG,LNSLRD,LDRADJ,LSRADJ,           &
     &             LBACKW,LSETS
      COMMON/CRMI/RMI(9)
      COMMON/SETAPT/                                                    &
     &       JSADRG(1),JSASRD(1),JSAGA(1),JFAADJ,JTHDRG,JACBIA,JATITD,  &
     &       JDSTAT,JDTUM ,                                             &
     &       JSACS, JSATID,JSALG,JSAGM,JSAKF,JSAXYP,JSADJ2,JSAPGM,      &
     &       JFGADJ,JLTPMU,JLTPAL,JL2CCO,JL2SCO,JL2GM,JLRELT,           &
     &       JLJ2SN,JLGMSN,JLPLFM,JL2AE,JSAXTO,JSABRN
      COMMON/SETIOR/IORDRG,IORSRD,IORGA,IPDDRG,IPDSRD,IPDGA
      COMMON/STARTT/ESSTRT,FSSTRT
      DIMENSION XS(3),XD(3),Y(3),X(3),Z(3),XDN(3),                      &
     &         XN(3,3),RTX(3,3),XO(3),YO(3),XON(3),YON(3),              &
     &         PARTL(3),FF(3),FS(3),DPARTL(3,3,3),GRPAR(NEQN,3),        &
     &         XE(3),YE(3),ZE(3),WN(3,3),ITYPE(3)
      DIMENSION ITACCX(1),STACC(1),PARAM(1)
      DIMENSION XAXIS(3),YAXIS(3),ZAXIS(3),VMAT(3,6)
      DIMENSION DXAXIS(3,3),DYAXIS(3,3),DZAXIS(3,3),DBDX(3)
      DIMENSION IACCP(2)
      DATA ZE/0.D0,0.D0,1.D0/,XE/1.D0,0.D0,0.D0/,YE/0.D0,1.D0,0.D0/
      DATA ZERO/0.0D0/,ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      LFIRST=.TRUE.
      IACCP(1)=0
      IACCP(2)=0
      CALL TESTS(XS,BDTRUE(1,8),XAXIS,YAXIS,ZAXIS,DXAXIS,DYAXIS,DZAXIS, &
     &           DBDX,.TRUE.)
      LX=.FALSE.
      LY=.FALSE.
      LZ=.FALSE.
!
! NPAR = MAXIMUM NUMBER OF RESONANT TERMS IN EACH DIRECTION
!       (ALONG,ACROSS,RADIAL): ONE SINE, ONE COSINE, ONE CONSTANT.
      NPAR = 3
!
! IPDA IS THE EXACT NUMBER OF STOP TIMES SPECIFIED
! JPDGA IS THE NUMBER OF DISCRETE TIME INTERVALS FOR THE ARC
!
! DETERMINE WHICH GENACC TIME PERIOD APPLIES
         TOTTIM=ESSTRT+FSSTRT
         DO 5 I=9,IPDGA,9
! IF TIME IS LESS THAN PERIOD STOP TIME, SET PERIOD POINTER
            IF(TOTTIM.LE.STACC(I)) THEN
               JPRD1=I-8
               JPRD2=I
               GOTO 7
            ENDIF
    5    CONTINUE
    7    CONTINUE
      IND=0
      JND=JPRD1
      ITYPE(1)=0
      ITYPE(2)=0
      ITYPE(3)=0
      DO 8 K=1,3
      WN(1,K)=ZERO
      WN(2,K)=ZERO
      WN(3,K)=ZERO
      IF(ITACCX(JND).GE.0.AND.ITACCX(JND).LE.9999) THEN
         IND=IND+1
         ITYPE(IND)=MOD(ITACCX(JND),100)/10
      ELSE
         IF(ITACCX(JND+1).GE.0.AND.ITACCX(JND+1).LE.9999) THEN
            IND=IND+1
            ITYPE(IND)=MOD(ITACCX(JND+1),100)/10
         ELSE
            IF(ITACCX(JND+2).GE.0.AND.ITACCX(JND+2).LE.9999) THEN
               IND=IND+1
               ITYPE(IND)=MOD(ITACCX(JND+2),100)/10
            ENDIF
         ENDIF
      ENDIF
      JND=JND+3
    8 END DO
      NPACT=IND
      IF(NPACT.LE.0) THEN
         WRITE(6,6000)
         STOP
      ENDIF
!
!            COMPUTING RESONANT ACCELERATIONS FOR GPS .
!
!            FIRST, FIND THE ORTHONORMAL VECTORS XN(*,1), XN(*,2),
!                   XN(*,3) (SEE DESCRIPTION BELOW).
!
      CALL VNORM(XS,XN(1,3),RNORM)
      CALL VNORM(XD,XDN,RNORM)
      CALL VPROD(XN(1,3),XDN,Y)
      CALL VPROD(Y,XN(1,3),X)
      CALL VNORM(X,XN(1,1),RNORM)
      CALL VNORM(Y,XN(1,2),RNORM)
!
!            XN(*,1) IS ALONG TRACK, XN(*,2) IS ACROSS TRACK, XN(*,3)
!            IS RADIAL. ALL THREE ARE UNIT VECTORS.
!
!            WN(*,1), WN(*,2), WN(*,3) ARE THE (UP TO) 3 DIRECTIONS
!            MODELLED (UNIT VECTORS). EVENTUALLY, THESE DIRCTIONS
!            MAY BE ANY ONE OF 9 POSSIBILITIES. FOR NOW, THE
!            POSSIBILITIES ARE RESTRICTED TO ALONG, CROSS, RADIAL
!            AND GPS YBIAS.
!
      DO 9 I=1,NPACT
      IF(ITYPE(I).GE.1.AND.ITYPE(I).LE.3) THEN
         WN(1,I)=XN(1,ITYPE(I))
         WN(2,I)=XN(2,ITYPE(I))
         WN(3,I)=XN(3,ITYPE(I))
         IF(ITYPE(I).EQ.3) THEN
           LZ=.TRUE.
           ILZ=I
         ENDIF
      ELSE
         IF(ITYPE(I).EQ.5.OR.ITYPE(I).EQ.6) THEN
!   GPS X-AXIS (X-BIAS)
            IF(ITYPE(I).EQ.5) THEN
!               SIGNZ=-ONE
!               CALL BDAXES(XS(1),XMNSNS(1,2),WN(1,I),YAXIS,ZAXIS,SIGNZ,
!     1                     VMAT,VMAT,1,.TRUE.,.TRUE.)
            WN(1,I)=XAXIS(1)
            WN(2,I)=XAXIS(2)
            WN(3,I)=XAXIS(3)
            ILX=I
            LX=.TRUE.
            ENDIF
!   GPS Y-AXIS (Y-BIAS)
  425 CONTINUE
            IF(ITYPE(I).EQ.6) THEN
!               SIGNZ=-ONE
!               CALL BDAXES(XS(1),XMNSNS(1,2),XAXIS,WN(1,I),ZAXIS,SIGNZ,
!     1                     VMAT,VMAT,1,.TRUE.,.TRUE.)
            WN(1,I)=YAXIS(1)
            WN(2,I)=YAXIS(2)
            WN(3,I)=YAXIS(3)
            ILY=I
            LY=.TRUE.
            ENDIF
         ELSE
           WRITE(6,6001) ITYPE(I)
           STOP
         ENDIF
      ENDIF
    9 END DO
!
!            FIND THE TRANSFORMATION, ORTHOGONAL MATRIX  RTX
!            FROM EQUATORIAL TO WN
!
      DO 12    I = 1,3
      RTX(1,I) = WN(I,1)
      RTX(2,I) = WN(I,2)
      RTX(3,I) = WN(I,3)
   12 END DO
!
!            FOR A NON-EQUATORIAL ORBIT,
!            FIND THE SINE AND THE COSINE OF THE "ORBIT ANGLE" N0T
!            BETWEEN THE POSITION VECTOR "XS" AND
!            THE INTERSECTION OF THE ORBIT PLANE AND EQUATOR (POSITIVE
!            TOWARDS ASCENDING NODE)
!
      IF(ABS(XN(1,2)).LT.1.D-2.AND.ABS(XN(2,2)).LT.1.D-2) GO TO 15
      CALL VPROD(ZE,XN(1,2),XO)
!
!            ORBIT IS NON-EQUATORIAL. PROCEEDED AS EXPLAINED ABOVE.
!
      CALL VNORM(XO,XON,RNORM)
      CALL VPROD(XN(1,2),XON,YO)
      CALL VNORM(YO,YON,RNORM)
      CALL SPROD(XN(1,3),XON,COSN0T)
      CALL SPROD(XN(1,3),YON,SINN0T)
      GO TO 20
!
!     ORBIT IS VIRTUALLY EQUATORIAL, SO LINE OF NODES IS NOT DEFINED.
!     USE INERTIAL, EQUATORIAL X AXIS (POSITIVE TO VERNAL EQUINOX) AS
!     ORIGIN FOR THE ANGLE  N0T, INSTEAD.
!
   15 CALL SPROD(XN(1,3),XE,COSN0T)
      CALL SPROD(XN(1,3),YE,SINN0T)
   20 CONTINUE
!
!            COMPUTE HERE THE EMPIRICAL TERMS REPRESENTING THE
!            RESONANT ACCELERATIONS AND CORRESPONDING PARTIALS, FOR THE
!            ALONG-TRACK, ACROSS-TRACK, AND RADIAL DIRECTIONS.
!
!            THE GENERAL FORM IS:
!
!              F   = A  *COSN0T + B  *SINN0T+ C
!               I     I            I           I
!
!            WHERE  I = 1, 2, 3  FOR ALONG-, ACROSS-TRACK, AND RADIAL.
!
      PARTL(1) = COSN0T
      PARTL(2) = SINN0T
      PARTL(3) = ONE
!
!           FIRST, COMPUTE ESTIMATED RESONANT ACCELERATIONS USING THE
!           VALUES OF THE PARAMETERS IN "PARAM" FROM THE PRECEEDING
!           ITERATION. THIS PART IS DONE IN THE ORBITAL COORDINATES.
!           THE RESULT IS PUT IN ARRAY 'FF'.
!
      DO 22    I = 1,3
      FF(I) = 0.D0
      DO 22    K = 1,NPAR
      JP = JPRD1+K+(I-1)*3-1
   22 FF(I) = PARAM(JP)*PARTL(K)+FF(I)
!
!          CONVERT "FF" INTO "FS" IN EQUATORIAL COORDINATES.
!
      DO 25   I = 1,3
   25 FS(I) = FF(1)*WN(I,1)+FF(2)*WN(I,2)+FF(3)*WN(I,3)
      IF(LORBIT) RETURN
      IF(LX) THEN
        DO 27 I=1,3
        DO 26 J=1,3
        VMAT(I,J)=VMAT(I,J)+FF(ILX)*DXAXIS(I,J)
   26   CONTINUE
   27   CONTINUE
      ENDIF
      IF(LY) THEN
        DO 29 I=1,3
        DO 28 J=1,3
        VMAT(I,J)=VMAT(I,J)+FF(ILY)*DYAXIS(I,J)
   28   CONTINUE
   29   CONTINUE
      ENDIF
      IF(LZ) THEN
        DO 33 I=1,3
        DO 32 J=1,3
        VMAT(I,J)=VMAT(I,J)-FF(ILZ)*DZAXIS(I,J)
   32   CONTINUE
   33   CONTINUE
      ENDIF
!
!           FIND EXPLICIT RESONANT ACCELERATION PARTIALS
!           IN EQUATORIAL COORDINATES.
!
! K LOOPS THROUGH PARAMETERS (A,B,C OR COS, SIN, CONST)
! J LOOPS THROUGH X,Y,Z
! I LOOPS THROUGH ACROSS, ALONG, RADIAL
      DO 35    K = 1,NPAR
      DO 35    J = 1,3
      DO 35    I = 1,3
   35 DPARTL(I,J,K) = RTX(I,J)*PARTL(K)
!
      IPTR=JSAGA(1)
      DO 40    NPT=1,IPDGA
      IF(ITACCX(NPT).LT.100) GOTO 40
      IF(ITACCX(NPT).GT.1000) GOTO 40
        IF(LFIRST) IACCP(1)=IPTR
        LFIRST=.FALSE.
        IACCP(2)=IPTR
        IF(NPT.GE.JPRD1 .AND. NPT.LE.JPRD2) THEN
         I=(NPT-JPRD1)/3+1
         K=(NPT-JPRD1)-(I-1)*3+1
         GRPAR(IPTR,1) = DPARTL(I,1,K)*RMI(1)+DPARTL(I,2,K)*RMI(2)+     &
     &                   DPARTL(I,3,K)*RMI(3)
         GRPAR(IPTR,2) = DPARTL(I,1,K)*RMI(4)+DPARTL(I,2,K)*RMI(5)+     &
     &                   DPARTL(I,3,K)*RMI(6)
         GRPAR(IPTR,3) = DPARTL(I,1,K)*RMI(7)+DPARTL(I,2,K)*RMI(8)+     &
     &                   DPARTL(I,3,K)*RMI(9)
        ELSE
            DO 50 J = 1,3
            GRPAR(IPTR,J) = ZERO
   50       CONTINUE
        ENDIF
      IPTR=IPTR+1
   40 END DO
!
      RETURN
 6000 FORMAT(' EXECUTION TERMINATING IN RESON. NO DIRECTIONS FOUND.')
 6001 FORMAT(' EXECUTION TERMINATING IN RESON. INVALID DIRECTION # ',   &
     &        I10)
      END
