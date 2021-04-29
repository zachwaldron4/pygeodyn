!$ICRSITRF
      SUBROUTINE ICRSITRF(ROTMAT,DPSR,XPUT,YPUT,ETUTC,INDPI,MJDSC,FSEC, &
     &           COSTHG,SINTHG,NM,MINTIM,QUAT,NORBFL,XDPOLE,XDOTP,ROT,  &
     &           LEOPT)
!********1*********2*********3*********4*********5*********6*********7**
! ICRCITRF   @espina E. Pavlis
!
! FUNCTION:  This subroutine forms the total rotation matrix from TOR
!            to ECF corrected for polar motion for the times of output
!            of the ORBFIL. Look at comments for individual rotations be

!            Then call ROTQAT to convert a 3X3 rotation matrix to
!            quaternions
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!  ROTMAT   I     A    ROTATION MATRIX FROM MEAN OF 50 TO TOD
!  DPSR
!  XPUT
!  YPUT
!  ETUTC
!  INDPI
!  MJDSC
!  FSEC
!  COSTHG
!  SINTHG
!  NM
!  MINTIM
!  QUAT     O     A    QUATERNIONS FROM  THE TOR-ECF MATRIX
!  NORBFL
!  XDPOLE
!  XDOTP
!  ROT
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

!
      COMMON/CREFMT/REFMT(9)
      COMMON/COBGLO/MABIAS,MCBIAS,MPVECT,MINTPL,MPARAM,MSTA,MOBBUF,     &
     &       MNPITR,MWIDTH,MSIZE ,MPART ,MBUF  ,NXCOBG
      COMMON/POLINF/NPOLE,NPOLEA,NUTA,MXUTC,NMODEL,NXPOLI
!
      DIMENSION ROTMAT(MINTIM,9),RM1(9)
      DIMENSION ROT(3,3)
      DIMENSION DPSR(1),XPUT(1),YPUT(1),ETUTC(1)
      DIMENSION INDPI(1),INDPO(1)
      DIMENSION FSEC(1),FSECD(1)
      DIMENSION DUM1(1),DUM2(1),DUM3(1),NDUM2(1)
      DIMENSION BIHMAT(9),EFMAT(9)
      DIMENSION COSTHG(NM),SINTHG(NM)

      !orig  DIMENSION QUAT(NM,4)
      double precision,dimension(4,NM) :: QUAT

      DIMENSION XDPOLE(3,2,MINTPL)
      DIMENSION XDOTP(3,NPOLE)
!
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
! INITIALIZE RM1 TO 1
      RM1(1)=1.D0
      RM1(2)=0.D0
      RM1(3)=0.D0
      RM1(4)=0.D0
      RM1(5)=1.D0
      RM1(6)=0.D0
      RM1(7)=0.D0
      RM1(8)=0.D0
      RM1(9)=1.D0
!
!     DO 50 I=1, NM
!     WRITE(6,*)'1 ',ROTMAT(I,1),ROTMAT(I,2),ROTMAT(I,3)
!     WRITE(6,*)'1 ',ROTMAT(I,4),ROTMAT(I,5),ROTMAT(I,6)
!     WRITE(6,*)'1 ',ROTMAT(I,7),ROTMAT(I,8),ROTMAT(I,9)
!  50 CONTINUE

!   GET ROTATION FROM TRUE OF REFERENCE TO TRUE OF DATE
!   THIS IS DONE BY GOING FROM TOR(J2000) TO MEAN OF J2000 AND
!   THEN FROM MEAN OF J2000 TO TRUE OF DATE
!   REFMT TRANSPOSED  GOES FROM TRUE OF J2000 TO MEAN OF J2000
!   ROTMAT GOES FROM MEAN OF J2000  TO TRUE OF DATE
!
!
      IF(NORBFL.GT.2) THEN
      WRITE(6,*)' INDEX FOR OUTPUT COORDINATE SYSTEM IS INVALID'
      WRITE(6,*)' CHECK ORBFIL OPTION '
      STOP
      ENDIF
!
      DO 100 I=1,NM
      IF(NORBFL.EQ.0) GO TO 200
!
      IF(NORBFL.EQ.2) THEN
      DO J=1,9
      RM1(J)=ROTMAT(I,J)
      ENDDO
      GOTO 200
      ENDIF
!
      RM1(1)=ROTMAT(I,1)*REFMT(1)+ROTMAT(I,4)*REFMT(2)+                 &
     &       ROTMAT(I,7)*REFMT(3)
      RM1(2)=ROTMAT(I,2)*REFMT(1)+ROTMAT(I,5)*REFMT(2)+                 &
     &       ROTMAT(I,8)*REFMT(3)
      RM1(3)=ROTMAT(I,3)*REFMT(1)+ROTMAT(I,6)*REFMT(2)+                 &
     &       ROTMAT(I,9)*REFMT(3)
      RM1(4)=ROTMAT(I,1)*REFMT(4)+ROTMAT(I,4)*REFMT(5)+                 &
     &       ROTMAT(I,7)*REFMT(6)
      RM1(5)=ROTMAT(I,2)*REFMT(4)+ROTMAT(I,5)*REFMT(5)+                 &
     &       ROTMAT(I,8)*REFMT(6)
      RM1(6)=ROTMAT(I,3)*REFMT(4)+ROTMAT(I,6)*REFMT(5)+                 &
     &       ROTMAT(I,9)*REFMT(6)
      RM1(7)=ROTMAT(I,1)*REFMT(7)+ROTMAT(I,4)*REFMT(8)+                 &
     &       ROTMAT(I,7)*REFMT(9)
      RM1(8)=ROTMAT(I,2)*REFMT(7)+ROTMAT(I,5)*REFMT(8)+                 &
     &       ROTMAT(I,8)*REFMT(9)
      RM1(9)=ROTMAT(I,3)*REFMT(7)+ROTMAT(I,6)*REFMT(8)+                 &
     &       ROTMAT(I,9)*REFMT(9)
!     WRITE(6,*)' ',RM1(1),RM1(2),RM1(3)
!     WRITE(6,*)' ',RM1(4),RM1(5),RM1(6)
!     WRITE(6,*)' ',RM1(7),RM1(8),RM1(9)
!
  200 CONTINUE
!
      FSECD(1)=FSEC(I)
      IF(LEOPT) THEN
      CALL INPOLP(ETUTC,DPSR,XPUT,YPUT,.FALSE.,.FALSE.,.FALSE.,MJDSC,   &
     &            FSECD,1,DUM1,INDPI,NDUM1,NDUM2,INDPO,DUM2,BIHMAT,DUM3,&
     &            XDPOLE,XDOTP)
      ELSE
      CALL INPOLE(ETUTC,DPSR,XPUT,YPUT,.FALSE.,.FALSE.,.FALSE.,MJDSC,   &
     &            FSECD,1,DUM1,INDPI,NDUM1,NDUM2,INDPO,DUM2,BIHMAT,DUM3,&
     &            XDPOLE,XDOTP)
      ENDIF
!
! GET ROTATION MATRIX FROM TOD TO ECF (INCLUDING POLAR MOTION CORRECTION
         EFMAT(1)=BIHMAT(1)*COSTHG(I)-BIHMAT(2)*SINTHG(I)
         EFMAT(2)=BIHMAT(4)*COSTHG(I)-BIHMAT(5)*SINTHG(I)
         EFMAT(3)=BIHMAT(7)*COSTHG(I)-BIHMAT(8)*SINTHG(I)
         EFMAT(4)=BIHMAT(1)*SINTHG(I)+BIHMAT(2)*COSTHG(I)
         EFMAT(5)=BIHMAT(4)*SINTHG(I)+BIHMAT(5)*COSTHG(I)
         EFMAT(6)=BIHMAT(7)*SINTHG(I)+BIHMAT(8)*COSTHG(I)
         EFMAT(7)=BIHMAT(3)
         EFMAT(8)=BIHMAT(6)
         EFMAT(9)=BIHMAT(9)
!        write(6,*)' EFMAT ',efmat(1),efmat(2),efmat(3)
!        write(6,*)' EFMAT ',efmat(4),efmat(5),efmat(6)
!        write(6,*)' EFMAT ',efmat(7),efmat(8),efmat(9)
!
! GET ROTATION MATRIX FROM TOR TO ECF (INCLUDING POLAR MOTION CORRECTION
! EFMAT GOES FROM TRUE OF DATE TO ECF
! RM1   GOES FROM TOR(J2000) TO TOD
! ROTMAT GOES FROM TOR(J2000) TO ECF
         ROT(1,1)=EFMAT(1)*RM1(1)+EFMAT(4)*RM1(2)+                      &
     &   EFMAT(7)*RM1(3)
         ROT(1,2)=EFMAT(2)*RM1(1)+EFMAT(5)*RM1(2)+                      &
     &   EFMAT(8)*RM1(3)
         ROT(1,3)=EFMAT(3)*RM1(1)+EFMAT(6)*RM1(2)+                      &
     &   EFMAT(9)*RM1(3)
         ROT(2,1)=EFMAT(1)*RM1(4)+EFMAT(4)*RM1(5)+                      &
     &   EFMAT(7)*RM1(6)
         ROT(2,2)=EFMAT(2)*RM1(4)+EFMAT(5)*RM1(5)+                      &
     &   EFMAT(8)*RM1(6)
         ROT(2,3)=EFMAT(3)*RM1(4)+EFMAT(6)*RM1(5)+                      &
     &   EFMAT(9)*RM1(6)
         ROT(3,1)=EFMAT(1)*RM1(7)+EFMAT(4)*RM1(8)+                      &
     &   EFMAT(7)*RM1(9)
         ROT(3,2)=EFMAT(2)*RM1(7)+EFMAT(5)*RM1(8)+                      &
     &   EFMAT(8)*RM1(9)
         ROT(3,3)=EFMAT(3)*RM1(7)+EFMAT(6)*RM1(8)+                      &
     &   EFMAT(9)*RM1(9)
!
!orig       CALL ROTQAT(ROT,QUAT(I,1))
!orig !     WRITE(6,*)' dbg QUAT ',QUAT(I,1),QUAT(I,2),QUAT(I,3),QUAT(I,4)

      CALL ROTQAT(ROT, QUAT(1,I) )

      !write(6,'(A,1x,I9, 4(1x,E15.7))') &
      !      'icrsitrf: I, QUAT(1:4,I)  ', &
      !                 I, QUAT(1,I),QUAT(2,I),QUAT(3,I),QUAT(4,I)

  100 END DO
!
      RETURN
      END
