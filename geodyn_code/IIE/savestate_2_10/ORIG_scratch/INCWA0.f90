!$INCWA0
      SUBROUTINE INCWA0(IOL1,NEQNA3,                                    &
     &                  SUMXA,XDDOTA,SUMPXA,PXDDTA,SUMXR,XRDOT,         &
     &                SUMXA1,XDDTA1,SMPXA1,PDDTA1,SUMXR1,XRDOT1,        &
     &                  SUMXA2,XDDTA2,SMPXA2,PXDDA2,SUMXR2,XRDOT2,      &
     &                SMXA22,XDDA21,SMPA21,PXDD21,SMXR21,XRDT21)
!********1*********2*********3*********4*********5*********6*********7**
! INCWA0           13/10/02            8208.0    PGMR - DDR, JTW
!
! FUNCTION:  THE VALUES IN ARRAYS SUMXA - XRDOT ARE WRITTEN INTO
!            ARRAYS SUMXA1 - XRDOT1
!            THESE ARRAYS ALLOW FOR MULTIPLE SATELLITE
!            EPOCHS FOR THE LASTSN OPTION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/AXIS/LINTAX
      COMMON/LASTER/LASTR(2),LBINAST,NXASTR
      COMMON/LDUAL/LASTSN,LSTSNX,LSTSNY
      COMMON/XPCNT /NXTRAC,NXFLAG,NXALLC,NCPPER,NMINRT,NDPOR,NXXTRA
      DIMENSION SUMXA(3,2),                                             &
     &   XDDOTA(3,IOL1),SUMPXA(NEQNA3,2),                               &
     &   PXDDTA(NEQNA3,IOL1)
      DIMENSION SUMXR(NDPOR,2),XRDOT(NDPOR,IOL1)
!
      DIMENSION SUMXA1(3,2),                                            &
     &   XDDTA1(3,IOL1),SMPXA1(NEQNA3,2),                               &
     &   PDDTA1(NEQNA3,IOL1)
      DIMENSION SUMXR1(NDPOR,2),XRDOT1(NDPOR,IOL1)
!
      DIMENSION SUMXA2(3,2),                                            &
     &   XDDTA2(3,IOL1),SMPXA2(NEQNA3,2),                               &
     &   PXDDA2(NEQNA3,IOL1)
      DIMENSION SUMXR2(NDPOR,2),XRDOT2(NDPOR,IOL1)
!
      DIMENSION SMXA21(3,2),                                            &
     &   XDDA21(3,IOL1),SMPA21(NEQNA3,2),                               &
     &   PXDD21(NEQNA3,IOL1)
      DIMENSION SMXR21(NDPOR,2),XRDT21(NDPOR,IOL1)
!
      DO I=1,6
       SUMXA1(I,1)=SUMXA(I,1)
      ENDDO
!
      NE=3*IOL1
      DO I=1,NE
       XDDTA1(I,1)=XDDOTA(I,1)
      ENDDO

!
      NE=2*NEQNA3
      DO I=1,NE
       SMPXA1(I,1)=SUMPXA(I,1)
      ENDDO

!
      NE=NEQNA3*IOL1
      DO I=1,NE
       PDDTA1(I,1)=PXDDTA(I,1)
      ENDDO
!
      IF(.NOT.LBINAST) GO TO 100
!
! SECONDAY ASTEROID
!
      DO I=1,6
       SMXA21(I,1)=SUMXA2(I,1)
      ENDDO
!
      NE=3*IOL1
      DO I=1,NE
       XDDA21(I,1)=XDDTA2(I,1)
      ENDDO

!
      NE=2*NEQNA3
      DO I=1,NE
       SMPA21(I,1)=SMPXA2(I,1)
      ENDDO

!
      NE=NEQNA3*IOL1
      DO I=1,NE
       PXDD21(I,1)=PXDDA2(I,1)
!
      ENDDO
!
 100  CONTINUE
!
      IF(.NOT.LINTAX) RETURN
!
      DO I=1,2*NDPOR
       SUMXR1(I,1)=SUMXR(I,1)
      ENDDO
!
      NE=NDPOR*IOL1
      DO I=1,NE
       XRDOT1(I,1)=XRDOT(I,1)
      ENDDO
!
      IF(.NOT.LBINAST) RETURN
!
      DO I=1,2*NDPOR
       SMXR21(I,1)=SUMXR2(I,1)
      ENDDO
!
      NE=NDPOR*IOL1
      DO I=1,NE
       XRDT21(I,1)=XRDOT2(I,1)
      ENDDO
!
      RETURN
      END
