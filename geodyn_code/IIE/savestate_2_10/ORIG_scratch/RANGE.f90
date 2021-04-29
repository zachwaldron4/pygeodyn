!$RANGE
      SUBROUTINE RANGE (X2,X1,UR,R,WORK,NM,LPARTL)
!********1*********2*********3*********4*********5*********6*********7**
! RANGE            83/04/01            8304.0    PGMR - TOM MARTIN
!
! FUNCTION:  COMPUTE THE SLANT RANGE FROM ONE POINT TO ANOTHER
!            ALSO COMPUTE ANY NECESSARY POSITION PARTIALS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   X2       I    A    POSITION VECTORS OF POINT #1
!   X1       I    A    POSITION VECTORS OF POINT #2
!   UR       O    A    UNIT VECTORS FROM POINT #1 TO POINT #2
!   R        O    A    SLANT RANGE FROM POINT #1 TO POINT #2
!   WORK    I/O   A
!   NM       I    S    NUMBER OF MEASUREMENTS
!   LPARTL   I    S   .TRUE. WHEN PARTIALS REQUIRED
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      DIMENSION X2(MINTIM,3),X1(MINTIM,3),UR(NM,3),R(NM),WORK(NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! FORM THE RELATIVE VECTOR FROM X1 TO X2
      DO 2000 I=1,3
      DO 1000 N=1,NM
      UR(N,I)= X2(N,I)-X1(N,I)
 1000 END DO
 2000 END DO
! COMPUTE THE SQUARE OF THE SLANT RANGE
      CALL DOTPRD(UR,UR,WORK,NM,NM,NM,3)
! TAKE THE SQUARE ROOT TO OBTAIN THE RANGE
      DO 6000 N=1,NM
      R(N)=SQRT(WORK(N))
 6000 END DO
! RETURN IF NO PARTIALS ARE REQUIRED
      IF(.NOT.LPARTL) RETURN
! RANGE PARTIALS ARE SIMPLY THE UNIT VECTOR. THIS SAME UNIT VECTOR
!       IS ALSO THE PARTIAL OF RANGE RATE W.R.T. VELOCITY
      DO 8000 I=1,3
      DO 7000 N=1,NM
      UR(N,I)=UR(N,I)/R(N)
 7000 END DO
 8000 END DO
      RETURN
      END
