!$PDFSAT
      SUBROUTINE PDFSAT(WORD10,FSECM,XSM,VSM,SATLAT,SATLON,SATH,M)
!********1*********2*********3*********4*********5*********6*********7**
! PDFSAT           86/06/03            8606.0    PGMR - TOM MARTIN
!
! FUNCTION:  LOAD S/C INERTIAL POS/VEL AND GEODETIC PHI,LAMBDA,H
!            INTO PARTIAL DERIVATIVE FILE OUTPUT RECORD.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   WORD10  I/O   A    PARTIALLY FILLED OUTPUT RECORD AS INPUT.
!                      MODIFIED OUTPUT RECORD AS OUTPUT.
!   FSECM    I    A    ELAPSED SECONDS ET FROM BLOCK START TIME
!                      FOR ALL OBSERVATIONS IN BLOCK
!   XSM      I    A    INERTIAL S/C POSITION FOR ALL OBS IN BLOCK
!   VSM      I    A    INERTIAL S/C VELOCITY FOR ALL OBS IN BLOCK
!   SATLAT   I    S    GEODETIC LATITUDE OF S/C FOR THIS OBS.
!   SATLON   I    S    EAST LONGITUDE OF S/C FOR THIS OBS.
!   SATH     I    S    ELLIPSOIDAL HEIGHT OF S/C FOR THIS OBS.
!   M        I    S    POINTER TO FSECM ARRAY INDICATING OBSERVATION
!                      TIME
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/COBGLO/MABIAS,MCBIAS,MPVECT,MINTPL,MPARAM,MSTA,MOBBUF,     &
     &       MNPITR,MWIDTH,MSIZE ,MPART ,MBUF  ,NXCOBG
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      DIMENSION WORD10(10),FSECM(MINTIM),XSM(MINTIM,3),VSM(MINTIM,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!.....ELAPSED SECONDS ET SINCE START OF OBSERVATION BLOCK
      WORD10( 1)=FSECM(M)
!.....INERTIAL S/C POSITION AND VELOCITY
      DO 2000 I=1,3
      WORD10( 1+I)=XSM(M,I)
      WORD10( 4+I)=VSM(M,I)
 2000 END DO
!.....GEODETIC LATITUDE
      WORD10( 8)=SATLAT
!.....EAST LONGITUDE
      WORD10( 9)=SATLON
!.....ELLIPSOIDAL HEIGHT
      WORD10(10)=SATH
      RETURN
      END
