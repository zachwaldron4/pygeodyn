
!-----------------------------------
!     end of function interpolateX
!-----------------------------------

!$INTFAC
      SUBROUTINE INTFAC(AA,II,LL)
!********1*********2*********3*********4*********5*********6*********7**
! INTFAC           00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  CONTROL READING  OF GEODYN IIS/IIE INTERFACE FILE
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   LL      I/O   A    LOGICAL DYNAMIC ARRAY
!
!
!
! COMMENTS:
!
! INTERFACE FILE CONSISTS OF:
!
!         INFORMATION ABOUT MAXIMUM BUFFER SPACE REQUIRED TO READ IFF
!         GLOBAL COMMON BLOCKS
!         GLOBAL DYNAMIC ARRAYS
!                1. INTEGRATION AND FORCE MODEL PARAMETERS
!                2. INTERPOLATION
!                3. COORDINATE SYSTEM
!                4. MEASUREMENT MODELING
!                5. STATISTICS
!                6. ESTIMATION
!                7. ESTIMATION A PRIORI
!         EPHEMERIS,POLAR MOTION AND FLUX DATA
!
!         ARC DIRECTORY INFORMATION
!         ARC COMMON BLOCKS               ***
!         ARC DYNAMIC ARRAYS                * REPEATED FOR EACH ARC
!            1 - 7 SAME AS FOR GLOBAL       *
!         TRACKING DATA DIRECTORY RECORDS ***
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CIFF  /IFHEAD,IFLNTH,IFBUFL,IFBUFR,KDYNHD,KDYNLN,          &
     &              KDYNIF,KDYNFF,NXCIFF
      COMMON/CITERM/MAXINR,MININR,MAXLST,IHYPSW,NXITER
      COMMON/CITERG/MARC,MGLOBL,MGLOBM,                                 &
     &              MGLOCV,NXITRG
      COMMON/CORE2E/KORE2E(3,7),KSCL2E(3),NGRP2E,MACH2E,KCOM2E,NXCORE
      COMMON/CORI07/KSMSA1,KSMSA2,KSMSA3,KSMST1,KSMST2,KSMST3,KSMTYP,   &
     &              KSMT1,KSMT2,KMSDM,KIOUT,KISUPE,NXCI07
!
      DIMENSION AA(1)
      DIMENSION II(1)
      DIMENSION LL(1)
!
      DATA LOAD/.FALSE./,LRDIFF/.TRUE./
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      IARCNO=0
!**********************************************************************
! READ FIRST IFF BLOCK AND DETERMINE BUFFER SPACE REQUIREMENTS FOR IFF
!**********************************************************************
      CALL IFRGLB(AA)
!**********************************************************************
! READ GLOBAL COMMON BLOCKS FROM INTERFACE FILE
!**********************************************************************
      CALL IFRGCB(II(KDYNHD),II(KDYNLN),AA(KDYNFF) )
!***********************************************************************
! READ AND STORE GLOBAL DYNAMIC ARRAYS
!***********************************************************************
! INTEGRATION AND FORCE MODEL
      CALL IFRGD1(AA,II,LL,II(KDYNHD),II(KDYNLN),AA(KDYNFF) )
! INTERPOLATION ARRAYS
! COORDINATE SYSTEM
      CALL IFRGD3(AA,II,LL,II(KDYNHD),II(KDYNLN),AA(KDYNFF) )
! MEASUREMENT MODELING
      CALL IFRGD4(AA,II,LL,II(KDYNHD),II(KDYNLN),AA(KDYNFF) )
! STATISTICS
      CALL IFRGD5(AA,II,LL,II(KDYNHD),II(KDYNLN),AA(KDYNFF) )
! ESTIMATION MATRICES
      CALL IFRGD6(AA,II,LL,II(KDYNHD),II(KDYNLN),AA(KDYNFF) )
! SIMULATED DATA
      CALL IFRGD7(AA,II,LL,II(KDYNHD),II(KDYNLN),AA(KDYNFF) )
!*********************************************************************
! READ AND STORE EPHEMERIS DATA
!*********************************************************************
      CALL IFREPH(AA ,II(KDYNHD),AA(KDYNFF) )
      CALL IFREP2(AA,II(KISUPE),II(KDYNHD),AA(KDYNFF) )
!**********************************************************************
! READ AND STORE ARC DIRECTORY INFORMATION
!**********************************************************************
      CALL IFRADR(II(KDYNHD),II(KDYNLN) )
!
!
! LOOP ON ARC INFORMATION
!
!
      DO 500 IARCNO=1,MARC
!**********************************************************************
! READ AND STORE ARC COMMON BLOCK DATA
!**********************************************************************
      CALL IFRACB(AA,II,LL,IARCNO,LRDIFF,LOAD,II(KDYNHD),II(KDYNLN) ,   &
     & AA(KDYNFF) )
!**********************************************************************
! READ AND STORE  ARC DYNAMIC ARRAYS
!**********************************************************************
      CALL IFRAD1(AA,II,LL,IARCNO,LRDIFF,LOAD,II(KDYNHD),AA(KDYNFF) )
      CALL IFRAD2(AA,II,LL,IARCNO,LRDIFF,LOAD,II(KDYNHD),AA(KDYNFF) )
      CALL IFRAD4(AA,II,LL,IARCNO,LRDIFF,LOAD,II(KDYNHD),AA(KDYNFF) )
      CALL IFRAD6(AA,II,LL,IARCNO,LRDIFF,LOAD,II(KDYNHD),AA(KDYNFF) )
      CALL IFRAD7(AA,II,LL,IARCNO,LRDIFF,LOAD,II(KDYNHD),AA(KDYNFF) )
!**********************************************************************
! READ AND STORE ARC TRACKING DATA DIRECTORY INFORMATION
!**********************************************************************
      CALL IFRTDD(AA,II,LL,IARCNO,LRDIFF,II(KDYNHD),AA(KDYNFF) )
!
! END LOOP ON ARCS
!
  500 END DO
!
! LOAD INTEGRATION/INTERPOLATION COEFFICIENTS
!
      CALL COMOVR
      CALL COEFIN
      CALL IGFILL(AA)
      RETURN
      END
