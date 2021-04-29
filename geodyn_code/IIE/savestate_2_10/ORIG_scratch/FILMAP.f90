!$FILMAP
      SUBROUTINE FILMAP(NEQNI,NSTEPS,NINTOT,IR,MEMSAT)
!********1*********2*********3*********4*********5*********6*********7**
! FILMAP
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NEQNI    I    A    NUMBER OF FORCE MODEL EQUATIONS FOR EACH
!                      SATELLITE
!   NSTEPS   I    A    STEPS DEDICATED TO EACH SATELLITE
!   NINTOT   I    S    NUMBER OF SATELLITES FOR SAVING INTEGRATION INFO.
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
      COMMON/IRAPBUF/IBUFBEG(3)
      COMMON/IOS/ISTOS1,ISTOS2,ISTOS3,ISTLST,INTSUB(3),JNTSUB(3),      &
     &        ILSTSG(80),INTOSP,MAPSAT(3,4)
!
!
! MAPSAT(3,4) HAS BEEN DEFINED IN COMMON BLOCK IOS
!     DIMENSION MAPSAT(3,4),NEQNI(NINTOT),NSTEPS(NINTOT)
      DIMENSION             NEQNI(NINTOT),NSTEPS(NINTOT)
      DIMENSION MEMSAT(1)

!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
! ZERO OUT MAPSAT
      DO I=1,3
      MAPSAT(I,1)=0
      MAPSAT(I,2)=0
      MAPSAT(I,3)=0
      MAPSAT(I,4)=0
      ENDDO
!     ZERO OUT BUFFER START LOCATION
      DO JJ=1,3
      IBUFBEG(JJ)=0
      ENDDO
!     REMEMBER WE ARE ASSUMING THAT THE SATELLITES HAVE EQUAL MEMORY
!     OR GET THE MAX MEMORY.
      MEM=0
      DO JJ=1,NINTOT
      MEM=MAX(MEMSAT(JJ),MEM)
      ENDDO
      MAXMEM=MEM*IR

!     STARTING LOCATION FOR THE TWO OR THREE BUFFERS.
      IBUFBEG(1)=1
      DO JJ=2,IR
      IBUFBEG(JJ)=IBUFBEG(JJ-1)+(MAXMEM/IR)
!     write(6,*)' dbg IBUFBEG ',IBUFBEG(JJ),JJ
      ENDDO

!     SUM1=0
!     SUM2=0
!  NOW THE POINTERS IN THIS MAP DENOTE POINTERS AFTER THE
!  BEGINNING OF ANY BUFFER (3 BUFFERS)
!  NOT NINTOT ANYMORE BUT IR
      DO I=1,3
      MAPSAT(I,1)=IBUFBEG(I)
      MAPSAT(I,2)=IBUFBEG(I)+6
      MAPSAT(I,3)=IBUFBEG(I)+6+NEQNI(1)*6
      MAPSAT(I,4)=MAPSAT(I,3)+NSTEPS(1)*3

!     write(6,*)' dbg FILMAP MAPSAT ',MAPSAT(I,1),MAPSAT(I,2),       &
!    & MAPSAT(I,3),MAPSAT(I,4),I
      ENDDO



      RETURN
      END
