!$ARCBHD
      SUBROUTINE ARCBHD
!********1*********2*********3*********4*********5*********6*********7**
! ARCBHD           85/03/25            8504.0    PGMR - D. ROWLANDS
!
!  FUNCTION:  WRITE OUT ARC HEADER INFORMATION
!             FOR BINARY RESIDUAL FILE
!
!
!
! I/O PARAMETERS: NONE
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBINRL/LBINR,LLOCR,LODR,LBNCR,LBINRI,LLOCRI,LODRI,LBNCRI,  &
     &              NXCBIN
! /CEDIT / EDITING MULTIPLIER, EDITING RMS, AND EDIT LEVEL
      COMMON/CEDIT /EDITX ,EDTRMS,EDLEVL,CONVRG,GLBCNV,EBLEVL,EDITSW,   &
     &              ENPX  ,ENPRMS,ENPCNV,EDBOUN,FREEZI,FREEZG,FREEZA,   &
     &              XCEDIT
!                   EDITX  - EDIT MULTIPLIER (D=3.5)
!                   EDTRMS - RMS FOR FIRST ITERATION EDITING(D=200.)
!                   EDLEVL - EDITX*EDTRMS(SET BY IIE)
!                   CONVRG - INNER ITERATION CONVERGENCE CRITERIA(D=.02)
!                   GLBCNV - GLOBAL ITERATION CONVERGENCE (D=.02)
!
! /CITER / ARC NUMBER & GLOBAL AND INNER ITERATION COUNTS
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DATA ZERO/0.D0/
      DATA ONEM/-1.D0/,ONE/1.D0/,TWO/2.D0/,W1/-1000000000000.D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      W2=DBLE(NARC)
      W3=DBLE(NGLOBL)
      W4=DBLE(NINNER)
      W5=EDITX
      W6=EDTRMS
      W7=ZERO
      IF(LLOCRI) W7=ONE
      W8=ZERO
      IF(LODRI ) W8=ONE
      IF(LBNCRI) W8=TWO
!
!CC      WRITE(6,*) 'arcbhd: header ',
!CC     &              W1,W2,W3,W4,W5,W6,W7,W8,W1,W1,
!CC     &              W1,W2,W3,W4,W5,W6,W7,W8,W1,W1
!
      WRITE(IUNT19) W1,W2,W3,W4,W5,W6,W7,W8,W1,W1,                      &
     &              W1,W2,W3,W4,W5,W6,W7,W8,W1,W1
      RETURN
      END
