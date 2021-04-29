!$CGRVPD
!********1*********2*********3*********4*********5*********6*********7**
! CGRVPD           00/00/00            9611.01   PGMR - S.LUO
!
!
! FUNCTION:  BLOCK DATA SUBROUTINE FOR COMMON PDOBGR CONTAINING PHOBOS
!            (DEIMOS) 4x4 GRAVITY FIELD
!
! COMMENTS: 1. B.FONG CHAO AND DAVID P.RUBINCAM : THE GRAVITATIONAL
!              FIELD OF PHOBOS    Geophysical Research Letters,
!                                   Vol.16, No.8, p859-862 August 1989
!           2. DAVID P.RUBINCAM and B.FONG CHAO : THE GRAVITATIONAL
!              FIELD OF DEIMOS    ICARUS, 114 p63-67
!
!********1*********2*********3*********4*********5*********6*********7**
       BLOCK DATA GRVPDB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/PDOBGR/PDC(22,2),PDS(22,2)
!
!  LOAD THE COEF.  C,S OF PHOBOS(DEIMOS=2.) GRAVITY FIELD 4X4
!  (UNIT=D-2)
!
!    Phobos                  C20   C21   C22
      DATA PDC/0.0,0.0,0.0,0.0,-4.698,0.136,2.276,0.0,0.0,              &
     &        0.293,-0.309,-0.847, 0.224,0.0,0.0,                       &
     &          0.762,0.347,-0.288,-0.280,-0.120,0.0,0.0,               &
     &         0.0,0.0,0.0,0.0,-4.827,-0.0727,4.773,0.0,0.0,            &
     &         0.970,+1.430,-1.108, 0.121,0.0,0.0,                      &
     &          0.508,-0.663,-0.520,+1.137,+1.200,0.0,0.0/
!    Phobos    C30   C31    C32    C33
!    Phobos      C40   C41   C42    C43    C44
!    Deimos                      C20    C21    C22
!    Deimos      C30   C31     C32    C33
!    Deimos      C40    C41    C42    C43    C44
!
!    Phobos                      S20   S21    S22
      DATA PDS/ 0.0,0.0,0.0,0.0, 0.0, 0.138,-0.0202, 0.0,0.0,           &
     &           0.0, 0.181,-0.0655, -1.392,0.0,0.0,                    &
     &           0.0,-0.0776,-0.112,0.337,-0.0622,0.0,0.0,              &
     &           0.0,0.0,0.0,0.0,0.0, 0.362, 0.122,0.0,0.0,             &
     &           0.0,  0.0521, -0.499, 0.728,0.0,0.0,                   &
     &          0.0257,0.689,-0.614,-0.274,  0.0,0.0,0.0/
!    Phobos      S30   S31    S32     S33
!    Phobos      S40    S41    S42    S43    S44
!    Deimos                      S20    S21    S22
!    Deimos       S30   S31     S32    S33
!    Deimos      S40    S41    S42    S43    S44
      END
