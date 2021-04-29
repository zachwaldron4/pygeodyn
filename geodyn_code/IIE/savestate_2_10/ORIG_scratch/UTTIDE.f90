!$UTTIDE
      SUBROUTINE UTTIDE(MJDS1,FSEC1,MJDS2,FSEC2,NM,DEL1,DEL2)
!********1*********2*********3*********4*********5*********6*********7**
! UTTIDE           84/03/21            0000.0    PGMR -
!
!
! FUNCTION:  CALCULATE THE ZONAL TIDE EFFECT ON UT TIME FOR
!            TIDAL CONSTITUENTS WHICH HAVE A PERIOD OF 35 DAYS
!            OR LESS.IT IS BELIEVED THAT THE EFFECT OF THESE 41
!            CONSTITUENTS IS NOT PRESENT IN THE BIH TABLES AND THAT
!            ANY FIVE DAY TABLE WOULD HAVE TROUBLE REPRESENTING
!            THESE SHORT PERIOD EFFECTS.REFERENCE IS THE BIH CIRCULAR
!            D NOTICE TO USERS,MARCH 5,1982.
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDS1    I         INTEGER SECONDS SINCE GEODYN REF.TIME AT FIRST TI
!                      FOR WHICH CORRECTION IS DESIRED.
!   FSEC1    I         FRACTIONAL SECONDS REMAINING FROM MJDS1.
!   MJDS2    I         INTEGER SECONDS SINCE GEODYN REF.TIME AT LAST TIM
!                      FOR WHICH CORRECTION IS DESIRED.
!   FSEC2    I         FRACTIONAL SECONDS REMAINING FROM MJDS2.
!   NM       I         1,IF FIRST AND LAST TIME ARE IDENTICAL
!                      2,OTHERWISE.
!   DEL1     O         SECONDS TO BE ADDED TO BIH VALUE OF UT1 AT FIRST
!                      TIME
!   DEL2     O         SECONDS TO BE ADDED TO BIH VALUE OF UT1 AT SECOND
!                      TIME
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION XL(41),XLP(41),XF(41),XD(41),XW(41),XARG(41),SARG(41)
      COMMON/DELUT1/UTTDAM(41),XDELUT
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      DATA ZERO/0.D0/
!     DATA MJDS0/1861444800/

! Old values are in radians
!      DATA ARGL0/2.3555D0/,ARGLP0/6.2401D0/,ARGF0/1.6279D0/,
!     .     ARGD0/5.1985D0/,ARGW0/2.1824D0/
! Old values are in radians per second since J2000
!      DATA ARGLS/.2639203063D-5/,ARGLPS/.1990968184D-6/,
!     .     ARGFS/.2672404113D-5/,ARGDS/.2462600791D-5/,
!     .     ARGWS/.1069700986D-7/


! Following values are in degrees
!      DATA ARGL0/134.9629D0/,ARGLP0/357.5254D0/,ARGF0/93.2728D0/,
!     .     ARGD0/297.8503D0/,ARGW0/125.0433D0/
! Following values are in radians
      DATA ARGL0/2.355547D0/,ARGLP0/6.2399954D0/,ARGF0/1.6279175D0/,    &
     &     ARGD0/5.1984684D0/,ARGW0/2.1824173D0/
! Following values are in degrees per day since J2000
!      DATA ARGLS/13.06499295D0/,ARGLPS/0.985600258D0/,
!     .     ARGFS/13.22935027D0/,ARGDS/12.19074911D0/,
!     .     ARGWS/0.05295381D0/
! Following values are in degrees per second since J2000
!      DATA ARGLS/1.512151961806D-4/,ARGLPS/1.140741039352D-5/,
!     .     ARGFS/1.531174799769D-4/,ARGDS/1.410966332176D-4/,
!     .     ARGWS/6.128913194444D-7/
! Following values are in radians per second since J2000
      DATA ARGLS/.26392030524D-5/,ARGLPS/.1990968704931D-6/,            &
     &     ARGFS/.2672404167953D-5/,ARGDS/.2462600813126D-5/,           &
     &     ARGWS/.106969714812D-7/

!  COEF IS EQUIVALENT TO UTTDAM (UT TIDE AMPLITUDE)
!
! Old values had error on 9.61 day period (# 12)
!     DATA COEF/-0.02D-4,-0.04D-4,-0.10D-4,-0.05D-4,-0.12D-4,
!    .          -0.04D-4,-0.41D-4,-0.99D-4,-0.02D-4,-0.08D-4,
!    .          -0.20D-4,-0.03D-4,+0.02D-4,+0.03D-4,-0.30D-4,
!    .          -3.21D-4,-7.76D-4,+0.02D-4,-0.34D-4,+0.02D-4,
!    .          -0.02D-4,-0.05D-4,-0.73D-4,-0.05D-4,-0.05D-4,
!    .          +0.05D-4,+0.10D-4,+0.04D-4,+0.05D-4,+0.18D-4,
!    .          +0.44D-4,+0.53D-4,-8.26D-4,+0.54D-4,+0.05D-4,
!    .          -0.06D-4,+0.12D-4,-1.82D-4,+0.13D-4,+0.02D-4,
!    .          -0.09D-4/
!
! The following are from IERS Technical Note 21,13 and 3
!   Has correct 9.61 day period amplitude value
!              DATA UTTDAM/-0.02D-4,-0.04D-4,-0.10D-4,-0.05D-4,-0.12D-4,
!     1                    -0.04D-4,-0.41D-4,-0.99D-4,-0.02D-4,-0.08D-4,
!     2                    -0.20D-4,-0.08D-4,+0.02D-4,+0.03D-4,-0.30D-4,
!     3                    -3.21D-4,-7.76D-4,+0.02D-4,-0.34D-4,+0.02D-4,
!     4                    -0.02D-4,+0.05D-4,-0.73D-4,-0.05D-4,-0.05D-4,
!     5                    +0.05D-4,+0.10D-4,+0.04D-4,+0.05D-4,+0.18D-4,
!     6                    +0.44D-4,+0.53D-4,-8.26D-4,+0.54D-4,+0.05D-4,
!     7                    -0.06D-4,+0.12D-4,-1.82D-4,+0.13D-4,+0.02D-4,
!     8                    -0.09D-4/
!
! The following are from Yoder, eta al., 1981
!   These amplitudes have more digits and match tables
!   make sure multiply by 0.94(k/c) and 1.0D-7(units) to get same as abo
!
!      data uttdam /-0.2500d+02,-0.4300d+02,-0.1050d+03,-0.5400d+02,
!     .             -0.1310d+03,
!     .          -0.41000d+02,-0.43700d+03,-0.10560d+04,-0.19000d+02,
!     .          -0.87000d+02,-0.21000d+03,-0.81000d+02, 0.23000d+02,
!     .           0.27000d+02,-0.31800d+03,-0.34130d+04,-0.82520d+04,
!     .           0.23000d+02,-0.36000d+03, 0.19000d+02,-0.26000d+02,
!     .          +0.50000d+02,-0.78100d+03,-0.56000d+02,-0.54000d+02,
!     .           0.53000d+02, 0.10700d+03, 0.42000d+02, 0.50000d+02,
!     .           0.18800d+03, 0.46300d+03, 0.56800d+03,-0.87880d+04,
!     .           0.57900d+03, 0.50000d+02,-0.59000d+02, 0.12500d+03,
!     .          -0.19400d+04, 0.14000d+03, 0.19000d+02,-0.91000d+02/

      DATA AK/0.940D0/

      DATA XL/1.D0,2.D0,2.D0,0.D0,0.D0,1.D0,1.D0,1.D0,3.D0,-1.D0,       &
     &       -1.D0,1.D0,2.D0,0.D0,0.D0,0.D0,0.D0,2.D0,2.D0,2.D0,        &
     &        0.D0,0.D0,0.D0,0.D0,0.D0,1.D0,1.D0,1.D0,-1.D0,-1.D0,      &
     &       -1.D0,1.D0,1.D0,1.D0,0.D0,1.D0,-1.D0,-1.D0,-1.D0,1.D0,     &
     &       -1.D0/
      DATA XLP/0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,       &
     &         0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,       &
     &        -1.D0,0.D0,0.D0,0.D0,-1.D0,0.D0,0.D0,1.D0,0.D0,0.D0,      &
     &         0.D0,0.D0,0.D0,0.D0,0.D0,-1.D0,0.D0,0.D0,0.D0,0.D0,      &
     &        -1.D0/
      DATA XF/2.D0,2.D0,2.D0,2.D0,2.D0,2.D0,2.D0,2.D0,0.D0,2.D0,        &
     &        2.D0,0.D0,2.D0,2.D0,2.D0,2.D0,2.D0,0.D0,0.D0,0.D0,        &
     &        2.D0,0.D0,0.D0,0.D0,0.D0,2.D0,2.D0,0.D0,2.D0,2.D0,        &
     &        2.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,-2.D0,       &
     &        0.D0/
      DATA XD/2.D0,0.D0,0.D0,2.D0,2.D0,0.D0,0.D0,0.D0,0.D0,2.D0,        &
     &        2.D0,2.D0,-2.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,       &
     &        0.D0,2.D0,2.D0,2.D0,2.D0,-2.D0,-2.D0,0.D0,0.D0,0.D0,      &
     &        0.D0,0.D0,0.D0,0.D0,1.D0,0.D0,2.D0,2.D0,2.D0,2.D0,        &
     &        2.D0/
      DATA XW/2.D0,1.D0,2.D0,1.D0,2.D0,0.D0,1.D0,2.D0,0.D0,1.D0,        &
     &        2.D0,0.D0,2.D0,2.D0,0.D0,1.D0,2.D0,-1.D0,0.D0,1.D0,       &
     &        2.D0,-1.D0,0.D0,1.D0,0.D0,1.D0,2.D0,0.D0,0.D0,1.D0,       &
     &        2.D0,-1.D0,0.D0,1.D0,0.D0,0.D0,-1.D0,0.D0,1.D0,-1.D0,     &
     &        0.D0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      D2000=51544.5D0-TMGDN2
      MJDS0=INT(D2000*86400.D0)
!
      MJDS=MJDS1
      FSEC=FSEC1
      DO 1000 II=1,NM
      XTIME=FSEC+(MJDS-MJDS0)
      ARGL=ARGL0+XTIME*ARGLS
      ARGLP=ARGLP0+XTIME*ARGLPS
      ARGF=ARGF0+XTIME*ARGFS
      ARGD=ARGD0+XTIME*ARGDS
      ARGW=ARGW0-XTIME*ARGWS
      DO 100 I=1,41
      XARG(I)=ARGL*XL(I)+ARGLP*XLP(I)+ARGF*XF(I)+ARGD*XD(I)             &
     &       +ARGW*XW(I)
  100 END DO
      DO 200 I=1,41
      SARG(I)=SIN(XARG(I))
  200 END DO
      DEL2=ZERO
      DO 300 I=1,41
      DEL2=DEL2+AK*1.0D-07*UTTDAM(I)*SARG(I)
  300 END DO
      IF(II.EQ.1) DEL1=DEL2
      MJDS=MJDS2
      FSEC=FSEC2
 1000 END DO
      RETURN
      END
