
!$LDATTP
      SUBROUTINE LDATTP(ARRAY,ROLL,PITCH,YAW,X,IFIRST,TDIFF,SINWT,COSWT,&
     &ISECND,LFORCE,SBFTOD)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!********1*********2*********3*********4*********5*********6*********7**
! LDATTP
!
! FUNCTION         LOAD ATTITUDE PARTIALS FOR A DYNAMIC ACCELERATION
!                  CASE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ARRAY   I   A     PARTIALS
!   ROLL     I   S     ROLL ANGLE IN RADIANS
!   PITCH    I   S     PITCH ANGLE IN RADIANS
!   YAW      I   S     YAW ANGLE IN RADIANS
!   X        I   A     ACCELERATIONS IN THE TOD REFERENCE FRAME/SBF GEOM
!   NEQN     I   S     NUMBER OF FORCE MODEL PARAMETERS
!   TDIFF    I   S     TIME DIFFERENCE BETWEEN CUURENT OBS AND ATTITUDE
!                      REFERENCE TIME
!**********************************************************************
!
!
       PARAMETER (  ZERO = 0.D0 )
       PARAMETER (  ONE  = 1.D0 )
       DIMENSION ARRAY(IFIRST,ISECND),X(3)
       DIMENSION R(3,3),P(3,3),Y(3,3)
       DIMENSION RR(3,3),PP(3,3),YY(3,3)
       DIMENSION PY(3,3),RY(3,3),RP(3,3)
       DIMENSION AA(3,3),BB(3,3),CC(3,3)
       DIMENSION SBFTOD(3,3)
       DIMENSION SBFTOR(3,3)
!
      COMMON/ATTCB /KPAT,MAXAT,MAXLAS,IBLAS,                            &
     &              NASAT,IATCNT,NXATT
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CRMI/RMI(9)
      COMMON/IATTCB/INDXAT
      COMMON/SETAPT/                                                    &
     &       JSADRG(1),JSASRD(1),JSAGA(1),JFAADJ,JTHDRG,JACBIA,JATITD,  &
     &       JDSTAT,JDTUM ,                                             &
     &       JSACS, JSATID,JSALG,JSAGM,JSAKF,JSAXYP,JSADJ2,JSAPGM,      &
     &       JFGADJ,JLTPMU,JLTPAL,JL2CCO,JL2SCO,JL2GM,JLRELT,           &
     &       JLJ2SN,JLGMSN,JLPLFM,JL2AE,JSAXTO,JSABRN
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************

!     write(6,*)' dbg ROLL PITCH YAW FROM LDATTP ',ROLL,PITCH,YAW
      SINR=SIN(ROLL)
      COSR=COS(ROLL)
      SINP=SIN(PITCH)
      COSP=COS(PITCH)
      SINY=SIN(YAW)
      COSY=COS(YAW)

      SCALE=DEGRAD

! The partials below is derived from the formulas YPR in GETRPY.
! The partials below are the partials of (YPR) Transposed
! wrt ROLL, PITCH, YAW

      AA(1,1)=ZERO
      AA(1,2)=ZERO
      AA(1,3)=ZERO
      AA(2,1)=SCALE*(COSR*SINP*COSY-SINR*SINY)
      AA(2,2)=SCALE*(-COSR*SINP*SINY-SINR*COSY)
      AA(2,3)=SCALE*(-COSR*COSP)
      AA(3,1)=SCALE*(SINP*SINR*COSY+COSR*SINY)
      AA(3,2)=SCALE*(-SINP*SINR*SINY+COSY*COSR)
      AA(3,3)=SCALE*(-SINR*COSP)

      BB(1,1)=SCALE*(-SINP*COSY)
      BB(1,2)=SCALE*(SINP*SINY)
      BB(1,3)=SCALE*COSP
      BB(2,1)=SCALE*(SINR*COSP*COSY)
      BB(2,2)=SCALE*(-SINR*COSP*SINY)
      BB(2,3)=SCALE*SINR*SINP
      BB(3,1)=SCALE*(-COSP*COSR*COSY)
      BB(3,2)=SCALE*COSP*COSR*SINY
      BB(3,3)=SCALE*(-COSR*SINP)

      CC(1,1)=SCALE*(-COSP*SINY)
      CC(1,2)=SCALE*(-COSP*COSY)
      CC(1,3)=ZERO
      CC(2,1)=SCALE*(SINR*SINP*SINY+COSR*COSY)
      CC(2,2)=SCALE*(-SINR*SINP*COSY-COSR*SINY)
      CC(2,3)=ZERO
      CC(3,1)=SCALE*(SINP*COSR*SINY+SINR*COSY)
      CC(3,2)=SCALE*(SINP*COSR*COSY-SINY*SINR)
      CC(3,3)=ZERO
!
!     write(6,*)' dbg AA '
!     write(6,*)AA(1,1),AA(1,2),AA(1,3)
!     write(6,*)AA(2,1),AA(2,2),AA(2,3)
!     write(6,*)AA(3,1),AA(3,2),AA(3,3)
!     write(6,*)' dbg BB '
!     write(6,*)BB(1,1),BB(1,2),BB(1,3)
!     write(6,*)BB(2,1),BB(2,2),BB(2,3)
!     write(6,*)BB(3,1),BB(3,2),BB(3,3)
!     write(6,*)' dbg CC '
!     write(6,*)CC(1,1),CC(1,2),CC(1,3)
!     write(6,*)CC(2,1),CC(2,2),CC(2,3)
!     write(6,*)CC(3,1),CC(3,2),CC(3,3)

              IF(LFORCE) THEN

      T1 = AA(1,1)*X(1) + AA(2,1)*X(2) + AA(3,1)*X(3)
      T2 = AA(1,2)*X(1) + AA(2,2)*X(2) + AA(3,2)*X(3)
      T3 = AA(1,3)*X(1) + AA(2,3)*X(2) + AA(3,3)*X(3)
!
      SBFTOR(1,1)=RMI(1)*SBFTOD(1,1)+RMI(2)*SBFTOD(2,1)                 &
     &           +RMI(3)*SBFTOD(3,1)
      SBFTOR(2,1)=RMI(4)*SBFTOD(1,1)+RMI(5)*SBFTOD(2,1)                 &
     &           +RMI(6)*SBFTOD(3,1)
      SBFTOR(3,1)=RMI(7)*SBFTOD(1,1)+RMI(8)*SBFTOD(2,1)                 &
     &           +RMI(9)*SBFTOD(3,1)
!
      SBFTOR(1,2)=RMI(1)*SBFTOD(1,2)+RMI(2)*SBFTOD(2,2)                 &
     &           +RMI(3)*SBFTOD(3,2)
      SBFTOR(2,2)=RMI(4)*SBFTOD(1,2)+RMI(5)*SBFTOD(2,2)                 &
     &           +RMI(6)*SBFTOD(3,2)
      SBFTOR(3,2)=RMI(7)*SBFTOD(1,2)+RMI(8)*SBFTOD(2,2)                 &
     &           +RMI(9)*SBFTOD(3,2)
!
      SBFTOR(1,3)=RMI(1)*SBFTOD(1,3)+RMI(2)*SBFTOD(2,3)                 &
     &           +RMI(3)*SBFTOD(3,3)
      SBFTOR(2,3)=RMI(4)*SBFTOD(1,3)+RMI(5)*SBFTOD(2,3)                 &
     &           +RMI(6)*SBFTOD(3,3)
      SBFTOR(3,3)=RMI(7)*SBFTOD(1,3)+RMI(8)*SBFTOD(2,3)                 &
     &           +RMI(9)*SBFTOD(3,3)
!
      Xroll = T1*SBFTOR(1,1) + T2*SBFTOR(1,2) + T3*SBFTOR(1,3)
      Yroll = T1*SBFTOR(2,1) + T2*SBFTOR(2,2) + T3*SBFTOR(2,3)
      Zroll = T1*SBFTOR(3,1) + T2*SBFTOR(3,2) + T3*SBFTOR(3,3)

      T1 = BB(1,1)*X(1) + BB(2,1)*X(2) + BB(3,1)*X(3)
      T2 = BB(1,2)*X(1) + BB(2,2)*X(2) + BB(3,2)*X(3)
      T3 = BB(1,3)*X(1) + BB(2,3)*X(2) + BB(3,3)*X(3)

      Xpitch = T1*SBFTOR(1,1) + T2*SBFTOR(1,2) + T3*SBFTOR(1,3)
      Ypitch = T1*SBFTOR(2,1) + T2*SBFTOR(2,2) + T3*SBFTOR(2,3)
      Zpitch = T1*SBFTOR(3,1) + T2*SBFTOR(3,2) + T3*SBFTOR(3,3)

      T1 = CC(1,1)*X(1) + CC(2,1)*X(2) + CC(3,1)*X(3)
      T2 = CC(1,2)*X(1) + CC(2,2)*X(2) + CC(3,2)*X(3)
      T3 = CC(1,3)*X(1) + CC(2,3)*X(2) + CC(3,3)*X(3)

      Xyaw = T1*SBFTOR(1,1) + T2*SBFTOR(1,2) + T3*SBFTOR(1,3)
      Yyaw = T1*SBFTOR(2,1) + T2*SBFTOR(2,2) + T3*SBFTOR(2,3)
      Zyaw = T1*SBFTOR(3,1) + T2*SBFTOR(3,2) + T3*SBFTOR(3,3)


                    ! IF(LFORCE)
              ELSE

!     write(6,*)' dbg X BF ',X(1),X(2),X(3)
! convert X from TOD to SBF multiply by SBFTOT TRANSPOSE

      Xroll = AA(1,1)*X(1) + AA(1,2)*X(2) + AA(1,3)*X(3)
      Yroll = AA(2,1)*X(1) + AA(2,2)*X(2) + AA(2,3)*X(3)
      Zroll = AA(3,1)*X(1) + AA(3,2)*X(2) + AA(3,3)*X(3)
!      write(6,*) 'dbg Xroll Yroll Zroll ',Xroll,Yroll,Zroll

      Xpitch = BB(1,1)*X(1) + BB(1,2)*X(2) + BB(1,3)*X(3)
      Ypitch = BB(2,1)*X(1) + BB(2,2)*X(2) + BB(2,3)*X(3)
      Zpitch = BB(3,1)*X(1) + BB(3,2)*X(2) + BB(3,3)*X(3)
!      write(6,*) 'dbg Xp Yp Zp ',Xpitch,Ypitch,Zpitch

      Xyaw = CC(1,1)*X(1) + CC(1,2)*X(2) + CC(1,3)*X(3)
      Yyaw = CC(2,1)*X(1) + CC(2,2)*X(2) + CC(2,3)*X(3)
      Zyaw = CC(3,1)*X(1) + CC(3,2)*X(2) + CC(3,3)*X(3)
!      write(6,*) 'dbg Xp Yp Zp ',Xyaw,Yyaw,Zyaw


                     ! IF(LFORCE)
              ENDIF



      IF(LFORCE) THEN

! ROLL PRMS                       PITCH              YAW
! THETA X/THETA C0 =   THETA X/THETA C0             THETA X/ THETA C0=E
! THETA X/THETA LI =
! THETA X/THETA QU =
! THETA X/THETA P1 =
! THETA X/THETA P2 =

       INDEX=JATITD+(INDXAT-1)*15

         ARRAY(INDEX,1) = Xroll
         ARRAY(INDEX+1,1) = Xroll * TDIFF
         ARRAY(INDEX+2,1) = Xroll * TDIFF **2
         ARRAY(INDEX+3,1) = Xroll * SINWT
         ARRAY(INDEX+4,1) = Xroll
         ARRAY(INDEX+5,1) = Xpitch
         ARRAY(INDEX+6,1) = Xpitch * TDIFF
         ARRAY(INDEX+7,1) = Xpitch * TDIFF **2
         ARRAY(INDEX+8,1) = Xpitch * SINWT
         ARRAY(INDEX+9,1) = Xpitch * COSWT
         ARRAY(INDEX+10,1) = Xyaw
         ARRAY(INDEX+11,1) = Xyaw * TDIFF
         ARRAY(INDEX+12,1) = Xyaw * TDIFF **2
         ARRAY(INDEX+13,1) = Xyaw * SINWT
         ARRAY(INDEX+14,1) = Xyaw * COSWT


! THETA Y/THETA C0 =          THETA Y /THETA C0 =  THETA Y/THETA CO
! THETA Y/THETA LI =
! THETA Y/THETA QU =
! THETA Y/THETA P1 =
! THETA Y/THETA P2 =

         ARRAY(INDEX,2) = Yroll
         ARRAY(INDEX+1,2) = Yroll * TDIFF
         ARRAY(INDEX+2,2) = Yroll * TDIFF **2
         ARRAY(INDEX+3,2) = Yroll * SINWT
         ARRAY(INDEX+4,2) = Yroll * COSWT
         ARRAY(INDEX+5,2) = Ypitch
         ARRAY(INDEX+6,2) = Ypitch * TDIFF
         ARRAY(INDEX+7,2) = Ypitch * TDIFF **2
         ARRAY(INDEX+8,2) = Ypitch * SINWT
         ARRAY(INDEX+9,2) = Ypitch * COSWT
         ARRAY(INDEX+10,2) = Yyaw
         ARRAY(INDEX+11,2) = Yyaw * TDIFF
         ARRAY(INDEX+12,2) = Yyaw * TDIFF **2
         ARRAY(INDEX+13,2) = Yyaw * SINWT
         ARRAY(INDEX+14,2) = Yyaw * COSWT

! THETA Z/THETA C0 =             THETA Z/THETA C0 =        THETA Z/THETA
! THETA Z/THETA LI =
! THETA Z/THETA QU =
! THETA Z/THETA P1 =
! THETA Z/THETA P2 =
!

         ARRAY(INDEX,3) = Zroll
         ARRAY(INDEX+1,3) = Zroll * TDIFF
         ARRAY(INDEX+2,3) = Zroll * TDIFF **2
         ARRAY(INDEX+3,3) = Zroll * SINWT
         ARRAY(INDEX+4,3) = Zroll * COSWT
         ARRAY(INDEX+5,3) = Zpitch
         ARRAY(INDEX+6,3) = Zpitch * TDIFF
         ARRAY(INDEX+7,3) = Zpitch * TDIFF **2
         ARRAY(INDEX+8,3) = Zpitch * SINWT
         ARRAY(INDEX+9,3) = Zpitch * COSWT
         ARRAY(INDEX+10,3) = Zyaw
         ARRAY(INDEX+11,3) = Zyaw * TDIFF
         ARRAY(INDEX+12,3) = Zyaw * TDIFF **2
         ARRAY(INDEX+13,3) = Zyaw
         ARRAY(INDEX+14,3) = Zyaw

                     ! IF(LFORCE)
          ELSE

      DO 20 IAT=2,16
      DO 20 J=1,IFIRST
         ARRAY(J,IAT) = ZERO
   20    CONTINUE


! ROLL PRMS                       PITCH              YAW
! THETA X/THETA C0 =   THETA X/THETA C0 =           THETA X/ THETA C0=
! THETA X/THETA LI =
! THETA X/THETA QU =
! THETA X/THETA P1 =
! THETA X/THETA P2 =

       INDEX=2

         ARRAY(1,INDEX)   = Xroll
         ARRAY(1,INDEX+1) = Xroll * TDIFF
         ARRAY(1,INDEX+2) = Xroll * TDIFF **2
         ARRAY(1,INDEX+3) = Xroll * SINWT
         ARRAY(1,INDEX+4) = Xroll * COSWT
         ARRAY(1,INDEX+5) = Xpitch
         ARRAY(1,INDEX+6) = Xpitch * TDIFF
         ARRAY(1,INDEX+7) = Xpitch * TDIFF **2
         ARRAY(1,INDEX+8) = Xpitch * SINWT
         ARRAY(1,INDEX+9) = Xpitch * COSWT
         ARRAY(1,INDEX+10) = Xyaw
         ARRAY(1,INDEX+11) = Xyaw * TDIFF
         ARRAY(1,INDEX+12) = Xyaw * TDIFF **2
         ARRAY(1,INDEX+13) = Xyaw * SINWT
         ARRAY(1,INDEX+14) = Xyaw * COSWT

!       do jj=1,15
!       write(6,*)' dbg part ld ',array(1,index-1+jj),1,index-1+jj
!       enddo


! THETA Y/THETA C0 =          THETA Y /THETA C0 =  THETA Y/THETA CO =
! THETA Y/THETA LI =
! THETA Y/THETA QU =
! THETA Y/THETA P1 =
! THETA Y/THETA P2 =

         ARRAY(2,INDEX) =   Yroll
         ARRAY(2,INDEX+1) = Yroll * TDIFF
         ARRAY(2,INDEX+2) = Yroll * TDIFF **2
         ARRAY(2,INDEX+3) = Yroll * SINWT
         ARRAY(2,INDEX+4) = Yroll * COSWT
         ARRAY(2,INDEX+5) = Ypitch
         ARRAY(2,INDEX+6) = Ypitch * TDIFF
         ARRAY(2,INDEX+7) = Ypitch * TDIFF **2
         ARRAY(2,INDEX+8) = Ypitch * SINWT
         ARRAY(2,INDEX+9) = Ypitch * COSWT
         ARRAY(2,INDEX+10) = Yyaw
         ARRAY(2,INDEX+11) = Yyaw * TDIFF
         ARRAY(2,INDEX+12) = Yyaw * TDIFF **2
         ARRAY(2,INDEX+13) = Yyaw * SINWT
         ARRAY(2,INDEX+14) = Yyaw * COSWT

!       do jj=1,15
!       write(6,*)' dbg part ld ',array(2,index-1+jj),2,index-1+jj
!       enddo

! THETA Z/THETA C0 =             THETA Z/THETA C0 =        THETA Z/THETA
! THETA Z/THETA LI =
! THETA Z/THETA QU =
! THETA Z/THETA P1 =
! THETA Z/THETA P2 =
!

         ARRAY(3,INDEX) = Zroll
         ARRAY(3,INDEX+1) = Zroll * TDIFF
         ARRAY(3,INDEX+2) = Zroll * TDIFF **2
         ARRAY(3,INDEX+3) = Zroll * SINWT
         ARRAY(3,INDEX+4) = Zroll * COSWT
         ARRAY(3,INDEX+5) = Zpitch
         ARRAY(3,INDEX+6) = Zpitch * TDIFF
         ARRAY(3,INDEX+7) = Zpitch * TDIFF **2
         ARRAY(3,INDEX+8) = Zpitch * SINWT
         ARRAY(3,INDEX+9) = Zpitch * COSWT
         ARRAY(3,INDEX+10) = Zyaw
         ARRAY(3,INDEX+11) = Zyaw * TDIFF
         ARRAY(3,INDEX+12) = Zyaw * TDIFF **2
         ARRAY(3,INDEX+13) = Zyaw * SINWT
         ARRAY(3,INDEX+14) = Zyaw * COSWT

!       do jj=1,15
!       write(6,*)' dbg part ld ',array(3,index-1+jj),3,index-1+jj
!       enddo

                  ! IF(LFORCE)
         ENDIF

      RETURN
      END
