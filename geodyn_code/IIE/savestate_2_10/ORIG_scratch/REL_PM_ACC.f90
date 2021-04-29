!$REL_PM_ACC
      SUBROUTINE REL_PM_ACC(XP,REL_ACC,IPPN_BODIES,IBODY,DBETA,DGAMMA,  &
     &DGM)
!
!********1*********2*********3*********4*********5*********6*********7**
!REL_PM_ACC       14/11/01            1410.0    PGMR - J WIMERT
!
! FUNCTION:  CALCULATE RELATIVISTIC POINT MASS ACCELERATION
!            FOR WHEN THE INTGCB OPTION IS SELECTED
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XP       I    A    ORBITING BODY POSITION
!   REL_ACC  O    A    CALCULATED ACCELERATION
!   IPPN_BODIES I S    NUMBER OF BODIES
!   IBODY    I    S    IDENTIFIER FOR WHAT BODY THE ACCELERATION
!                      IS BEING CALCULATED FOR
!                        IF 1, CALCULATION IS FOR ACCELERATION OF
!                              SPACECRAFT ORBITING ASTEROID
!                              (EXCLUDES ASTEROID NEWTONIAN POINT
!                              MASS CONTRIBUTION)
!                        IF 2, CALCULATION IS FOR ASTEROID BEING
!                              ORBITED (EXCLUDES ASTEROID)
!                        IF 3, CALCULATION IS FOR ACCELERATION OF
!                              ASTEROID ORBITING SUN
!                              (EXCLUDES ASTEROID AND NEWTONIAN
!                              POINT MASS CONTRIBUTION)
!                        IF 4, CALCULATION IS FOR ACCELERATION OF
!                              SUN BEING ORBITED BY ASTEROID
!                              (EXCLUDES SUN)
!                        IF 5, CALCULATION IS FOR ACCELERATION OF
!                              SPACECRAFT ORBITING ASTEROID
!                              (EXCLUDES ASTEROID NEWTONIAN POINT
!                              MASS CONTRIBUTION) EXCLUDING
!                              THE CENTRAL BODY (12). THIS IS FOR
!                              NO-INTGCB RUNS USING F.f90.
!                        IF 6, CALCULATION IS FOR ACCELERATION OF
!                              ASTEROID BEING ORBITED
!                              EXCLUDING BODY (12) AND THE CENTRAL BODY
!                              NO-INTGCB RUNS USING F.f90.
!
!
! COMMENTS:  RELATIVISTIC ACCELERATION IS COMPUTED USING ISOTROPIC,
!            PARAMETERIZED POST-NEWTONIAN (PPN) N-BODY METRIC
!
!            INPUTS AND OUTPUTS IN TRUE OF REF
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
!
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/CBDACC/BD_ACCEL(10,999)
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/IEPHM2/ISUPL,ICBODY,ISEQB(2),MAXDEG,ITOTSE,IREPL(988), &
     &              NXEPH2
      COMMON/CBDTRU/BDTRUE(7,999)
      COMMON/CRMI/RMI(9)
!
      DIMENSION XP(3,2),XPSSB(3,2), IBV(11)
      DIMENSION Ra_ik(3),Rc_b(3),Rc_a(3)
      DIMENSION SUM1F(3,IPPN_BODIES),SUM2(3,IPPN_BODIES)
      DIMENSION SUM3(3,IPPN_BODIES),SUM1GM(3,IPPN_BODIES)
      DIMENSION SUM1DGM(3,IPPN_BODIES), TERM1DGM(3,IPPN_BODIES)
      DIMENSION TERM2DGM(3,IPPN_BODIES), TERM3DGM(3,IPPN_BODIES)
      DIMENSION SUM1G(3,IPPN_BODIES),SUM1AE(3,IPPN_BODIES)
      DIMENSION REL_PM_ACCEL(3,IPPN_BODIES),SUM1(3,IPPN_BODIES)
      DIMENSION REL_PM_ACCEL_SUM(3)
      DIMENSION REL_ACC(3)
      DIMENSION DBETA(3),DGAMMA(3),DGM(3)
!
       do j=1,355
       do i=1,10
!      write(6,*)' dbg REL', BD_ACCEL(I,J),I,J
       enddo
       enddo

      SUNGRV_TESTX=0.D0
      SUNGRV_TESTY=0.D0
      SUNGRV_TESTZ=0.D0

      SUM1F(:,:)=0.0D0
      SUM2(:,:)=0.0D0
      SUM3(:,:)=0.0D0
      SUM1GM(:,:)=0.0D0
      SUM1DGM(:,:)=0.0D0
      TERM1DGM(:,:)=0.0D0
      TERM2DGM(:,:)=0.0D0
      TERM3DGM(:,:)=0.0D0
      SUM1G(:,:)=0.0D0
      SUM1AE(:,:)=0.0D0
      REL_PM_ACCEL(:,:)=0.0D0
      SUM1(:,:)=0.0D0
      REL_PM_ACCEL_SUM(:)=0.0D0
      REL_ACC(:)=0.0D0
      DBETA(:)=0.D0
      DGAMMA(:)=0.D0
      DGM(:)=0.D0
!
!!! calculate ppn point mass perturbation model
!
! number of bodies to worry about
! 11- planetary bodies
! 12- intgcb body
! 13+ ICBODY bodies from external ephem
!       IPPN_BODIES=12+ICBODY
!
! constants
       IPPN_BETA = 1.0D0
       IPPN_GAMMA= 1.0D0
       VLIGHT2 = VLIGHT*VLIGHT
! IBV vector to allocate the correct position of bodies
       IBV(1)=10
       IBV(2)=1
       IBV(3)=9
       IBV(4)=11
       IBV(5)=2
       IBV(6)=3
       IBV(7)=4
       IBV(8)=5
       IBV(9)=6
       IBV(10)=7
       IBV(11)=8

!
! speed of sc
       VASQ = XP(1,2)*XP(1,2)+XP(2,2)*XP(2,2)+XP(3,2)*XP(3,2)
       VA   = SQRT(VASQ)
!
!GM      = BD_ACCEL(10,IK)
!R(3)body= BD_ACCEL(1:3,IK)
!V(3)body= BD_ACCEL(4:6,IK)
!a(3)body= BD_ACCEL(7:9,IK)
!R(3)A = XPS(1:3,1)
!V(3)A = XPS(1:3,2)
!! A - spacecraft
!!
!
!!!
!!!   PPN EQUATION IS BROKEN UP INTO 3 SUMS
!!!
!
!!! SUM1

       DO IK=1,IPPN_BODIES

        IF (IBODY.EQ.2.AND.IK.EQ.12) GOTO 1234
        IF (IBODY.EQ.6.AND.IK.EQ.IBV(ICBDGM)) GOTO 1234
        IF (IBODY.EQ.6.AND.IK.EQ.12) GOTO 1234
        IF (IBODY.EQ.5.AND.IK.EQ.12) GOTO 1234
        IF (IBODY.EQ.4.AND.IK.EQ.8)  GOTO 1234
        IF (IBODY.EQ.3.AND.IK.EQ.12)  GOTO 1234
!
!! IF (A.NE.B).. B=IK

        !SUM1GM
        Ra_ik(1)=BD_ACCEL(1,IK)-XP(1,1)
        Ra_ik(2)=BD_ACCEL(2,IK)-XP(2,1)
        Ra_ik(3)=BD_ACCEL(3,IK)-XP(3,1)
        RAIK2 = Ra_ik(1)*Ra_ik(1)+Ra_ik(2)*Ra_ik(2)+Ra_ik(3)*Ra_ik(3)
        RAIK  = SQRT(RAIK2)
        RAIK3 = RAIK2*RAIK
!
        VBSQ = BD_ACCEL(4,IK)*BD_ACCEL(4,IK)+BD_ACCEL(5,IK)*            &
     &                 BD_ACCEL(5,IK)+BD_ACCEL(6,IK)*BD_ACCEL(6,IK)
        VB = SQRT(VBSQ)
!
        DO JJ=1,3
         SUM1GM(JJ,IK)=BD_ACCEL(10,IK)*(Ra_ik(JJ))/RAIK3
        END DO
        !SUM1GM
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!   Partial Derivatives GM
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        DO JJ=1,3
          SUM1DGM(JJ,IK)=(Ra_ik(JJ))/RAIK3
        END DO

!
        !SUM1G

        DO ICA=1,IPPN_BODIES
         IF (IBODY.EQ.2.AND.ICA.EQ.12) GOTO 2234
         IF (IBODY.EQ.6.AND.ICA.EQ.IBV(ICBDGM)) GOTO 2234
         IF (IBODY.EQ.6.AND.ICA.EQ.12) GOTO 2234
         IF (IBODY.EQ.5.AND.ICA.EQ.12) GOTO 2234
         IF (IBODY.EQ.4.AND.ICA.EQ.8)  GOTO 2234
         IF (IBODY.EQ.3.AND.ICA.EQ.12)  GOTO 2234
!
!IF( ICA.NE.A)
         Rc_a(1)=XP(1,1)-BD_ACCEL(1,ICA)
         Rc_a(2)=XP(2,1)-BD_ACCEL(2,ICA)
         Rc_a(3)=XP(3,1)-BD_ACCEL(3,ICA)
         RCA2 = Rc_a(1)*Rc_a(1)+Rc_a(2)*Rc_a(2)+         &
     &             Rc_a(3)*Rc_a(3)
         RCA  = SQRT(RCA2)
         RCA3 = RCA2*RCA
!
         DO JJ=1,3
           SUM1G(JJ,IK)=SUM1G(JJ,IK)+BD_ACCEL(10,ICA)/RCA
         END DO

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !!!   Partial Derivatives GM
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         IF (ICA.EQ.8) THEN
         DO JJ=1,3
           TERM1DGM(JJ,IK)=TERM1DGM(JJ,IK)+2.D0*BD_ACCEL(10,ICA)/RCA*   &
     &     (2.0D0*(IPPN_BETA+IPPN_GAMMA)/VLIGHT2)
         END DO
         ELSE
         DO JJ=1,3
           TERM1DGM(JJ,IK)=TERM1DGM(JJ,IK)+BD_ACCEL(10,ICA)/RCA*        &
     &     (2.0D0*(IPPN_BETA+IPPN_GAMMA)/VLIGHT2)
         END DO
         END IF

2234  CONTINUE
        END DO

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!   Partial Derivatives Gamma and Beta
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        DO JJ=1,3

        DBETA(JJ)=DBETA(JJ)-SUM1GM(JJ,IK)*(2.0D0/VLIGHT2)*SUM1G(JJ,IK)
        DGAMMA(JJ)=DGAMMA(JJ)-SUM1GM(JJ,IK)*(2.0D0/VLIGHT2)*SUM1G(JJ,IK)

        SUM1G(JJ,IK)=SUM1G(JJ,IK)*(2.0D0*(IPPN_BETA+IPPN_GAMMA)/VLIGHT2)

        END DO
        !SUM1G
!
        !SUM1F

        DO ICB=1,IPPN_BODIES
!
         IF (ICB.NE.IK) THEN

         Rc_b(1)=BD_ACCEL(1,IK)-BD_ACCEL(1,ICB)
         Rc_b(2)=BD_ACCEL(2,IK)-BD_ACCEL(2,ICB)
         Rc_b(3)=BD_ACCEL(3,IK)-BD_ACCEL(3,ICB)
         RCB2 = Rc_b(1)*Rc_b(1)+Rc_b(2)*Rc_b(2)+                        &
     &             Rc_b(3)*Rc_b(3)
         RCB  = SQRT(RCB2)
         RCB3 = RCB2*RCB
!
         DO JJ=1,3
          SUM1F(JJ,IK)=SUM1F(JJ,IK)+BD_ACCEL(10,ICB)/RCB
         END DO

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!   Partial Derivatives GM
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         IF (IBODY.EQ.4.AND.ICB.EQ.8) THEN
         DO JJ=1,3
          TERM2DGM(JJ,IK)=1.0D0/RCB*((2.0D0*IPPN_BETA-1.0D0)/VLIGHT2)
         END DO
         END IF
         IF (IBODY.NE.4) THEN
         DO JJ=1,3
          TERM2DGM(JJ,IK)=TERM2DGM(JJ,IK)+BD_ACCEL(10,ICB)/RCB*         &
     &                    ((2.0D0*IPPN_BETA-1.0D0)/VLIGHT2)
         END DO
         END IF

         END IF
3234  CONTINUE
        END DO

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!   Partial Derivatives Gamma and Beta
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        DO JJ=1,3

        DBETA(JJ)=DBETA(JJ)-SUM1GM(JJ,IK)*(2.0D0/VLIGHT2)*SUM1F(JJ,IK)

        SUM1F(JJ,IK)=SUM1F(JJ,IK)*((2.0D0*IPPN_BETA-1.0D0)/VLIGHT2)

        END DO
        !SUM1F
!
!!!!! DOT PRODUCTS
!
        VAVB=(XP(1,2)*BD_ACCEL(4,IK)+XP(2,2)*BD_ACCEL(5,IK)+XP(3,2) &
     &          *BD_ACCEL(6,IK))
        RVB=-(Ra_ik(1)*BD_ACCEL(4,IK)+Ra_ik(2)*BD_ACCEL(5,IK)+Ra_ik(3) &
     &          *BD_ACCEL(6,IK))
        RAB=(Ra_ik(1)*BD_ACCEL(7,IK)+Ra_ik(2)*BD_ACCEL(8,IK)+Ra_ik(3) &
     &          *BD_ACCEL(9,IK))

!
        !SUM1AE
        DO JJ=1,3
        SUM1AE(JJ,IK)=SUM1AE(JJ,IK)+IPPN_GAMMA*(VA/VLIGHT)*(VA/VLIGHT)
        SUM1AE(JJ,IK)=SUM1AE(JJ,IK)+((1.0D0+IPPN_GAMMA)*(VB/VLIGHT)*    &
     &                         (VB/VLIGHT))   !SUM1D
        SUM1AE(JJ,IK)=SUM1AE(JJ,IK)-(2.0D0*(1.0D0+IPPN_GAMMA)/VLIGHT2)* &
     &                VAVB
        SUM1AE(JJ,IK)=SUM1AE(JJ,IK)+(-3.0D0/(2.0D0*VLIGHT2))*(RVB/      &
     &                RAIK)**2.D0
        SUM1AE(JJ,IK)=SUM1AE(JJ,IK)+((RAB)/(2.0D0*VLIGHT2))   !SUM1A

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!   Partial Derivatives GM
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        TERM3DGM(JJ,IK)=SUM1AE(JJ,IK)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!   Partial Derivatives Gamma and Beta
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        DGAMMA(JJ)=DGAMMA(JJ)+((VA/VLIGHT)*(VA/VLIGHT)+(VB/VLIGHT)*     &
     &(VB/VLIGHT) - 2.D0*VAVB/VLIGHT2) * SUM1GM(JJ,IK)

        END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        !SUM1AE
!
        DO JJ=1,3
         SUM1(JJ,IK)=SUM1(JJ,IK)+SUM1GM(JJ,IK)*                         &
     &                 (1.0D0-SUM1G(JJ,IK)-SUM1F(JJ,IK)+SUM1AE(JJ,IK))
!
! When calculating acceleration from sun on asteroid, ignore
! newtonian contribution (only want relativistic effect)
! Last IF is necessary to exclude the Newtonian contribution of the asteroid
        IF (IBODY.EQ.3.AND.IK.EQ.8) THEN
          SUM1(JJ,IK)=SUM1(JJ,IK)-SUM1GM(JJ,IK)
        END IF
        IF (IBODY.EQ.1.AND.IK.EQ.12) THEN
         SUM1(JJ,IK)=SUM1(JJ,IK)-SUM1GM(JJ,IK)
        END IF
        IF (IBODY.EQ.4.AND.IK.EQ.12) THEN
         SUM1(JJ,IK)=SUM1(JJ,IK)-SUM1GM(JJ,IK)
        END IF
        IF (IBODY.EQ.5.AND.IK.EQ.IBV(ICBDGM)) THEN
         SUM1(JJ,IK)=SUM1(JJ,IK)-SUM1GM(JJ,IK)
        END IF

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!   Partial Derivatives GM
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF (IK.EQ.8) THEN
          DGM(JJ)=SUM1DGM(JJ,IK)*(1.0D0-TERM1DGM(JJ,IK)-TERM2DGM(JJ,IK)+&
     &                                  TERM3DGM(JJ,IK))
        END IF

        IF (IBODY.EQ.4) THEN
          DGM(JJ)=BD_ACCEL(10,IK)*SUM1DGM(JJ,IK)*(-TERM2DGM(JJ,IK))
        ENDIF

!!        WRITE(6,87657)  SUM1G(1,IK), SUM1F(1,IK), SUM1AE(1,IK), IK
!!        WRITE(6,87658)  BD_ACCEL(10,IK)
        END DO


1234  CONTINUE
       END DO
!!! SUM1
!
!
!!! SUM2

        DO JK=1,IPPN_BODIES
!! IF (A.NE.B).. B=JK

         IF (IBODY.EQ.2.AND.JK.EQ.12) GOTO 1235
         IF (IBODY.EQ.6.AND.JK.EQ.IBV(ICBDGM)) GOTO 1235
         IF (IBODY.EQ.6.AND.JK.EQ.12) GOTO 1235
         IF (IBODY.EQ.5.AND.JK.EQ.12) GOTO 1235
         IF (IBODY.EQ.4.AND.JK.EQ.8)  GOTO 1235
         IF (IBODY.EQ.3.AND.JK.EQ.12)  GOTO 1235
!
         Ra_ik(1)=BD_ACCEL(1,JK)-XP(1,1)
         Ra_ik(2)=BD_ACCEL(2,JK)-XP(2,1)
         Ra_ik(3)=BD_ACCEL(3,JK)-XP(3,1)
         RAIK2 = Ra_ik(1)*Ra_ik(1)+Ra_ik(2)*Ra_ik(2)+Ra_ik(3)*Ra_ik(3)
         RAIK  = SQRT(RAIK2)
         RAIK3 = RAIK2*RAIK
!
         RVA=-(Ra_ik(1)*XP(1,2)+Ra_ik(2)*XP(2,2)+Ra_ik(3)*XP(3,2))
         RVB=-(Ra_ik(1)*BD_ACCEL(4,JK)+Ra_ik(2)*BD_ACCEL(5,JK)+Ra_ik(3) &
     &          *BD_ACCEL(6,JK))

         DO JJ=1,3
         SUM2(JJ,JK)=BD_ACCEL(10,JK)/RAIK3/VLIGHT2
         SUM2(JJ,JK)=SUM2(JJ,JK)*(RVA*(2.0D0+2.0D0*IPPN_GAMMA)-        &
     &               RVB*(1.0D0+2.0D0*IPPN_GAMMA))
         SUM2(JJ,JK)=SUM2(JJ,JK)*(XP(JJ,2)-BD_ACCEL(3+JJ,JK))

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!   Partial Derivatives GM
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF (JK.EQ.8) THEN
          DGM(JJ)=DGM(JJ)+SUM2(JJ,JK)/BD_ACCEL(10,JK)
        END IF

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!   Partial Derivatives Gamma and Beta
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         DGAMMA(JJ)=DGAMMA(JJ)+BD_ACCEL(10,JK)/RAIK3/VLIGHT2*(RVA*     &
     &(2.0D0)-RVB*(2.0D0))*(XP(JJ,2)-BD_ACCEL(3+JJ,JK))
         END DO

!!        WRITE(6,87659)  SUM2(1:3,JK), JK
1235  CONTINUE
        END DO


!!! SUM2
!
!
!!!! SUM3
        DO KK=1,IPPN_BODIES
!!! IF (A.NE.B).. B=KK

         IF (IBODY.EQ.2.AND.KK.EQ.12) GOTO 1236
         IF (IBODY.EQ.6.AND.KK.EQ.IBV(ICBDGM)) GOTO 1236
         IF (IBODY.EQ.6.AND.KK.EQ.12) GOTO 1236
         IF (IBODY.EQ.5.AND.KK.EQ.12) GOTO 1236
         IF (IBODY.EQ.4.AND.KK.EQ.8)  GOTO 1236
         IF (IBODY.EQ.3.AND.KK.EQ.12)  GOTO 1236
!
         Ra_ik(1)=BD_ACCEL(1,KK)-XP(1,1)
         Ra_ik(2)=BD_ACCEL(2,KK)-XP(2,1)
         Ra_ik(3)=BD_ACCEL(3,KK)-XP(3,1)
         RAIK2 = Ra_ik(1)*Ra_ik(1)+Ra_ik(2)*Ra_ik(2)+Ra_ik(3)*Ra_ik(3)
         RAIK  = SQRT(RAIK2)
         RAIK3 = RAIK2*RAIK
         DO JJ=1,3
         SUM3(JJ,KK)=BD_ACCEL(10,KK)*BD_ACCEL(6+JJ,KK)/RAIK
         END DO
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!   Partial Derivatives Gamma and Beta
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        DO JJ=1,3
         DGAMMA(JJ)=DGAMMA(JJ)+SUM3(JJ,KK)*((4.0D0)/(2.0D0*VLIGHT2))

         SUM3(JJ,KK)=SUM3(JJ,KK)*((3.0D0+4.0D0*IPPN_GAMMA)/             &
     &               (2.0D0*VLIGHT2))
        END DO
!        WRITE(6,87660)  SUM3(1:3,KK), KK

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!   Partial Derivatives GM
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF (KK.EQ.8) THEN
        DO JJ=1,3
          DGM(JJ)=DGM(JJ)+SUM3(JJ,KK)/BD_ACCEL(10,KK)
        END DO
        END IF
1236  CONTINUE
        END DO
! SUM3
!
!
!!!
!!!   SUM SUM1,SUM2, and SUM3 FOR ACCEL
!!!
!
       DO KK=1,IPPN_BODIES
        DO JJ=1,3
         REL_PM_ACCEL(JJ,KK)=SUM1(JJ,KK)+SUM2(JJ,KK)+SUM3(JJ,KK)
        END DO

       END DO
!
       DO KK=1,IPPN_BODIES
         DO JJ=1,3
         REL_ACC(JJ)=REL_ACC(JJ)+REL_PM_ACCEL(JJ,KK)
         END DO
       END DO
!
      RETURN
      END
