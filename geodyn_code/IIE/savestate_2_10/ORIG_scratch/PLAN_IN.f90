      SUBROUTINE PLAN_IN ( PLAN, PLANET , PLANV ,IUER )
! ************************************************************************
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   PLANET                  Planet coordinates in the SSBC system      *
! *   PLANV                   Planet velocities in the SSBC system      *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###              VTD_PLAN_IN  v1.1 (c)  D. Pavlis                 *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      DOUBLE PRECISION PLANET, PLANV,PLAN(3,2,11)
      INTEGER  IUER
      DIMENSION   PLANET(1,3,11), PLANV(1,3,11)
!
! Load SUN
      PLAN(1,1,1)=PLANET(1,1,8)
      PLAN(2,1,1)=PLANET(1,2,8)
      PLAN(3,1,1)=PLANET(1,3,8)
      PLAN(1,2,1)=PLANV(1,1,8)
      PLAN(2,2,1)=PLANV(1,2,8)
      PLAN(3,2,1)=PLANV(1,3,8)
!
! Load MERCURY
      PLAN(1,1,2)=PLANET(1,1,10)
      PLAN(2,1,2)=PLANET(1,2,10)
      PLAN(3,1,2)=PLANET(1,3,10)
      PLAN(1,2,2)=PLANV(1,1,10)
      PLAN(2,2,2)=PLANV(1,2,10)
      PLAN(3,2,2)=PLANV(1,3,10)
!
! Load VENUS
      PLAN(1,1,3)=PLANET(1,1,1)
      PLAN(2,1,3)=PLANET(1,2,1)
      PLAN(3,1,3)=PLANET(1,3,1)
      PLAN(1,2,3)=PLANV(1,1,1)
      PLAN(2,2,3)=PLANV(1,2,1)
      PLAN(3,2,3)=PLANV(1,3,1)
!
! Load EARTH
      PLAN(1,1,4)=PLANET(1,1,9)
      PLAN(2,1,4)=PLANET(1,2,9)
      PLAN(3,1,4)=PLANET(1,3,9)
      PLAN(1,2,4)=PLANV(1,1,9)
      PLAN(2,2,4)=PLANV(1,2,9)
      PLAN(3,2,4)=PLANV(1,3,9)

!
! Load MARS
      PLAN(1,1,5)=PLANET(1,1,2)
      PLAN(2,1,5)=PLANET(1,2,2)
      PLAN(3,1,5)=PLANET(1,3,2)
      PLAN(1,2,5)=PLANV(1,1,2)
      PLAN(2,2,5)=PLANV(1,2,2)
      PLAN(3,2,5)=PLANV(1,3,2)
!
! Load JUPITER
      PLAN(1,1,6)=PLANET(1,1,3)
      PLAN(2,1,6)=PLANET(1,2,3)
      PLAN(3,1,6)=PLANET(1,3,3)
      PLAN(1,2,6)=PLANV(1,1,3)
      PLAN(2,2,6)=PLANV(1,2,3)
      PLAN(3,2,6)=PLANV(1,3,3)
!
! Load SATURN
      PLAN(1,1,7)=PLANET(1,1,4)
      PLAN(2,1,7)=PLANET(1,2,4)
      PLAN(3,1,7)=PLANET(1,3,4)
      PLAN(1,2,7)=PLANV(1,1,4)
      PLAN(2,2,7)=PLANV(1,2,4)
      PLAN(3,2,7)=PLANV(1,3,4)
!
! Load URANUS
      PLAN(1,1,8)=PLANET(1,1,5)
      PLAN(2,1,8)=PLANET(1,2,5)
      PLAN(3,1,8)=PLANET(1,3,5)
      PLAN(1,2,8)=PLANV(1,1,5)
      PLAN(2,2,8)=PLANV(1,2,5)
      PLAN(3,2,8)=PLANV(1,3,5)
!
! Load NEPTUNE
      PLAN(1,1,9)=PLANET(1,1,6)
      PLAN(2,1,9)=PLANET(1,2,6)
      PLAN(3,1,9)=PLANET(1,3,6)
      PLAN(1,2,9)=PLANV(1,1,6)
      PLAN(2,2,9)=PLANV(1,2,6)
      PLAN(3,2,9)=PLANV(1,3,6)
!
! Load PLUTO
      PLAN(1,1,10)=PLANET(1,1,7)
      PLAN(2,1,10)=PLANET(1,2,7)
      PLAN(3,1,10)=PLANET(1,3,7)
      PLAN(1,2,10)=PLANV(1,1,7)
      PLAN(2,2,10)=PLANV(1,2,7)
      PLAN(3,2,10)=PLANV(1,3,7)
!
! Load EARTH'S MOON
      PLAN(1,1,11)=PLANET(1,1,11)
      PLAN(2,1,11)=PLANET(1,2,11)
      PLAN(3,1,11)=PLANET(1,3,11)
      PLAN(1,2,11)=PLANV(1,1,11)
      PLAN(2,2,11)=PLANV(1,2,11)
      PLAN(3,2,11)=PLANV(1,3,11)
!
      RETURN
      END  SUBROUTINE  PLAN_IN
