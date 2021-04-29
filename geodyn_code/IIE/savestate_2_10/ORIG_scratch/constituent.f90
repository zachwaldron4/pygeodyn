      character*4 function constituent( doodson_number )
!
!  Returns the Darwin-Kelvin tidal constituent name for a particular
!  6-vector Doodson number.
!  (Note: This number should NOT have the additional "5"'s added to them.)
!  A blank name is returned if the input Doodson number does not
!  correspond to any commonly accepted constituent name.
!
!  Language: Fortran 90
!
      implicit none
      INTEGER      doodson_number(6)

      INTEGER      k(6),k1,k2
      character    name*4
      intrinsic    ALL

      k = doodson_number
      k1 = k(1)
      k2 = k(2)
      name = '    '

!     Long-period tides
!     -----------------
      if (k1 == 0) then
         if (k2 == 0) then
            if (ALL( k==(/ 0, 0, 1, 0, 0, 0 /) )) then
               name = 'Sa  '
            else if (ALL( k==(/ 0, 0, 2, 0, 0, 0 /) )) then
               name = 'Ssa '
            else if (ALL( k==(/ 0, 0, 0, 0, 1, 0 /) )) then
               name = 'Node'
            end if
         else if (k2 == 1) then
            if (ALL( k==(/ 0, 1,-2, 1, 0, 0 /) )) then
               name = 'MSm '
            else if (ALL( k==(/ 0, 1, 0,-1, 0, 0 /) )) then
               name = 'Mm  '
            end if
         else if (k2 == 2) then
            if (ALL( k==(/ 0, 2,-2, 0, 0, 0 /) )) then
               name = 'MSf '
            else if (ALL( k==(/ 0, 2, 0, 0, 0, 0 /) )) then
               name = 'Mf  '
            end if
         else if (k2 == 3) then
            if (ALL( k==(/ 0, 3,-2, 1, 0, 0 /) )) then
               name = 'MSt '
            else if (ALL( k==(/ 0, 3, 0,-1, 0, 0 /) )) then
               name = 'Mt  '
            end if
         else if (k2 == 4) then
            if (ALL( k==(/ 0, 4,-2, 0, 0, 0 /) )) then
               name = 'MSq '
            else if (ALL( k==(/ 0, 4, 0,-2, 0, 0 /) )) then
               name = 'Mq  '
            end if
         end if

!     Diurnal tides
!     -------------
      else if (k1 == 1) then
         if (k2 == -4) then
            if (ALL( k==(/ 1,-4, 2, 1, 0, 0 /) )) then
               name = 'alp1'
            end if
         else if (k2 == -3) then
            if (ALL( k==(/ 1,-3, 0, 2, 0, 0 /) )) then
               name = '2Q1 '
            else if (ALL( k==(/ 1,-3, 2, 0, 0, 0 /) )) then
               name = 'sig1'
            end if
         else if (k2 == -2) then
            if (ALL( k==(/ 1,-2, 0, 1, 0, 0 /) )) then
               name = 'Q1  '
            else if (ALL( k==(/ 1,-2, 2,-1, 0, 0 /) )) then
               name = 'rho1'
            end if
         else if (k2 == -1) then
            if (ALL( k==(/ 1,-1, 0, 0, 0, 0 /) )) then
               name = 'O1  '
            else if (ALL( k==(/ 1,-1, 2, 0, 0, 0 /) )) then
               name = 'tau1'
            end if
         else if (k2 == 0) then
            if (ALL( k==(/ 1, 0,-2, 1, 0, 0 /) )) then
               name = 'bet1'
            else if (ALL( k==(/ 1, 0, 0, 0, 0, 0 /) )) then
               name = 'M1'' '
            else if (ALL( k==(/ 1, 0, 0, 1, 0, 0 /) )) then
               name = 'M1  '
            else if (ALL( k==(/ 1, 0, 2,-1, 0, 0 /) )) then
               name = 'chi1'
            end if
         else if (k2 == 1) then
            if (ALL( k==(/ 1, 1,-3, 0, 0, 1 /) )) then
               name = 'pi1 '
            else if (ALL( k==(/ 1, 1,-2, 0, 0, 0 /) )) then
               name = 'P1  '
            else if (ALL( k==(/ 1, 1,-1, 0, 0, 0 /) )) then
               name = 'S1  '
            else if (ALL( k==(/ 1, 1, 0, 0, 0, 0 /) )) then
               name = 'K1  '
            else if (ALL( k==(/ 1, 1, 1, 0, 0,-1 /) )) then
               name = 'psi1'
            else if (ALL( k==(/ 1, 1, 2, 0, 0, 0 /) )) then
               name = 'phi1'
            end if
         else if (k2 == 2) then
            if (ALL( k==(/ 1, 2,-2, 1, 0, 0 /) )) then
               name = 'the1'
            else if (ALL( k==(/ 1, 2, 0,-1, 0, 0 /) )) then
               name = 'J1  '
            end if
         else if (k2 == 3) then
            if (ALL( k==(/ 1, 3,-2, 0, 0, 0 /) )) then
               name = 'SO1 '
            else if (ALL( k==(/ 1, 3, 0, 0, 0, 0 /) )) then
               name = 'OO1 '
            end if
         else if (k2 == 4) then
            if (ALL( k==(/ 1, 4, 0,-1, 0, 0 /) )) then
               name = 'ups1'
            end if
         end if


!     Semidiurnal tides
!     -----------------
      else if (k1 == 2) then
         if (k2 == -3) then
            if (ALL( k==(/ 2,-3, 2, 1, 0, 0 /) )) then
               name = 'eps2'
            end if
         else if (k2 == -2) then
            if (ALL( k==(/ 2,-2, 0, 2, 0, 0 /) )) then
               name = '2N2 '
            else if (ALL( k==(/ 2,-2, 2, 0, 0, 0 /) )) then
               name = 'mu2 '
            end if
         else if (k2 == -1) then
            if (ALL( k==(/ 2,-1, 0, 1, 0, 0 /) )) then
               name = 'N2  '
            else if (ALL( k==(/ 2,-1, 2,-1, 0, 0 /) )) then
               name = 'nu2 '
            end if
         else if (k2 == 0) then
            if (ALL( k==(/ 2, 0,-2, 0, 0, 0 /) )) then
               name = 'OP2 '
            else if (ALL( k==(/ 2, 0,-2, 2, 0, 0 /) )) then
               name = 'Gam2'
!!          else if (ALL( k==(/ 2, 0,-1, 0, 0, 1 /) )) then
!!             name = 'alp2'
            else if (ALL( k==(/ 2, 0,-1, 0, 0, 1 /) )) then
               name = 'M2a '
            else if (ALL( k==(/ 2, 0, 0, 0, 0, 0 /) )) then
               name = 'M2  '
            else if (ALL( k==(/ 2, 0, 1, 0, 0,-1 /) )) then
               name = 'M2b '
            else if (ALL( k==(/ 2, 0, 2, 0, 0, 0 /) )) then
               name = 'del2'
            end if
         else if (k2 == 1) then
            if (ALL( k==(/ 2, 1,-2, 1, 0, 0 /) )) then
               name = 'lam2'
            else if (ALL( k==(/ 2, 1, 0,-1, 0, 0 /) )) then
               name = 'L2  '
            endif
         else if (k2 == 2) then
            if (ALL( k==(/ 2, 2,-3, 0, 0, 1 /) )) then
               name = 'T2  '
            else if (ALL( k==(/ 2, 2,-2, 0, 0, 0 /) )) then
               name = 'S2  '
            else if (ALL( k==(/ 2, 2,-1, 0, 0,-1 /) )) then
               name = 'R2  '
            else if (ALL( k==(/ 2, 2, 0, 0, 0, 0 /) )) then
               name = 'K2  '
            endif
         else if (k2 == 3) then
            if (ALL( k==(/ 2, 3,-2, 1, 0, 0 /) )) then
               name = 'zet2'
            else if (ALL( k==(/ 2, 3, 0,-1, 0, 0 /) )) then
               name = 'eta2'
            endif
         else if (k2 == 4) then
            if (ALL( k==(/ 2, 4,-4, 0, 0, 0 /) )) then
               name = '2SM2'
            end if
         end if

!     Terdiurnal tides
!     ----------------
      else if (k1 == 3) then
         if (k2 == 0) then
            if (ALL( k==(/ 3, 0, 0, 0, 0, 0 /) )) then
               name = 'M3  '
            end if
         else if (k2 == 1) then
            if (ALL( k==(/ 3, 1, 0, 0, 0, 0 /) )) then
               name = 'MK3 '
            end if
         else if (k2 == 3) then
            if (ALL( k==(/ 3, 3,-5, 0, 0, 0 /) )) then
               name = 'U3  '
            else if (ALL( k==(/ 3, 3,-4, 0, 0, 0 /) )) then
               name = 'T3  '
            else if (ALL( k==(/ 3, 3,-3, 0, 0, 0 /) )) then
               name = 'S3  '
            else if (ALL( k==(/ 3, 3,-2, 0, 0, 0 /) )) then
               name = 'R3  '
            endif
         end if

!     Quarter-diurnal tides
!     ---------------------
      else if (k1 == 4) then
         if (k2 == -1) then
            if (ALL( k==(/ 4,-1, 0, 1, 0, 0 /) )) then
               name = 'MN4 '
            end if
         else if (k2 == 0) then
            if (ALL( k==(/ 4, 0, 0, 0, 0, 0 /) )) then
               name = 'M4  '
            end if
         else if (k2 == 2) then
            if (ALL( k==(/ 4, 2,-2, 0, 0, 0 /) )) then
               name = 'MS4 '
            else if (ALL( k==(/ 4, 2, 0, 0, 0, 0 /) )) then
               name = 'MK4 '
            end if
         else if (k2 == 4) then
            if (ALL( k==(/ 4, 4,-5, 0, 0, 0 /) )) then
               name = 'T4  '
            else if (ALL( k==(/ 4, 4,-4, 0, 0, 0 /) )) then
               name = 'S4  '
            else if (ALL( k==(/ 4, 4,-3, 0, 0, 0 /) )) then
               name = 'R4  '
            end if
         endif

!
!     A few more higher-order tides, mostly solar harmonics
!     -----------------------------------------------------
      else
         if (ALL( k==(/ 5, 5,-5, 0, 0, 0 /) )) then
            name = 'S5  '
         else if (ALL( k==(/ 6, 0, 0, 0, 0, 0 /) )) then
            name = 'M6  '
         else if (ALL( k==(/ 6, 6,-6, 0, 0, 0 /) )) then
            name = 'S6  '
         else if (ALL( k==(/ 7, 7,-7, 0, 0, 0 /) )) then
            name = 'S7  '
         else if (ALL( k==(/ 8, 0, 0, 0, 0, 0 /) )) then
            name = 'M8  '
         else if (ALL( k==(/ 8, 8,-8, 0, 0, 0 /) )) then
            name = 'S8  '
         end if
      end if

      constituent = name
      return
      end
