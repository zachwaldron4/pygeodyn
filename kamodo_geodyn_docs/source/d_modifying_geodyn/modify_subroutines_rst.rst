#######################
Modifying GEODYN
#######################


List of Major Modifications
=============================

Below is a list of all the major modification made to the version of GEODYN on the AWS server:

+-------+------------------------------------------+----------------------+-----------------------------+--------------+
| Mod # | Short Description                        | Directory of edit    | Files Edited                |  Output      |
+=======+==========================================+======================+=============================+==============+
| 1     | Print Model Density                      | `IIE/pygeodyn_MODS/` | ``DRAG.f90``                | ``fort.99``  |
+-------+------------------------------------------+----------------------+-----------------------------+--------------+
| 2     | ``iieout`` density model identifier      | `IIE/pygeodyn_MODS/` |``SUMMRY.f90``               | ``iieout``   |
+-------+------------------------------------------+----------------------+-----------------------------+--------------+
| 3     | Include MSISe00  to GEODYN Source code   | `IIE/pygeodyn_MODS/` | See the Adding MSIS docs    | see docs     |
+-------+------------------------------------------+----------------------+-----------------------------+--------------+
| 4     | Include MSIS 2.0 to GEODYN Source code   | `IIE/pygeodyn_MODS/` | See the Adding MSIS docs    | see docs     |
+-------+------------------------------------------+----------------------+-----------------------------+--------------+
| 5     | Modying the MSIS DRHODZ Calculation      | `IIE/pygeodyn_MODS/` | ``MSIS.f90``                | N/A          |
+-------+------------------------------------------+----------------------+-----------------------------+--------------+
| 6     | Remove the ATGRAV printing to IIS/IIEOUT | `IIS/ORIG/`          | ``ALLOAD.f90``              |              |
+-------+------------------------------------------+----------------------+-----------------------------+--------------+


.. note:: If one modifies the GEODYN source code, you must recompile using the compiler script in that directory to reconstruct the executable.


Modification Details
=============================



1) Print Model Density
------------------------------------------------------------------



**Description:**  

.. _ * LSTINR is last inner iteration!
.. _ * FSSTRT is seconds since initial epoch!

* If on the last iteration and 200 seconds have passed (``IF(LSTINR.AND.FSSTRT.GT.200.D0) THEN``), it prints the parameters to a file ``fort.99`` using the format stored at ``7000`` (arbitrary value).  The format is such that there are 14 parameters being stored and 14 data-types being identified for each parameter.  


**Fortran WRITE Formatting:**  

*  ``F12.1``   -- reserves 12 spaces (including '.' and '-') for a real floating-point number with 1 decimal place.  
*  ``2I8``     -- reserves 8 spaces for an integer.  The 2 at the front indicates to do this twice.  
*  ``2F12.4``  -- reserves 12 spaces (including '.' and '-') for a real floating-point number with 4 decimal place.  The 2 indicates to do this twice.  
*  ``2D20.11`` -- the D specifier is for the exponential form of decimal double-precision items.  
*  ``6(3X,F12.4)`` -- `3x` places 3 spaces before the `F12.4`.  The 6 and parenthesis indicates to repeat the formation 6 times.  



**Code to include:**

.. code-block:: fortran

    !     ##########    WRITE TO A DENSITY FILE        ##############
    !
      160 CONTINUE 
    !
          if(kentry.eq.1)then
              WRITE(6,*) 'CHECK DENSITY UNITS: rho=', RHO
              WRITE(6,*) 'CHECK DENSITY UNITS: drhodz=', DRHODZ
          endif 
          XDOTR=XDOT+THDOT*HTSCAL*Y
          YDOTR=YDOT-THDOT*HTSCAL*X
          IF(LSTINR.AND.FSSTRT.GT.200.D0) THEN
            WRITE(99,7000) FSSTRT,IYMD,IHMS,XLATD,XLOND,ALTI,RHO,DRHODZ,    &
         &                 X,Y,Z,XDOT,YDOT,ZDOT     
    !    FSSTRT -- Elapsed seconds since initial epoch  
    !    IYMD   -- YYMMDD of current epoch  
    !    IHMS   -- HHMMSS of current epoch  
    !    XLATD  -- Lat of Space station (degrees)  
    !    XLOND  -- Lon of Space station (degrees)  
    !    ALTI   -- Hieght of Space station above elipsoid (M)  
    !    RHO    -- Density of atmosphere (KG/M^3)  
    !    DRHODZ -- Change in density of atmosphere with height (KG/M^3/M)  
    !    X      -- X component of position vector  
    !    Y      -- Y component of position vector  
    !    Z      -- Z component of position vector  
    !    XDOT   -- X component of velocity vector  
    !    YDOT   -- Y component of velocity vector  
    !    ZDOT   -- Z component of velocity vector
    !
     7000   FORMAT(F12.1,2I8,2F12.4,F12.3,2D20.11,3F15.3,3F15.6)
          ENDIF
          GO TO 200
    !      
    !################################################################




2) iieout density model identifier
------------------------------------------------------------------

**Description:** 

There was an error where the IIEOUT printout showed the that atmospheric model being used was "JAACHIA9999" when MSIS86 was selected.  The ``SUMMRY.f90`` subroutine was edited to fix this.  

**Code to include:**

.. code-block:: fortran

    !(Line 365)
    !!!!  900 WRITE(IOUT6,10500) IYATDN(JATDEN)
    !!!!      WRITE(IUNT88,10500) IYATDN(JATDEN)
          IF(IATDN.EQ.2) WRITE(6,10551)
          IF(IATDN.EQ.4) WRITE(6,10552)
          IF(IATDN.EQ.5) WRITE(6,10553)
      950 CONTINUE
    !
    ...
    !
    !(Line 880)
    10551 FORMAT(' ATMOSPHERIC DENSITY MODEL  USED: JAACHIA 71')
    10552 FORMAT(' ATMOSPHERIC DENSITY MODEL  USED: FRENCH DTM')
    10553 FORMAT(' ATMOSPHERIC DENSITY MODEL  USED: MSIS')
    
    
    
    
    
    
    
    
    
3) and 4) Include MSISe00 and MSISe2 into the GEODYN Source code
------------------------------------------------------------------

**Description**:

We upgraded GEODYN to include the newer versions of the MSIS empirical models. 
 These are:

* NRLMSISE-00  
* NRLMSISe2.0

For an in-depth explanation on this process,  `please see the included MSIS PDF <../_static/msis_gII_update_instructions.pdf>`_. 








5) Modying the MSIS DRHODZ Calculation 
---------------------------------------------------

**Description**:

``DRHODZ`` is the calculation of the partial derivative of density with respect to altitude.  It is calculated in the subroutines of each respective density model and returned to the DRAG.f90 subroutine to then be used in the drag acceleration calculation.  The calculation of ``DRHODZ`` in ``MSIS.f90`` had a few errors which we fixed. We also modified this calculation to include Anomolous Oxygen (an included output in MSISe00 and MSISe2) and ``O2`` which was omitted previously.

For an in-depth explanation on this process,  `please see the included DRHODZ PDF <../_static/DrhoDz_Modification.pdf>`_. 






6) Remove the ATGRAV printing to IIS/IIEOUT
---------------------------------------------------

**Description**:

When constructing the final ``iieout`` file, the printout from ``iisout`` is appended to the front of the ``iieout`` to allow for a complete view of the run output.  The ``iisout`` printout had a line of code (nested in the ``IIS/ORIG/ALLOAD.f90`` subroutine) that was likely meant for debugging, but had been left in the version of GEODYN handed down to us.  This line printed all of the gravitational coefficients from the ``ATGRAV`` file to ``iisout``, using up about ~60000 lines and ~80 MB of storage space per ``iieout`` file...

I commented out these lines in ``ALLOAD.f90`` to remove the unnecssary prints and save space in the final ``iieout`` file.


