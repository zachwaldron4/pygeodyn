#######################
Modifying GEODYN
#######################

To make modifications to GEODYN, we use the methodology setup by the creators in which we copy the subroutines we wish to overwrite into a new directory and edit them.  At the time of compile, all subroutines in the ORIG/ directory are copied to there, and overwritten by their counterparts in the mods directory.

.. note:: If one modifies the GEODYN source code, you must recompile using the compiler script in that directory to reconstruct the executable.


Modifications to IIE:
====================================

As of Feb. 2022, the most up-to-date MOD directory is ``/data/geodyn_proj/geodyn_code/IIE/CD_model_proj``.  

Below is a list of some of the major modifications made to the version of GEODYN we use on the AWS server (as of Feb. 2022).  All modifications were made to IIE:  

 1. Constructed a ``density_file`` (fort.99) to be written in the ``DRAG.f90``      
 
 2. The ``SUMMRY.f90`` subroutine was edited to fixan error where the IIEOUT printout showed the that atmospheric model being used was "JAACHIA9999" when MSIS86 was selected.  

 3. Added the MSISe00 density model  
     
     - For an in-depth explanation on this process,  `please see the included MSIS PDF <../_static/msis_gII_update_instructions.pdf>`_.   
 
 4. Added the MSIS 2.0 density model  
     
     - For an in-depth explanation on this process,  `please see the included MSIS PDF <../_static/msis_gII_update_instructions.pdf>`_.   
       
 5. Made the DRHODZ calculation done in each of the MSIS routines more accurate   
     
     - ``DRHODZ`` is the calculation of the partial derivative of density with respect to altitude.  It is calculated in the subroutines of each respective density model and returned to the DRAG.f90 subroutine to then be used in the drag acceleration calculation.  The calculation of ``DRHODZ`` in ``MSIS.f90`` had a few errors which we fixed. We also modified this calculation to include Anomolous Oxygen (an included output in MSISe00 and MSISe2) and ``O2`` which was omitted previously.  For an in-depth explanation on this process,  `please see the included DRHODZ PDF <../_static/DrhoDz_Modification.pdf>`_.  
     
 6. Added the JB2008 density model  
 
 7. Added the DTM2020 density model  
 
 8. Constructed a set of routines to connect GEODYN to Kamodo through the Orbit Cloud Method  
 
 9. Included the capability to physically calculate the drag coefficient using the modifified DRIA method.  This serves as an improvement to the BWDRAG routine.  
 
 10. Constructed a ``drag_file`` to be written in the ``DRAG.f90``  


Modifications to IIS:
====================================

1. One modification was made to IIS/ORIG.  We removed the ATGRAV printing to IIEOUT which was in the ``IIS/ALLOAD.f90`` routine.
    
    - When constructing the final ``iieout`` file, the printout from ``iisout`` is appended to the front of the ``iieout`` to allow for a complete view of the run output.  The ``iisout`` printout had a line of code (nested in the ``IIS/ORIG/ALLOAD.f90`` subroutine) that was likely meant for debugging, but had been left in the version of GEODYN handed down to us.  This line printed all of the gravitational coefficients from the ``ATGRAV`` file to ``iisout``, using up about ~60000 lines and ~80 MB of storage space per ``iieout`` file... I commented out these lines in ``ALLOAD.f90`` to remove the unnecessary prints and save space in the final ``iieout`` file.



