


Project Overview
###########################################

We have completed the goal of this research project, which has been to implement and test the Community Coordinated Modeling Center's (CCMC) Kamodo python package and NASA's GEODYN software on an Amazon Web Services (AWS)-hosted CCMC collaborative environment. We have constructed a novel method for integrating Kamodo and its associated thermospheric models into GEODYN, opening the door for using satellite drag and precise orbit determination to perform model validation on CCMC hosted models.  Documentation has been created to simplify the user process for running and modifying GEODYN and analysis codes have been developed for user-friendly reading of the GEODYN output. 


Pygeodyn 
====================
PYGEODYN is a intended to be an easy-to-use Python-based wrapper for the GEODYN precise orbit determination software.
Pygeodyn is not yet available for public use or for use outside of its current server placement.

Some of pygeodyn's primary cababilities are:
 - Running the GEODYN source code (written in FORTRAN)
 - Organizing and using different compiled versions of GEODYN
 - Editing the setup files at the time of run
 - Controlling file inputs and outputs
 - Controlling modifications to the GEODYN source code (without need to recompile between variations)
 - Interpreting the GEODYN outputs into easy-to-use formats (i.e. CSVs, pandas DataFrames, etc.)
 - Reading Fortran unformatted binary files
 - Making plots to analyze GEODYN outputs
 - Connects GEODYN to Kamodo to provide the POD access to physics-based modeling output 
 
PYGEODYN is written as a collection of Python objects, with the highest level being the ``Pygeodyn`` Object.  From here, ``Pygeodyn`` inherits a Satellite class which in-turn inherits the Run and Read classes of functions.  For examples on how to use PYGEODYN, refer to the example notebooks.


GEODYN
===========================
GEODYN is an orbit determination and geodetic parameter estimation program.
GEODYN is a set of software tools used for orbit determination and geodetic parameter estimation. Users of GEODYN input estimates of orbital parameters (such as the initial satellite state and solar radiation coefficients) and geodetic parameters (such as tracking station coordinates). GEODYN then computes orbits from the input parameters and can also compute theoretical values of satellite tracking observations using the input geodetic parameters. GEODYN can compare the theoretical values of tracking observations with real tracking observations to refine the input values of orbital and geodetic parameters.

For more information please refer to: [https://earth.gsfc.nasa.gov/geo/data/geodyn-documentation](https://earth.gsfc.nasa.gov/geo/data/geodyn-documentation).


Kamodo
===========================
Kamodo is a new CCMC tool for access, interpolation, and visualization of space weather models and data in python. Kamodo allows model developers to represent simulation results as mathematical functions which may be manipulated directly by end users. Kamodo handles unit conversion transparently and supports interactive science discovery through jupyter notebooks with minimal coding and is accessible through python.

Official site page [https://ccmc.gsfc.nasa.gov/Kamodo/](https://ccmc.gsfc.nasa.gov/Kamodo/). 



.. Goals
.. ###########################################
.. 
.. The CCMC’s Kamodo python package and NASA's GEODYN software have been implemented on
.. an Amazon Web Services-hosted CCMC collaborative environment.  In the interest of fully integrating Kamodo and associated ITM models into GEODYN, some key issues must be resolved:
.. 
.. 1. The output of the GEODYN software is not user-friendly for modern programming languages (e.g., Python), and requires extensive modification and manipulation to read from its unformatted .. Fortran binary files. This removes GEODYN as a handy option for quick analysis both from the perspective of satellite operators and model users.
.. 
..     - Implementation of a GEODYN output reader that has been catered to the needs
..     of Python users will make GEODYN a more accessible option for CCMC users.
..     Furthermore, providing portability from Fortran to Kamodo will extend the
..     GEODYN output to the multitude of visualization capabilities within Kamodo’s
..     environment. (January 2021 - February 2021)
.. 
.. 2. Visualizations of GEODYN are not available to new users. What currently exists are either written in Fortran, or have been created by independent users— neither are widely available.
..     1.  Constructing a series of basic visualizations of the GEODYN output will aid users in quick validation and assessment of runs. Further catering these visualizations to atmospheric model .. users will be useful to the CCMC and its partners. These visualizations can be made available in the Kamodo environment to add to the portability of GEODYN. _(February 2021 - March 2021)_  
..     
.. 3. Current capabilities within GEODYN have been optimized for geodetic research, but lack some functionality for model validation.
..     - No easily readable method exists within the GEODYN output to determine the model density along the orbit of the satellite. Implementation of such a feature will prove to be helpful for .. model validation. (March 2021)
..    
.. 4. The current default thermospheric implementation in GEODYN is the Jacchia 1971
.. model, with options existing to use the Thermosphere Drag Model (DTM), or the
.. MSIS-86 Empirical Drag Model. 
..    -  Implementation of the MSISe00 and MSIS 2.0 empirical models into GEODYN will
.. offer internal improvements to the GEODYN models by leveraging the more
.. modern upgrades made to the MSIS series of empirical-models. (April 2021)
.. 
.. 5. The method for “switching out” satellites for quick orbital determination is not
.. user-friendly.
..     -  Providing CCMC specific documentation, and an easy method for quickly making
.. modifications, will save CCMC users time and make GEODYN a more accessible
.. option for model validation from the perspective of different satellites. (May
.. 2021)
.. 
.. 6. GEODYN does not have access to the more complicated I-T models that are currently
.. hosted by the CCMC or by Kamodo.
..     -  Leveraging the collaboration with the CU Boulder SWxTREC, we aim to work with
.. the Missions, Applications, and Data Technology (MADTech) team to implement
.. a Kamodo-GEODYN Interface. Such an interface will incorporate Kamodo, and its
.. modeling capabilities, into GEODYN. A fortran wrapper will need to be written
.. for Kamodo to call atmospheric model output to be used in the orbital
.. determination integration. Such a connection will allow scientists to take
.. advantage of the interpolation and visualization of space weather model tools
.. granted by Kamodo as well as the orbital dynamics capabilities offered by
.. GEODYN. (September 2021 - October 2021)
..        
.. 7. Vehicle implementation within GEODYN
..     -  The vehicle implementation of satellites within GEODYN has been used for
.. geodetic research, but may serve as a source of error in the form of overfitting
.. the coefficient of drag, Cd. Fully documenting this process and potentially
.. improving the implementation of vehicle models could bring GEODYN up to
.. speed with current satellite modeling capabilities and offer users understanding
.. for how this factor impacts their atmospheric modeling results. (December
.. 2021)
       