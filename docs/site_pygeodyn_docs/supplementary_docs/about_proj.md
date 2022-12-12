
# Project Overview


## About the Project

The goal of this research project is to implement and test the CCMC’s Kamodo python package and NASA's GEODYN on an Amazon Web Services-hosted CCMC collaborative environment with the long-term goal of fully integrating Kamodo and associated thermospheric models into GEODYN. We will test ionospheric model interpolation in Kamodo and assist with installation and testing of GEODYN in collaboration with the developers of the GEODYN software. We will also provide CCMC-specific documentation.  The expected significance of this proposed research is to offer an easy-to-use, science-oriented orbital dynamics package for Ionosphere-Thermosphere-Mesosphere (ITM) researchers to use for satellite trajectory tracking and atmospheric drag research.  The Kamodo package further allows streamlined atmospheric model data processing for collaborative ITM research. 



## Kamodo

Kamodo is a new CCMC tool for access, interpolation, and visualization of space weather models and data in python. Kamodo allows model developers to represent simulation results as mathematical functions which may be manipulated directly by end users. Kamodo handles unit conversion transparently and supports interactive science discovery through jupyter notebooks with minimal coding and is accessible through python.

Official site page [https://ccmc.gsfc.nasa.gov/Kamodo/](https://ccmc.gsfc.nasa.gov/Kamodo/). 



## GEODYN

GEODYN is an orbit determination and geodetic parameter estimation program.
GEODYN is a set of software tools used for orbit determination and geodetic parameter estimation. Users of GEODYN input estimates of orbital parameters (such as the initial satellite state and solar radiation coefficients) and geodetic parameters (such as tracking station coordinates). GEODYN then computes orbits from the input parameters and can also compute theoretical values of satellite tracking observations using the input geodetic parameters. GEODYN can compare the theoretical values of tracking observations with real tracking observations to refine the input values of orbital and geodetic parameters.

For more information please refer to: [https://earth.gsfc.nasa.gov/geo/data/geodyn-documentation](https://earth.gsfc.nasa.gov/geo/data/geodyn-documentation).


## Goals
The CCMC’s Kamodo python package and NASA's GEODYN software have been implemented on
an Amazon Web Services-hosted CCMC collaborative environment.  In the interest of fully integrating Kamodo and associated ITM models into GEODYN, some key issues must be resolved:

1. The output of the GEODYN software is not user-friendly for modern programming
languages (e.g., Python), and requires extensive modification and manipulation to read
from its unformatted Fortran binary files. This removes GEODYN as a handy option for
quick analysis both from the perspective of satellite operators and model users.
    - Implementation of a GEODYN output reader that has been catered to the needs
    of Python users will make GEODYN a more accessible option for CCMC users.
    Furthermore, providing portability from Fortran to Kamodo will extend the
    GEODYN output to the multitude of visualization capabilities within Kamodo’s
    environment. _(January 2021 - February 2021)_
2. Visualizations of GEODYN are not available to new users. What currently exists are
either written in Fortran, or have been created by independent users— neither are
widely available.
    - Constructing a series of basic visualizations of the GEODYN output will aid users in quick validation and assessment of runs. Further catering these visualizations to atmospheric model users will be useful to the CCMC and its partners. These visualizations can be made available in the Kamodo environment to add to the portability of GEODYN. _(February 2021 - March 2021)_  
    
3. Current capabilities within GEODYN have been optimized for geodetic research, but lack some functionality for model validation.
    - No easily readable method exists within the GEODYN output to determine the model density along the orbit of the satellite. Implementation of such a feature will prove to be helpful for model validation. _(March 2021)_
   
4. The current default thermospheric implementation in GEODYN is the Jacchia 1971
model, with options existing to use the Thermosphere Drag Model (DTM), or the
MSIS-86 Empirical Drag Model. 
   -  Implementation of the MSISe00 and MSIS 2.0 empirical models into GEODYN will
offer internal improvements to the GEODYN models by leveraging the more
modern upgrades made to the MSIS series of empirical-models. _(April 2021)_

5. The method for “switching out” satellites for quick orbital determination is not
user-friendly.
    -  Providing CCMC specific documentation, and an easy method for quickly making
modifications, will save CCMC users time and make GEODYN a more accessible
option for model validation from the perspective of different satellites. _(May
2021)_

6. GEODYN does not have access to the more complicated I-T models that are currently
hosted by the CCMC or by Kamodo.
    -  Leveraging the collaboration with the CU Boulder SWxTREC, we aim to work with
the Missions, Applications, and Data Technology (MADTech) team to implement
a Kamodo-GEODYN Interface. Such an interface will incorporate Kamodo, and its
modeling capabilities, into GEODYN. A fortran wrapper will need to be written
for Kamodo to call atmospheric model output to be used in the orbital
determination integration. Such a connection will allow scientists to take
advantage of the interpolation and visualization of space weather model tools
granted by Kamodo as well as the orbital dynamics capabilities offered by
GEODYN. _(September 2021 - October 2021)_
       
7. Vehicle implementation within GEODYN
    -  The vehicle implementation of satellites within GEODYN has been used for
geodetic research, but may serve as a source of error in the form of overfitting
the coefficient of drag, Cd. Fully documenting this process and potentially
improving the implementation of vehicle models could bring GEODYN up to
speed with current satellite modeling capabilities and offer users understanding
for how this factor impacts their atmospheric modeling results. _(December
2021)_
       