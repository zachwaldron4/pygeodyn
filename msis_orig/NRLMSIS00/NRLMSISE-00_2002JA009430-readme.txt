Electronic Data Submission for Paper 2002JA009430

NRLMSISE-00 Empirical Model of the Atmosphere: Statistical Comparisons and Scientific Issues

J. M. Picone, A. E. Hedin, and D. P. Drob
E. O. Hulburt Center for Space Research
Naval Research Laboratory
Washington, DC 20375

A. C. Aikin
Laboratory for Extraterrestrial Physics 
Planetary Magnetospheres Branch, Code 695
Goddard Space Flight Center 
Greenbelt, MD 20771

Journal of Geophysical Research, Space Physics, 2002 

Tables of Empirical Model Comparisons with the NRLMSIS Database


DESCRIPTION OF DATAVSMODELS.TXT

	The accompanying text file, datavsmodels.txt, contains a set of tables which compare all thermospheric data sets in the NRLMSIS database to the corresponding values of several empirical models of atmospheric temperature, composition, and total mass density: NRLMSISE-00 (denoted N00), MSISE-90 (M90), and Jacchia-70 (J70).  These data sets derive from satellite missions, rocket flights, and ground-based systems, and the observations extend unevenly over the period 1963-1997. The accompanying paper, "NRLMSISE-00 Empirical Model of the Atmosphere: Statistical Comparisons and Scientific Issues," and references therein, provide background on the data sets. 

	For each variable, three tables provide values of the mean residual (denoted "MEAN") and standard deviation ("SD") of the data relative to the corresponding model values. The three tables correspond to different levels of geomagnetic activity (quiet, high, and all levels) and altitude (km).  The tables include both the model generation database and the complete, newly added data sets (designated by footnotes). In the following definitions and equations, we use these symbols: exponentiation (double asterisk) **, subscript (underbar)"_", average over data (bracket) < >, and Greek letters, written as beta, sigma, and rho. For temperature T and data index i, the tables show a mean residual, defined to be 

"MEAN" = beta_T = <T_i(data) - T_i (model)>    , 

and a standard deviation 

"SD = "sigma_T = (<[T_i(data) - T_i(model)]**2> - beta_T**2)**(1/2)    , 

where both quantities are in units of Kelvin. For species number density [x] and total mass density rho, we have used log [x_i] and log rho_i as our respective primary statistical variables. We then express the mean residual for the mass density as an average logarithm of the density ratio (data to model), i.e.,  

<log_e{rho_i(data)} - log_e{rho_i(model)}> = <log_e{rho_i(data)/rho_i(model)}>, 

as a representative fractional density difference 

"MEAN" = beta_rho = exp<log_e{rho_i(data)/rho_i(model)}> -1 = 
                  = [<rho_i(data)> - <rho_i(model)>]/<rho_i(model)>,

where the brackets in the second line of the equation represent geometric averages.

In terms of beta_rho, the standard deviation of the log(density ratio) is 

"SD" = sigma_rho = [<log_e**2{rho_i(data)/rho_i (model)}> - log_e**2(beta_rho+1)]**(1/2).

The equations for beta_x and sigma_x have the same respective forms.

	As indicated above, the tables correspond to thermospheric variables and levels of geomagnetic activity (three tables per variable). Each table lists results separately for the data sets which address the particular variable and level of geomagnetic activity. Accompanying each data set name is a descriptor to indicate the source of the data; abbreviations include "accel" (accelerometer), "drag" (drag from orbit determination), "NMS" (neutral mass spectrometer), "IMS" (ion mass spectrometer), "ISR" (incoherent scatter radar), and "UV occ" (solar ultraviolet occultation/absorption).

	We have chosen to place all of the tables in a single text file to permit differential labeling by captions and thereby, to facilitate simple searches. In this way we hope to reduce the chance of error in accessing the comparisons of interest.


