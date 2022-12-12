# Introduction to GEODYN

A GEODYN run is based on the user inputs that get inputted into the run as a CARD.  The full set of CARDS is referred to as the deck. The deck is located at ```RUNS>INPUTS>iisset_start``` within our file structure. Information on each of the CARDS is in the Volume 3 Documentation.

## AWS Directory Structure:

```
IIE  
    ORIG/
        (thousands of subroutines)
    MODS/
        (any modifications to the above subroutines)
        
IIS  
    ORIG/
        (thousands of subroutines)
    MODS/
        (any modifications to the above subroutines)
RUNS  
    (output and input files for GEODYN)
```




### IIS
Description (Listed from Vol 5 page 5)
 - GEODYN IIS program reads and interprets the option cards  
 - Reads the input observation data  
 - Read the optional gravity model, station geodetics, and area/mass files  
 - Extracts ephemeris data from necessary files and tables given the input time periods
 - Rearranges the data into vector form to minimize the amount of data manipulation in IIE
 - Output is put into 2 files  
     - (Fort 11) One contains the data from IIS, and the other, (Fort 41) contains all the information to run IIE (i.e. user selection, appropriate ephemeris, flux, polar motion and time data; the pointers and the sizes for the dynamic arrays; the defaults for all model parameters; and all control information needed to output the requested files.
        

### IIE
Description (Listed from Vol 5 page 5)  
 - IIE performs all the computations normally associated with satellite orbit and geodetic parameter estimation programs.  
 - IIE is written to run efficiently on vector processing computers without having to handle the I/O intensive parts that are performed by IIS.

#### Optional Outputs from IIE  

Built-in File (binary) Outputs:
- Trajectory File (``ORBFIL``)
- Residual File (``RESIDU``)
- Partial Derivative File
- Normal Equation File
- Force Model Partial Derivative File


[comment]: <> (## RUNS )
[comment]: <> (The RUNS directory contains all of the relevant information for running GEODYN.  The main script that generates a GEODYN run is `zzz`.)
[comment]: <> (This directory contains all of the data inputs and outputs from GEODYN.)





