#########################################
Understanding the Setup file/Card deck
#########################################


When making a GEODYN run the CARD deck is what controls GEODYN's many options.  The card deck contained in ``iisset``.

**The options that one will include in a GEODYN run is dependent on several factors:**

1. The Satellite 
    * orbital elements
    * satellite shape (external attitude needed for non cannon-ball)
    
    
    
2. The tracking data type
    * gets very complicated and many different options have to be switched to correct state depending on your datatype.


3. The desired outcome/output
    * what files are needed as input?
    * what files are wanted as output?
    * is file size a concern?
    
4. Type of analysis being done
    * what parameters do you want adjusted vs modelled?
    * do you want time depended drag?
    * do you want a prediction time period?
    * should the empirically adjusted accelerations be on or off?


.. note:: The list of setup options has many names:

    * card deck
    * setup deck
    * options file
    * setup file
    * iisset
    * Unit/fort/ftn 5
    
**Setup Deck Structure:**
===========================

.. code-block:: 

    Global Title line 1        |   These are just three lines of strings that
    Global Title line 2        |   make up the first 3 lines of iisset.
    Global Title line 3        |   If missing IIS will give a cryptic error.
    Global Card 1...
    Global Card 2...
    Global Card 3...
    etc.
    .
    .
    .
    ENDGLB
    Arc title line 1           |   Same as above, except designates the ARC Section Title
    Arc title line 2           |    
    Arc title line 3           |   If missing IIS will give a cryptic error.
    REFSYS                   
    SATPAR                     ---
    EPOCH                      |   These four lines are repeated for 
    ELEMS1                     |   each satellite in the arc
    ELEMS2                     ---
    etc.
    .
    .
    .
    DATA
    SELECT
    etc.
    .
    .
    .
    DELETE
    DELETE
    DELETE
    etc.
    .
    .
    .
    ENDARC
    
    
    


Example: ICESat2 with PCE Data-- Trajectory Analysis
========================================================

This section represents a sample of the choices that were made for the iisset deck when constructing the Trajectory Analysis of the ICESat2 satellite tracked by PCE data.


**Initial setup deck:**
------------------------

We made our Trajectory Analysis runs by editing some provided setup files.  These initial files were from a reduced dynamics run of the ICESat2 Satellites orbit.  These decks are actually the ones there were used to CREATE our PCE data.


`For reference, here is an example setup file after it has been modified! <../_static/cleaned_setup_2018313>`_. 


**Modified Cards:**
--------------------

.. note:: The choices that were made here may be very different from other run types featuring different satellites that may be tracked by different datatypes.

.. note:: What follows here is a snippet of the choices we made a why.  Volume 3 of GEODYN should be referred to for all Card options and choices.


ORBFIL:
^^^^^^^^^
    **Example**: ``ORBFIL20131      1807001     181108210000.0  181111 24200.00``
    
    **Notes:**  
     - We include the orbit file so that we can see the Trajectory output of ICESat2.  
     - The final end time on this card is extended to allow for a prediction of the satellite past the time during which the parameters are adjusted.
    
    **Columns Info:**  
    
.. code-block:: python

            #####  ORBFIL KEY ------ Requests output of trajectory file(s) on specified unit(s) 
            #####                           on the last iteration of the run.
            #####
            #####   columns      Orbit output option
            #####    7           Coordinate system of output
            #####                      0 - True of date (default)
            #####                      1 - True of reference date 
            #####                   ** 2 - Mean of year 2000    
            #####    8           Switch indicating whether trajectory file is for a single 
            #####                  satellite or a set of satellites.
            #####                   ** 0 - Single satellite 0 0
            #####                      1 - Set of satellites. This option has meaning 
            #####                            only when used in conjunction with sets of 
            #####                            satellites (See EPOCH and SLAVE option cards
            #####                            for more details ). If satellite ID in columns
            #####                            18-24 is a master satellite , then the trajectory
            #####                          for all satellites in the set will be output.
            #####  9-11           Mandatory unit number for trajectory file. All trajectory 
            #####                  files within an arc must have unique unit numbers. 
            #####                  The suggested unit number starts at 130.
            #####  18-25        Satellite ID. This field must contain a valid ID.
            #####  25-44        START date and time for trajectory output (YYMMDDHHMMSS.SS).
            #####  45-59        STOP  date and time for trajectory output (YYMMDDHHMMSS.SS).
            #####  60-72        Time interval between successive trajectory outputs.




......




OBSVU:
^^^^^^^^^
    **Example**: ``OBSVU 3``
    
    **Notes:**  
     - Requests a printout of the observation residuals to ``iieout``.
     - A 3 in column 7 indicates that residuals are requested on the first inner iteration of the first global iteration and the last inner iteration of the last global iteration for all arcs.
     - The reference system is automatically made T.O.R. (True of Reference)


......




PRNTVU:
^^^^^^^^^
    **Example**: ``PRNTVU5521111211 121122``
    
    **Notes:**  
     
.. code-block:: python

            ####   PRNTVU KEY ------ Used to control the IIS and IIE printed content 
            ####                             Be warned that if you include a bunch, the file 
            ####                             will be huge, and the run time will increase 
            ####                             (printing to ascii is slow)
            ####    columns      IIS output option
            ####      9          Simple list of GEODYN -IIS setup. Interpretive
            ####     10          Interpretive list of GEODYN -IIS setup.
            ####     11          Observation block selection report.
            ####     12          Gravity model coefficients. Global  
            ####     13          Global parameter values and sigmas.                             
            ####     14          Arc parameter values and sigmas.
            ####     15          Sea surface topography. Ocean
            ####     16          Ocean Tide Model.  
            ####    columns      IIE output option
            ####     18          Simple list of GEODYN -IIS setup.
            ####     19          Values of estimated E-biases.
            ####     20          E-matrix labels in Summary Page.
            ####     21          Adjusted station baselines  
            ####     22          Correlations for adjusted parameters.                             
            ####     23          Shadow crossing. 
            
            
......





ATMDEN:
^^^^^^^^^

    **Notes:**  
     - Use this card to select which density model will be used in the run





.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. |    Card               | Example  (2018, Day 313)                                                             |                      |
.. +=======================+======================================================================================+======================+
.. | ``ORBFIL``            |  ``ORBFIL20131      1807001     181108210000.0  181111 24200.00          60``        |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``RESID``             |  removed                                                                             |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``OBSVU``             |  ``OBSVU 3``                                                                         |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``PRNTVU``            |  ``PRNTVU5521111211 121122``                                                         |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``ORBTVU``            |  removed                                                                             |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``ATMDEN``            |  ``ATMDEN  87``                                                                      |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``ATGRAV``            |  ``ATGRAV9090              181106210000.0000000181112030000.00``                     |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``I64G2E``            |  ``I64G2E         25``                                                               |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``SIGMA           1`` |  ``SIGMA           1               1.0                 1.0``                         |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``SIGMA           2`` |  ``SIGMA           2               1.0                 1.0``                         |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``SIGMA           3`` |  ``SIGMA           3               1.0                 1.0``                         |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``SIGMA          51`` |  ``SIGMA          51               10.0D+25             0.10``                       |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``SIGMA          85`` |  ``SIGMA          85               0.010000            0.010000``                    |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``REFSYS``            |  ``REFSYS193310        181108210000.00000000``                                       |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``EPOCH``             |  ``SATPAR   139     1807001          9.53000000       1514.000``                     |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``SATPAR``            |  ``EPOCH               181108210000.0000000181108210000.00000001811110300 00.000``   |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``ELEMS1``            |  ``ELEMS11             -746390.8775008094  -4870283.209209808  4764488.095739435``   |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``ELEMS2``            |  ``ELEMS2              456.5238693659707   5296.367627660121   5469.462164141512``   |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``FLUX    1``         |   ``FLUX    1                   1361.00``                                            |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``FLUX  0``           |   ``FLUX  0``                                                                        |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``CONDRG``            | ``CONDRG  1        1807001     181108210000.001811110300 0         0.50000  28800.`` |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+
.. | ``DRAG``              | ``DRAG             1807001 2.3000000000000E+00``                                     |                      |
.. +-----------------------+--------------------------------------------------------------------------------------+----------------------+


......

CARDNAME:
^^^^^^^^^^^
    **Example**: ``CARDNAME####      ######    #########      ######    #####``
    
    **Notes:**  
     - Note 1  
     - Note 2


.. **Removed Cards:**
.. 
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | Card       |    Reason for Removal                                       |                      |                             |              |
.. +=======+==================================================================+======================+=============================+==============+
.. | ACCEL9           |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | XEPHEM           |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | REFRAC           |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | GPSMOD           |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | OFFSET           |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | OFFADJ           |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | ANTPHC           |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | ANTPH2           |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | CGMASS           |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | OLOAD           |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | MBIAS            |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | SATPAR            |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | EPOCH            |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | ELEMS1            |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | ELEMS2            |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | ORBTVU            |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. | RESID            |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+
.. |             |                                                             |                      |                             |              |
.. +------------+-------------------------------------------------------------+----------------------+-----------------------------+--------------+




     