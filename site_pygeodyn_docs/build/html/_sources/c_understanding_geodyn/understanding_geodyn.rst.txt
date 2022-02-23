
GEODYN Overview
===========================================


GEODYN Internal Programs
---------------------------------------

The following is summarized from the `SGP Website <https://space-geodesy.nasa.gov/techniques/tools/GEODYN/GEODYN.html>`_:

GEODYN is an orbit determination and geodetic parameter estimation program. Users of GEODYN input estimates of orbital parameters (such as the initial satellite state and solar radiation coefficients) and geodetic parameters (such as tracking station coordinates). GEODYN then computes orbits from the input parameters and can also compute theoretical values of satellite tracking observations using the input geodetic parameters. GEODYN can compare the theoretical values of tracking observations with real tracking observations to refine the input values of orbital and geodetic parameters.


GEODYN is a combination of three programs: TDF, GEODYN_IIS and GEODYN_IIE:

* TDF (Tracking Data Formatter) is a program converting GEODYN input data in various types of ASCII formats, to a Binary file Format (G2B). The GEODYN program GEODYN_IIS reads input data in the G2B format.

* GEODYN_IIS (Scheduling) is the program that reads input data, ancillary data files and the user's setup options. According to these options GEODYN_IIS prepares and outputs two files: the data file (Fort 11) and the Interface file (Fort 41). Descriptions are provided in volume 5 of the GII Documentation.

* GEODYN_IIE (Execution) reads the two files created by GEODYN_IIS and processes the data. Each measurement type is processed by the appropriate modeling routines and applies the models selected in GEODYN_IIS. GEODYN_II performs satellite orbit determination and creates computed observables. These are used along with the provided measured observations in a statistical least squares scheme. GEODYN_IIE then provides solutions of requested geophysical parameters or updated orbits. Large scale solutions may be performed by GEODYN by saving normal matrices and combining them later using an external program.

.. note:: The version of GEODYN II used by the CCMC on the AWS server is the full GEODYN II program (not the SGP Version).  This simply means that it has a few additional capabilities beyond orbit determination of earth orbiting satellites and geophysical parameter estimation.  The ``pygeodyn`` program has been constructed to simplify the efforts of running GEODYN and reading its output.

.. note:: The ``TDF`` program is not used in ``Pygeodyn`` (for now).  Instead we employ our own preprocessing codes.


The generalized flow of GEODYN (as it is relevant to our density work) can be summarized into the following flow chart:

.. image:: ../pdfs_slides_images/geodyn_flow.png



Important Files: Inputs and Outputs
-----------------------------------------

In order to construct a successful run, the user must collect the appropriate input files (satellite and tracking data type dependent) and point then to the correct Fortran units when running IIS.  Below is a description of the GEODYN input and output files that we have come across.

+-----------------------------------------------------+------------+--------------------------------------------------------+--------+-----------------------------+------+
|                 Input Type                          |Desc. Name  | Example File Name                                      | Format | Unit (``ftn#``, ``fort.#``) | Info |
+=====================================================+============+========================================================+========+=============================+======+
| Setup File (CARD deck)                              |``iisset``  | ``iisset.2018.287``                                    | ascii  |  5                          |      |
+-----------------------------------------------------+------------+--------------------------------------------------------+--------+-----------------------------+------+
| Tracking data                                       |``G2B``     | ``icesat2g2b_pce_312_328``                             | binary |  40                         |      |
+-----------------------------------------------------+------------+--------------------------------------------------------+--------+-----------------------------+------+
| Atmospheric gravity file                            |``atgrav``  |``ATGRAV.glo-3HR_20160101-PRESENT_9999_AOD1B_0006.0090``| binary |  18                         |      |
+-----------------------------------------------------+------------+--------------------------------------------------------+--------+-----------------------------+------+
| General gravity field file                          |``gravity`` | ``eigen-6c.gfc_20080101_do_200_fix.grv``               | binary |  12                         |      |
+-----------------------------------------------------+------------+--------------------------------------------------------+--------+-----------------------------+------+
| Planetary Ephemeris file                            |``iisset``  |  ``ephem1430.data_2025``                               | binary |  1                          |      |
+-----------------------------------------------------+------------+--------------------------------------------------------+--------+-----------------------------+------+
| Solar Flux, Ap, Kp, Polar motion, A1-UTC, etc.      |``gdntable``| ``gdntable.data``                                      | binary |  2                          |      |
+-----------------------------------------------------+------------+--------------------------------------------------------+--------+-----------------------------+------+
| External Attitude                                   |``EXAT``    | ``EXAT01.2018.287``                                    | binary |  EXAT01                     |      |
+-----------------------------------------------------+------------+--------------------------------------------------------+--------+-----------------------------+------+

.. note:: External attitude files are not necessary for cannonball runs (spherical satellites).

.. note:: The gdntable is a common file across runs and only needs to be updated to include the right times.


+-----------------------------------------------------+--------------------------+--------------------------------------+--------+-----------------------------+------+
|                 Output File / Products              |Desc. Name                | Controlling Card                     | Format | Unit (``ftn#``, ``fort.#``) | Info |
+=====================================================+==========================+======================================+========+=============================+======+
| IIEOUT                                              |``iieout``                | ``PRNTVU``                           | ascii  |       6                     |      |
+-----------------------------------------------------+--------------------------+--------------------------------------+--------+-----------------------------+------+
| IISOUT                                              |``iisout``                | ``PRNTVU``                           | ascii  |       6                     |      |
+-----------------------------------------------------+--------------------------+--------------------------------------+--------+-----------------------------+------+
| Trajectory File                                     |``orbfil``                | ``ORBFIL``                           | binary | user choose: 130-230        |      |
+-----------------------------------------------------+--------------------------+--------------------------------------+--------+-----------------------------+------+
| Trajectory Printouts                                |``XYZ_TRAJ``, ``KEP_TRAJ``| ``ORBTVU``                           | ascii  | 8 (xyz), 10 (kep)           |      |
+-----------------------------------------------------+--------------------------+--------------------------------------+--------+-----------------------------+------+
| Residual File                                       | ``resid``                | ``RESIDU``                           | binary |       19                    |      |
+-----------------------------------------------------+--------------------------+--------------------------------------+--------+-----------------------------+------+
| Printed Residuals (printed to ``iieout``)           | loc. in ``iieout``       | ``OBSVU``                            | ascii  |       6                     |      |
+-----------------------------------------------------+--------------------------+--------------------------------------+--------+-----------------------------+------+
| Adjusted Parameters (printed to ``iieout``)         | loc. in ``iieout``       | ``TERMVU``                           | ascii  |       6                     |      |
+-----------------------------------------------------+--------------------------+--------------------------------------+--------+-----------------------------+------+
| Density File (manually added)                       | ``densityfil``           | Manually added WRITE in ``DRAG.f90`` | ascii  |       99                    |      |
+-----------------------------------------------------+--------------------------+--------------------------------------+--------+-----------------------------+------+

Using the ``EPOCH`` and ``ORBFIL`` card, we can extend out the final date to some period later than the final time of selected data.  This makes our POD be some time period where out parameters are being adjusted using the traking data (time is selected with the ``SELECT`` card) and then our orbit continues to propagate using those iterated conditions (adjusted parameters) to the final time identified by ``EPOCH`` and ``ORBFIL``.  We then can compare these output products externally.


.. _ * Drag Assessment: Output Products
.. _ * ---------------------------------------------------------- !
.. _ * 1. RMS of Fit !
.. _ * **Location**: ``iieout`` file !
.. _ * **Header**  : ``RESIDUAL SUMMARY BY MEASUREMENT TYPE FOR ARC ## INNER ITERATION ## OF GLOBAL ITERATION 1``   !
.. _ * 2. Residuals (Tracking data - orbit determination ) !
.. _ * **Location**: ``iieout`` file !
.. _ * **Header**  : ``OBSERVATION RESIDUALS FOR ARC ## FOR INNER ITERATION ## OF GLOBAL ITERATION 1``   !
.. _ * 3. Adjusted Parameters  (Cd and state vector) !
.. _ * **Location**: ``iieout`` file !
.. _ * **Header**  : ``#ARC ## PARAMETER ADJUSTMENT SUMMARY FOR INNER ITERATION ## OF GLOBAL ITERATION 1``   !
.. _ * 4. Density (model ouput) !
.. _ * **Location**: Density file  !
.. _ * **Header**  : ``#ARC ## PARAMETER ADJUSTMENT SUMMARY FOR INNER ITERATION ## OF GLOBAL ITERATION 1``   !
.. _ * 5. Arc Overlaps !
.. _ * Must run multiple arcs and look at the above outputs during the periods of overlap to check for consistency !
.. _ * 6. Compare Trajectory (+ Orbit Predictions) !
.. _ * **Location**: ``ORBFIL`` !




