##############################
Introduction to ``PYGEODYN``
##############################


PYGEODYN is a intended to be an easy(er) to use Python-based wrapper for the GEODYN precise orbit determination software.


PYGEODY Capabilities
------------------------------



PYGEODYN offers easy access to:
- Running GEODYN
- Editing the setup files
- Controlling different satellites and tracking data-types
- Controlling file inputs and outputs
- Controlling modifications to the GEODYN source code (without need to recompile between variations)
- Organizing different GEODYN runs
- Interpreting the GEODYN outputs into easy-to-use formats (i.e. CSVs, pandas DataFrames, etc.)
- Reading Fortran unformatted binary files
- Making plots to analyze GEODYN outputs


PYGEODYN is written as a collection of inherited Python objects, with the highest level being the ``Pygeodyn`` Object.  From here, ``Pygeodyn`` inherits a Satellite class which in-turn inherits the Run and Read classes of functions.


For instructions on how to use PYGEODYN, refer to the example notebooks.









