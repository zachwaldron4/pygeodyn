# Pygeodyn Documentation

PYGEODYN is a Python-based wrapper for the GEODYN precise orbit determination software.

Pygeodyn is not yet available for public use or for use outside of its current server placement.

## PYGEODYN Capabilities

PYGEODYN offers easy access to:
 - Running the GEODYN source code (written in FORTRAN)
 - Organizing and using different compiled versions of GEODYN
 - Editing the setup files at the time of run
 - Controlling file inputs and outputs
 - Controlling modifications to the GEODYN source code (without need to recompile between variations)
 - Interpreting the GEODYN outputs into easy-to-use formats (i.e. CSVs, pandas DataFrames, etc.)
 - Reading Fortran unformatted binary files
 - Making plots to analyze GEODYN outputs

PYGEODYN is written as a collection of Python objects, with the highest level being the ``Pygeodyn`` Object.  From here, ``Pygeodyn`` inherits a Satellite class which in-turn inherits the Run and Read classes of functions.

For examples on how to use PYGEODYN, refer to the example notebooks.