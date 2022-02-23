# Getting Started:


## Analysis:

The analysis notebooks are located at the follow path on the bastian server:
```
/data/analysis/
```

The ``/data/analysis/starlette_analysis`` directory contains the notebooks that analyze the GEODYN output for the Starlette-SLR data.


## Data Output from GEODYN:
With the addition of the Starlette-SLR run, the running configuration and data output location has been updated as follows: 

- Directory containing all Starllete scripts, input, and output:``/data/runs_geodyn/``

**File Structure and Contents in ``st/``**:
- The entire GEODYN run (data output and directory architecture) is controlled in the scripts.
```
 atgrav       - atmospheric gravity files
 ephem        - geodyn specific ephemeris
 examples     - sample files to use as examples
 g2b          - GEODYN G2B (observation file)
 gravity      - gravity field file (grvfld.goco05s)
 results      - GEODYN Ouput
 scripts      - scripts for running GEODYN
 setups       - input files for GOEDYN run (2week arcs)
```

### Running GEODYN from the script:

- Main geodyn script: ``geodyn_st``
- Script for running all density options: ``run_all_dens``

TODO: add the script contents and functions here.

















