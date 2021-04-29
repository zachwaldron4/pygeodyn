# Navigating AWS

***Under Construction!***

## Directory Structure of the GEODYN Project

 The project files are located in the `/data/geodyn_proj` directory.  The sections of the project are then separated into the following directories (superfluous directories have been omitted):  

```text
(base) [shared_user@tools_dev geodyn_proj]$ tree -L 2
.
├── analysis               ---- contains all notebooks for viewing and evaluating the data)
│   ├── geodynBinary_to_Python
│   ├── ISS_GPS_analysis
│   ├── starlette_analysis
│   └── util_funcs
|
├── geodyn_code            ---- contains the source code for the GEODYN software
│   ├── IIE
│   ├── IIS
|
├── msis_orig              ---- original MSIS code
│   ├── NRLMSIS00
│   ├── NRLMSIS2.0
│   ├── NRLMSIS_F90
│   └── pymsis
|
└── runs_geodyn            ---- contains all geodyn run info
    ├── extra_dirs
    ├── iss
    ├── orbfil_converter
    ├── results
    ├── st
    └── tmp
```

## Directories and Files of Importance

### GEODYN output readers:
- Location: ` /data/geodyn_proj/analysis/util_funcs/`

```text
├── py_read_geodyn_output
│   ├── a_ReadGEODYN.py     --- script used to read in the data and the GEODYN output readers
│   ├── b_ReadGEODYN.py     --- multiple functions that are used to read the GEODYN output
├── util_common
│   ├── datetime_column.py
│   └── read_density_file.py
```

### MSIS Evaluation Notebooks:
- Location: ` /data/geodyn_proj/analysis/starlette_analysis/msis_evaluation/`

```text
├── ap.csv
├── compare_msis_models_multiple_arcs.ipynb
├── compare_msis_models_multiplearcs_multiplemodels.ipynb   --- Views the MSIS evaluation for each model version across all available arcs
├── kp.csv
├── noaa_radio_flux.csv
```


### GEODYN Source Code:
- Location: `/data/geodyn_proj/geodyn_code`

```
├── IIE
│   ├── MODS
│   ├── MODS_msis00_f90
│   ├── MODS_msis2
│   ├── ORIG
├── IIS
│   ├── MODS
│   └── ORIG
```




### GEODYN Starlette Runs:

#### Starlette Run Directory:
- Loc: `/data/geodyn_proj/runs_geodyn/st`

```text
├── atgrav
├── ephem
├── examples
├── g2b
├── gravity
├── PaxHeader
├── scripts
    ├── geodyn_st
    ├── msis2_geodyn_st
    ├── orig_geodyn_zach
    ├── output_text.txt
    ├── run_all_arcs_single_den
    └── run_all_dens
└── setups
    ├── dtm87_acceloff
    ├── dtm87_accelon
    ├── jaachia71_acceloff
    ├── jaachia71_accelon
    ├── msis_acceloff
        ├── edit_setup_scripts.ipynb
        ├── st030914_2wk.bz2
        ├── st030928_2wk.bz2
        ├── st031012_2wk.bz2
        ├── st031026_2wk.bz2
        ├── st031109_2wk.bz2
        ├── st031123_2wk.bz2
        ├── st031207_2wk.bz2
        └── st031221_2wk.bz2
    └── msis_accelon
```


#### Starlette Run Output:

- Loc: `/data/geodyn_proj/runs_geodyn/results/st`

    - ```text
        ├── dtm87
        ├── goco05s
        ├── goco05s_dtm87
        ├── goco05s_dtm87_accelon
        ├── goco05s_jaachia71
        ├── goco05s_jaachia71_accelon
        ├── goco05s_msis00_accelon
        ├── goco05s_msis2_accelon
        ├── goco05s_msis86
        ├── goco05s_msis86_acceloff
        ├── goco05s_msis86_accelon
        ├── jaachia71
        ├── msis00
        ├── msis2
        └── msis86

Example of one of the outputs (MSIS 2, no empirical accelerations):
- Loc: ``/data/geodyn_proj/runs_geodyn/results/st/msis2/msis2_acceloff/``
- Each of the below directories have their respective files saved under the arcs listed in the same way as IIEOUT. 
    ```text
    ├── all_outputs
    ├── DENSITY
    ├── EMAT
    ├── IIEOUT
        ├── st030914_2wk.goco05s
        ├── st030928_2wk.goco05s
        ├── st031012_2wk.goco05s
        ├── st031026_2wk.goco05s
        ├── st031109_2wk.goco05s
        ├── st031123_2wk.goco05s
        ├── st031207_2wk.goco05s
        └── st031221_2wk.goco05s
    ├── IISSET
    ├── KEP_TRAJ
    ├── orbits
    ├── PUNCH
    ├── RESIDS
    ├── sumry
    ├── TELEM
    └── XYZ_TRAJ

























