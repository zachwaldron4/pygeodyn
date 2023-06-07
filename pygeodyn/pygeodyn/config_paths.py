"""
_summary_

_extended_summary_
"""

from pathlib import Path  # pathlib is seriously awesome!

### All paths must be written without the final forward slash "/"
###  
### Parent Project Directory
path_project  = '/data/SatDragModelValidation'

# Pygeodyn package/modules
path_pygeodyn   = Path(path_project+'/pygeodyn/pygeodyn')
path_io_geodyn  = Path(path_project+'/pygeodyn/io_geodyn')
path_GEODYN_src = Path(path_project+'/src/geodyn_code')
path_kamodo_src = Path(path_project+'/src/kamodo_interface')
#
path_IIS_exec =  Path(str(path_GEODYN_src) +'/IIS/ORIG')
path_IIE_exec =  Path(str(path_GEODYN_src) +'/IIE/CD_model_proj')

# Data directories
path_data_inputs        = Path(path_project+'/data/inputs')
path_data_rawinputs     = Path(path_project+'/data/inputs/raw_inputdata')
path_data_outputs_raw   = Path(path_project+'/data/outputs_raw')
path_data_outputs_clean = Path(path_project+'/data/outputs_clean')

# Temporary Run Directory
path_tmp = Path(path_project+'/data/tmp')

# Utility Directories
path_util    = Path(path_project+'/pygeodyn/pygeodyn/util_dir')
path_utilpce = Path(path_project+'/pygeodyn/pygeodyn/util_dir/pce')
