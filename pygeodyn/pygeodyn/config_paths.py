"""
_summary_

_extended_summary_
"""

from pathlib import Path  # pathlib is seriously awesome!

# Parent Project Directory
path_project  = '/data/SatDragModelValidation'

# Pygeodyn package/modules
path_pygeodyn = Path(path_project+'/pygeodyn/pygeodyn')

# Data directories
path_data_inputs        = Path(path_project+'/data/inputs')
path_data_outputs_raw   = Path(path_project+'/data/outputs_raw')
path_data_outputs_clean = Path(path_project+'/data/outputs_clean')

# Temporary Run Directory
path_tmp = Path(path_project+'/data/tmp')
