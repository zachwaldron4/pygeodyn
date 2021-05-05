import sys
sys.path.insert(0, '/data/geodyn_proj/pygeodyn/utils_pygeodyn_develop/')



from satellite_classes import *



class pygeodyn(Satellite_ICESat2, Satellite_Starlette):

    def __init__(self, params):  
        
        super().__init__(params)
        
    
    
    