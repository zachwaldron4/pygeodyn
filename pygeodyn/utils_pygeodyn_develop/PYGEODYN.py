import sys
sys.path.insert(0, '/data/geodyn_proj/pygeodyn/utils_pygeodyn_develop/')
sys.path.insert(0,'/data/geodyn_proj/pygeodyn/utils_pygeodyn_develop/util_preprocessing/')



from PYGEODYN_Satellites import *
# from pre_processing import pygeodyn_PreProcessing


class Satellite(Satellite_ICESat2, Satellite_Starlette ):
    '''
    Define a sattellite class that will inherit all of the methods associated with the controllers and readers.
    '''
    def __init__(self,params):
        super().__init__(params)

        if self.satellite ==  'icesat2':
            self.Satellite = Satellite_ICESat2(params)
            super().__init__(params)
        elif self.satellite ==  'starlette':
            self.Satellite = Satellite_Starlette(params)
            super().__init__(params)

        
        

class Pygeodyn(Satellite):

    def __init__(self, params):  
        
        super().__init__(params)
        self.action       = params['action']
        self.tab = '  '

        print('=================================================================')
        print(self.tab,'Initializing PYGEODYN to ... ')
        
        if self.action   == 'read':
#             READ_switch = True
            print(self.tabtab*3   ,"      ... READ GEODYN output")
        elif self.action == 'run':
#             RUN_switch = True
            print(self.tabtab*3   ,"      ... RUN GEODYN")
        elif self.action == 'prep':
            print(self.tabtab*3   ,"      ... PRE-PROCESS GEODYN inputs")
#             PREP_switch = True
        else:
            print()
            print(self.tabtab,"ERROR! Please identify an 'action' parameter: ")
            print(self.tabtab,self.tabtab,"['prep', 'run', or 'read']")
            print()
            sys.exit()
        print('=================================================================')

        
#         self.Satellite = Satellite(self.action)
