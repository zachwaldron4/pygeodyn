import sys
sys.path.insert(0, '/data/geodyn_proj/pygeodyn/pygeodyn_develop/')
sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_preprocessing/')


from PYGEODYN_Starlette import Satellite_Starlette
from PYGEODYN_ICESat2   import  Satellite_ICESat2

from util_classtools import Util_Tools




# class Inherit_Satellite(Satellite_ICESat2 if sat =='icesat2' else Satellite_Starlette):
#     def __init__(self, sat):
#         pass





class Inherit_Icesat2(Satellite_ICESat2):
    def __init__(self):
#         print('inherit Satellite_ICESat2 ')
        Satellite_ICESat2.__init__(self)

        pass
        

class Inherit_Starlette(Satellite_Starlette):
    def __init__(self):
#         print('inherit Satellite_Starlette ')

        Satellite_Starlette.__init__(self)

        pass

     
    
    
    
    
    

# class Pygeodyn(Util_Tools, Inherit_Icesat2, Inherit_Starlette): #Satellite_Starlette,Satellite_ICESat2):


class Pygeodyn(Util_Tools, Inherit_Icesat2): 
    def __init__(self, params):  
        
#         print('1a ---- check -- init Pygeodyn class')

        self.satellite         = params['satellite']
        self.den_model         = params['den_model']
        self.SpecialRun_name   = params['SpecialRun_name']
        self.verbose           = params['verbose']
        self.arc_input         = params['arc']
        self.set_density_model_setup_params( self.den_model )
        if "accels" in params.keys():
            if params["accels"] == True:
                self.empirical_accels =  True  
                self.ACCELS = 'accelon'
            else:
                self.empirical_accels =  False  
                self.ACCELS = 'acceloff'
        else:
            self.empirical_accels =  False  
            self.ACCELS = 'acceloff'
        
        #### The below interprets that no input has been given for special name
        if self.SpecialRun_name == None:
            self.SpecialRun_name = ''
        else:
            self.SpecialRun_name = params['SpecialRun_name']
       
  
        #### Hardcoded constants:    
#         super().__init__(params)
        self.action       = params['action']
        self.tab = '  '
        self.tabtab = '       '
        if self.action   == 'read':
            print(self.tabtab*3   ,"......... READING GEODYN output")
           
            self.request_data = params['request_data'] 
        
        elif self.action == 'run':
            print(self.tabtab*3   ,"......... RUNNING GEODYN")
            
            #### Hardcoded constants:    
            #------ Point to the GEODYN executables
            self.GDYN_version     = 'Kamodo_pygeodyn_MODS'  #'pygeodyn_MODS'
#             self.GDYN_version     = 'pygeodyn_MODS'  #'pygeodyn_MODS'

            self.G2SDIR      = '/data/geodyn_proj/geodyn_code' + '/IIS/ORIG'
            self.G2EDIR      = '/data/geodyn_proj/geodyn_code' + '/IIE/' + self.GDYN_version

            
            
        if self.satellite == 'icesat2':
#             print('icesat2')            
            Inherit_Icesat2.__init__(self)

            
        elif self.satellite == 'starlette':
#             print('starlette')
            Inherit_Starlette.__init__(self)
            
#         super(self, self).__init__()
   
    
#         if self.satellite == 'icesat2':
#             print('icesat2')
#             class Inherit_Icesat2(Satellite_ICESat2):
#                 def __init__(self):
#                     print('inherit Satellite_ICESat2 ')
#                     Satellite_ICESat2.__init__(self)
            
#             Inherit_Icesat2()

            
#         elif self.satellite == 'starlette':
#             print('starlette')
            
#             class Inherit_Starlette(Satellite_Starlette):
#                 def __init__(self):
# #                     super(Satellite_Starlette, self).__init__()
#                     print('inherit Satellite_Starlette ')
#                     pass

#             Inherit_Starlette()
# #         super(self, self).__init__()
            
#         print(self.__dict__)
        
        
        