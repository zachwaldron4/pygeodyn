import sys
sys.path.insert(0, '/data/geodyn_proj/pygeodyn/pygeodyn_develop/')
sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_preprocessing/')

from PYGEODYN_Starlette import Satellite_Starlette
from PYGEODYN_ICESat2   import  Satellite_ICESat2
from util_classtools import Util_Tools





class Inherit_Icesat2(Satellite_ICESat2):
    def __init__(self):
        Satellite_ICESat2.__init__(self)
        pass
        

class Inherit_Starlette(Satellite_Starlette):
    def __init__(self):
        Satellite_Starlette.__init__(self)
        pass

     
    
    
    
    
    

# class Pygeodyn(Util_Tools, Inherit_Icesat2, Inherit_Starlette): #Satellite_Starlette,Satellite_ICESat2):


class Pygeodyn(Util_Tools, Inherit_Icesat2): #Inherit_Icesat2): Inherit_Starlette 
    def __init__(self, run_settings_yaml):  ####  Initialize a Pygeodyn Object with the YAML file containing the run settings as an input.
        
        
        import yaml
        from yaml.loader import SafeLoader
        # Open the file and load the file
        with open(run_settings_yaml) as f:
            run_settings = yaml.load(f, Loader=SafeLoader)
        self.run_settings = run_settings
        
#         print('Settings file: ',f)
        params = self.run_settings['run_params']
        
        
        self.satellite         = params['satellite']
        self.den_model         = params['den_model']
        self.SpecialRun_name   = params['SpecialRun_name']
        self.verbose           = params['verbose']
        self.arc_input         = params['arc']
        self.geodyn_StepSize   = params['geodyn_StepSize']
        self.set_density_model_setup_params( self.den_model )
        
        
        self.request_data  =self.run_settings['request_data']      
        
#         print('Did this run?')
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
#         self.action       = params['action']
        self.tab = '  '
        self.tabtab = '       '
#         if self.action   == 'read':
#             print(self.tabtab*3   ,"......... READING GEODYN output")
           
#             self.request_data = params['request_data'] 
        
#         elif self.action == 'run':
#             print(self.tabtab*3   ,"......... RUNNING GEODYN")
            
        #### Hardcoded constants:    
        #------ Point to the GEODYN executables
        self.GDYN_version     = 'Kamodo_pygeodyn_MODS'  #'pygeodyn_MODS'
        self.G2SDIR      = '/data/geodyn_proj/geodyn_code' + '/IIS/ORIG'
        self.G2EDIR      = '/data/geodyn_proj/geodyn_code' + '/IIE/' + self.GDYN_version

            
            
        if self.satellite == 'icesat2':
            Inherit_Icesat2.__init__(self)

            
        elif self.satellite == 'starlette':
            Inherit_Starlette.__init__(self)

            
#         print('Printing the keys as of Pygeodyn', self.__dict__.keys())
            
        for i,val in enumerate(self.run_settings):
#             print(i,val, self.run_settings[val])
            if val in  ['run_params', 'model_data_path','request_data','cards_to_remove',
                        'epoch_start','epoch_end', 'file_string',
                        'cd_adjustment_boolean','total_hours_in_run','hours_between_cd_adj']:
                pass
            else:
                if self.run_settings[val] == self.__dict__[val]:
                    pass
#                     print('fine')
                else:
#                     print(val)
#                     print(self.run_settings[val])
#                     print(self.__dict__[val])
                    self.__dict__[val] = self.run_settings[val]
            
            
            
###### THE BELOW ARE ATTEMPTS TO BETTER IMPORT VARIOUS SATELLITES
            
            
            
            
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
        
        
        