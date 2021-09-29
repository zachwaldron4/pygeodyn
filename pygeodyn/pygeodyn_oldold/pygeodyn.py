import sys
sys.path.insert(0, '/data/geodyn_proj/pygeodyn/utils_pygeodyn/')



from pygeodyn_modules import UtilModules
from pygeodyn_Control import pygeodyn_CONTROL
from pygeodyn_ReadOutput  import pyGeodyn_Readers




class pygeodyn(UtilModules, pygeodyn_CONTROL, pyGeodyn_Readers):

    def __init__(self, params):  
        
#         self.load_modules()
        
        
        
        #### CHANGEABLE INPUTS
        self.satellite         = params['satellite']
        self.den_model         = params['den_model']
        self.empirical_accels  = params['empirical_accels']
        self.SpecialRun_name   = params['SpecialRun_name']
        self.options_in        = params['options_in']
        self.verbose           = params['verbose']
        self.run_ID            = params['run_ID']
        
        self.set_satellite_params( self.satellite )
        self.set_density_model_setup_params( self.den_model )
        self.set_acceleration_params( self.empirical_accels )
        
        #### The below interprets that no input has been given for special name
        if self.SpecialRun_name == None:
            self.SpecialRun_name = ''
        else:
            self.SpecialRun_name = params['SpecialRun_name']

        if np.size(params['arc']) == 1:
            self.arc = params['arc']
            
            self.path_to_model = ('/data/data_geodyn/results/'+
                                       self.SATELLITE_dir +'/'+
                                       self.den_model+'/'+  
                                       self.den_model+'_'+ self.ACCELS + self.SpecialRun_name +'/')
            file_name =  str(self.SATELLITE_dir) + str(self.arc) + '.'+ str(self.grav_id)        
#             print('        ')
            print('     File path: ')
            print('     Loading ', self.path_to_model ,'.../',file_name,'' ,sep = '')

            ####  save the specific file names as "private members" with the _filename convention
            self._asciixyz_filename = self.path_to_model + 'XYZ_TRAJ/'+ file_name
            self._iieout_filename   = self.path_to_model + 'IIEOUT/'  + file_name
            self._density_filename  = self.path_to_model + 'DENSITY/' + file_name     
            #### the _filenames are now stored into self as members and can be passed to the next class

          
        else:
            self.arc = params['arc']
            print('Calling pygeodyn with multiple arcs...')
            pass


#     def run(self):
#         self.RUN_GEODYN()
            
    
    
    def HELP_GetStarted_Run():
        pygeodyn_inputs = ['run_ID',
                           'arc',
                           'satellite',
                           'den_model',
                           'empirical_accels',
                           'SpecialRun_name',
                           'options_in',
                           'verbose']

        options_satellite = ['starlette', 'iss', '(please dont choose iss yet)']
        options_density_model = ['msis86', 'msis00', 'msis2', 'jaachia71', 'dtm87']
        options_arc = [ '030914_2wk','030928_2wk','031012_2wk','031026_2wk','031109_2wk','031123_2wk','031207_2wk', '(broken)031221_2wk' ]
        options_empirical_accels = ['True', 'False']
        options_SpecialRun_name = [' ']
        options_RunID = [' ']
        options_options_in =  ["True", "False"]
        options_verbose = ['True', 'False']
        tab ='      '

        ##############################################################

        print('----- Welcome to the pygeodyn help feature! -----')

#         time.sleep(0.5)
        print(tab,"pygeodyn currently requires the following inputs in a dictionary: ")
        for i in pygeodyn_inputs:
            print(tab, tab, i)
#         time.sleep(0.5)
        print()

        print('You can either see a pre-made example or make your own run with user inputs.')
        example = input(' Example? (True) or enter your own inputs (False):   ')


        if example == False:
            #-------------------------------------------------------------
            print(tab,'Please choose from the following optional inputs...')    

            #-----------SATELLITE--------------------------------------------------    
            print(tab,tab,'Satellite options: ',options_satellite,'.' )
            user___satellite = input('satellite:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___satellite = input('satellite:   ')


            #----------DENSITY MODEL---------------------------------------------------   
            print(tab,tab,'Density model options: ',options_density_model,'.' )
            user___den_model = input('den_model:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___den_model = input('den_model:   ')

            #----------EMPIRICAL-ACCELS----------------------------------------------------
            print(tab,tab,'Will empirically adjusted accelerations be ON (True) or OFF (False): ',options_empirical_accels )
            user___empirical_accels = input('empirical_accels:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___empirical_accels = input('empirical_accels:   ')


            #---------OPTIONS-IN----------------------------------------------------
            print(tab,tab,'Do you want to turn on the DRHODZ update?: ',options_options_in, )
            user___DrhodzOption = input('drhodz_update:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___DrhodzOption = input('drhodz_update:   ')

            #---------VERBOSE-----------------------------------------------------
            print(tab,tab,'Do you want a verbose run? this prints a LOT of text during the run: ',options_verbose,)
            user___verbose = input('verbose:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___verbose = input('verbose:   ')

            #--------SPECIAL-RUN-NAME-----------------------------------------------------
            print(tab,tab,'Do you want to give the save files a special name?')
            print(tab,tab,'This is recommended if you do not want to overwrite previously saved data.',options_SpecialRun_name,)
            user___SpecialRun_name = input('SpecialRun_name:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___SpecialRun_name = input('SpecialRun_name:   ')

            #-------RUN-ID------------------------------------------------------
            print(tab,tab,'Give this run an identifying tag.  This only shows up in the text while it runs: ',options_RunID, )
            user___run_ID = input('run_ID:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___run_ID = input('run_ID:   ')


            #-------Choose Arcs------------------------------------------------------
            print(tab,tab,'Choose the arc to run. ',options_arc, )
            user___arc = input('arc:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___arc = input('arc:   ')

            print()
            print()
#             time.sleep(1)
            print('Here are your inputs.  Copy and paste the following into a cell:')

        else:
            user___run_ID           = 'Run_Arc_1'
            user___arc              = '030914_2wk'
            user___satellite        = 'starlette'
            user___den_model        = 'msis2'
            user___empirical_accels = 'False'
            user___SpecialRun_name  = '_developer_test'
            user___DrhodzOption     = 'True'
            user___verbose          = 'False'
            print()
            print()
#             time.sleep(1)
            print('------------------------------------------------------------------')

            print('Example inputs to edit.  Copy and paste the following into a cell:')

        print()
        print()
        print("#------ A dictionary containing the run parameters ------  ")
        print("run_params = {} ")
        print("run_params['run_ID']           =  '",user___run_ID,"'  "                      ,sep='')
        print("run_params['arc']              =  '",user___arc,"'  "                         ,sep='')
        print("run_params['satellite']        =  '",user___satellite,"'  "                   ,sep='')
        print("run_params['den_model']        =  '",user___den_model,"'  "                   ,sep='')
        print("run_params['empirical_accels'] =  ",user___empirical_accels,"  "              ,sep='')
        print("run_params['SpecialRun_name']  =  '",user___SpecialRun_name,"'  "             ,sep='')
        print("run_params['options_in']       =  {'DRHODZ_update':",user___DrhodzOption,"}  ",sep='')
        print("run_params['verbose']          =  ",user___verbose,"  "                       ,sep='')

        print()
        print()

        print("#------ Initialize the run Object ------ ")
        print("Obj_run_geodyn = pygeodyn_CONTROL(run_params)")
        print()      
        print("#------ Call the run fuction ------ ")
        print("Obj_run_geodyn.RUN_GEODYN()")


        return    
    
    
    