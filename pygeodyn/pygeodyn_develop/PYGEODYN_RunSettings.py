

class PygeodynRunSettingsReader():
    """ PygeodynRunSettingsReader class documentation
    
    Description: 
    ------------
       This class reads the run settings files and is inherited by the controller/satellite file
       

               
    """

    def __init__(self):  
        pass
    
    
    def read_yaml_file(self):
        import yaml
        from yaml.loader import SafeLoader

        # Open the file and load the file
        with open('run_settings_test.yaml') as f:
            data = yaml.load(f, Loader=SafeLoader)

            
        self.data = data
    
    
    
    
    
    
    