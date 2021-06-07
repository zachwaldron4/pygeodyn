'''
Kamodofication of the CTIPe model output
'''

import numpy as np
import time as ti
import glob, os
from kamodo import Kamodo, kamodofy
from scipy.interpolate import RegularGridInterpolator
from netCDF4 import Dataset
from datetime import datetime, timezone


# constants and dictionaries
ctipe_varnames = {'density':['rho','kg/m**3'],
                  'temperature':['T','K'],
                  'electron_temperature':['T_e','K'],
                  'ion_temperature':['T_i','K'],
                  'height':['H','m'],                      
                  'meridional_neutral_wind':['Vn_lat','m/s'],
                  'zonal_neutral_wind':['Vn_lon','m/s'],
                  'vertical_neutral_wind':['Vn_H','m/s'],
                  'neutral_temperature':['T_n','K'],
                  'mean_molecular_mass':['Rmt','amu'],
                  'electron_density':['N_e','1/m**3'],
                  'neutral_density':['N_n','1/m**3'],
                  'solar_heating':['Q_Solar','J/kg/s'],
                  'joule_heating':['Q_Joule','J/kg/s'],
                  'radiation_heat_cool':['Q_radiation','J/kg/s'],
                  'atomic_oxygen_density':['N_O','1/m**3'],
                  'molecular_oxygen_density':['N_O2','1/m**3'],
                  'molecular_nitrogen_density':['N_N2','1/m**3'],
                  'nitric_oxide_density':['N_NO','1/m**3'],
                  'nitric_oxide_ion_density':['N_NOplus','1/m**3'],
                  'molecular_nitrogen_ion_density':['N_N2plus','1/m**3'],  
                  'molecular_oxygen_ion_density':['N_O2plus','1/m**3'],
                  'atomic_nitrogen_ion_density':['N_Nplus','1/m**3'],
                  'atomic_oxygen_ion_density':['N_Oplus','1/m**3'],
                  'atomic_hydrogen_ion_density':['N_Hplus','1/m**3'],
                  'pedersen_conductivity':['Sigma_P','S'],
                  'hall_conductivity':['Sigma_H','S'],
                  'zonal_ion_velocity':['Vi_lon','m/s'],
                  'meridional_ion_velocity':['Vi_lat','m/s'],
                  'height_integrated_joule_heating':['W_Joule','W/m**2'],
                  'energy_influx':['Eflux_precip','W/m**2'],
                  'mean_energy':['Eavg_precip','keV'],
                  'total_electron_content':['TEC','10**16/m**2'],
                  'theta_electric_field_at_140km':['E_theta140km','V/m'],
                  'lambda_electric_field_at_140km':['E_lambda140km','V/m'],
                  'theta_electric_field_at_300km':['E_theta300km','V/m'],
                  'lambda_electric_field_at_300km':['E_lambda300km','V/m']}


# some functions from tiegcm.util
def to_range(x, start, limit):
        """wraps x into range [start, limit], useful for longitude"""
        
        return start + (x - start) % (limit - start)

#convert an array of timestamps to an array of hrs since midnight
@np.vectorize
def ts_to_hrs(time_val, filedate):
    return (datetime.utcfromtimestamp(time_val).replace(tzinfo=timezone.utc)-filedate).total_seconds()/3600.

def str_to_hrs(time_val, filedate):
    return (datetime.strptime(time_val, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)-\
            filedate).total_seconds()/3600.

#main class
class CTIPe(Kamodo):
    def __init__(self, filename, variables_requested = None, filetimes=False,
                 runname = "noname", printfiles=False, **kwargs):  
                #date = None, date is in filename, so exclude? (self.date ....)
                #time=None, runpath = "./", not used

        # input file name can be one of the 4 files for each day of model outputs
        # YYYYMMDD-plot-[density|height|neutral|plasma].nc files
        # only the density, height and neutral files have data and are read
        super(CTIPe, self).__init__()   #what does this line do???
        
        #check for wrapped data output in dir, create if necessary
        if 'wrapped' not in filename:
            print('Wrapped file not given, searching for file...')
            file_pattern = filename.split('plot')[0]  #.../CTIPe/2015-03-18- to choose this group of files
            files = glob.glob(file_pattern+'plot-*-wrapped.nc')
            if len(files)==3: 
                print('Found file.')
                filename = files[0]  #all three files exist, use them
            else: 
                print('Files not found. Generating...')
                from kamodo.readers.ctipe_data_wrapper import ctipe_wrap_files as wrap
                filename = wrap(filename)  #produce the wrapped files, returns the new filename
        elif not os.path.isfile(filename): #check to make sure the file exists
            print('Files not found. Generating...')
            from kamodo.readers.ctipe_data_wrapper import ctipe_wrap_files as wrap
            filename = wrap(filename.split('-wrapped')[0]+'.nc')  #produce the wrapped files, returns the new filename                
        
        filetype_list = ['plot-density-wrapped','plot-height-wrapped',
                         'plot-neutral-wrapped','plot-plasma-wrapped']
        file_beg, file_end = [filename.split(filetype) for filetype in filetype_list 
                              if len(filename.split(filetype))>1][0]
        filename_density, filename_height, filename_neutral = [
            file_beg+filetype+file_end for filetype in filetype_list[:-1]]
        if printfiles: 
            print('Files:\n'+filename_density+'\n'+filename_height+'\n'+filename_neutral)    
            
        #establish time attributes first
        self._ctipe_density = Dataset(filename_density)
        self.filedate = datetime.strptime(file_beg[-11:-1]+' 00:00:00', 
                                          '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)
        t = np.array(self._ctipe_density.variables['time'])
        self.datetimes=[datetime.utcfromtimestamp(t[0]).isoformat(sep=' '), 
                        datetime.utcfromtimestamp(t[-1]).isoformat(sep=' ')]  #strings
        self.filetimes=[t[0], t[-1]]   #timestamps in seconds for matching in wrapper
        self.timerange={'min':self.datetimes[0], 'max':self.datetimes[1],
                            'n':len(t)}     #strings in format = YYYY-MM-DD HH:MM:SS 
        if filetimes: 
            return

        #pull in remaining datasets from files into kamodo object
        self._ctipe_height = Dataset(filename_height)  #in meters
        self._ctipe_neutral = Dataset(filename_neutral)
        
        #pull in coordinate variables into kamodo object (density file)
        self._time = np.array(ts_to_hrs(t, self.filedate))  #convert timestamps to hrs since midnight 
        self._ilev_density = np.array(self._ctipe_density.variables['plev'])
        self._lat_density = np.array(self._ctipe_density.variables['lat'])
        self._lon_density = np.array(self._ctipe_density.variables['lon'])

        #pull in coordinate variables into kamodo object (height file, in km)
        self._ht_height = np.array(self._ctipe_height.variables['ht'])
        self._lat_height = np.array(self._ctipe_height.variables['lat'])
        self._lon_height = np.array(self._ctipe_height.variables['lon'])

        ##pull in coordinate variables into kamodo object (neutral file)
        self._ilev_neutral = np.array(self._ctipe_neutral.variables['plev'])
        self._lat_neutral = np.array(self._ctipe_neutral.variables['lat'])
        self._lon_neutral = np.array(self._ctipe_neutral.variables['lon'])
        self._elat_neutral = np.array(self._ctipe_neutral.variables['elat'])
        self._elon_neutral = np.array(self._ctipe_neutral.variables['elon'])

        #initialize variables
        self._registered = 0  
        super(CTIPe, self).__init__()   #what does this line do???
        self.filename = filename
        #self.runpath = runpath
        self.runname = runname
        self.missing_value = np.NAN
        self.variables=dict()
        
        #if variables_requested not given, collect all values from dict above as a list
        if variables_requested is None:
            variables_requested = [value[0] for key,value in ctipe_varnames.items()]
        
        # add height variable needed to height (not IP-level) interpolatioms
        if 'H' not in variables_requested: variables_requested.append('H')
        #print(f'Requested {len(variables_requested)} variables: {variables_requested} \n')
        
        #collect list of ctipe variable name equivalents
        var_names = [key for key, value in ctipe_varnames.items() if value[0] in variables_requested]
        extra_variables = [var for var in variables_requested if var not in 
                     [value[0] for key, value in ctipe_varnames.items()]]
        if len(extra_variables)>0:   #pull out variables not allowed and error if not empty
            raise AttributeError("No such variable(s):{}".format(extra_variables))

        #cycle through all variables in one loop
        bad_varnames={'density': ['ZMAG','Rmt','H'], 'height': ['ZMAG'],
                  'neutral': ['ZMAG','electron_density']}   #initialize bad_var dictionary per file_type
        for varname in var_names:
            #determine source file type for variable
            file_type=''
            if varname in self._ctipe_density.variables.keys(): file_type = 'density'
            elif varname in self._ctipe_height.variables.keys(): file_type = 'height'
            elif varname in self._ctipe_neutral.variables.keys(): file_type = 'neutral'
            else:
                raise AttributeError(f"{varname} not found in the files' metadata.")  
            
            #set units, initialize variables
            variable = getattr(self, '_ctipe_'+file_type).variables[varname]#.__array__()  #set variable
            units = ctipe_varnames[varname][-1]
            if (len(variable.shape) not in [3,4]) or (varname in bad_varnames[file_type]):
                continue  #if not 3D or 4D or not allowed, skip to next variable
            
            #register allowed 3D and 4D variables
            kamodo_varname=ctipe_varnames[varname][0]  #retreive standardized name
            #if file_type=='height': print(varname, kamodo_varname, variable.shape)
            self.variables[kamodo_varname] = dict(units = units, data = variable)  #register in object
            if len(variable.shape) == 4:  #define and register interpolators for each
                self.register_4D_variable(units, np.array(variable), kamodo_varname, file_type)
            elif len(variable.shape) == 3:
                self.register_3D_variable(units, np.array(variable), kamodo_varname, file_type)
        
        #set gridSize for each coordinate
        self.dt = min(np.diff(self._time))
        self.dlat = min(np.diff(self._lat_density)) #same for _lat_height and _lat_neutral
        self.dlon = min(np.diff(self._lon_density)) #same for _lon_height and _lon_neutral
        self.dilev = min(np.diff(self._ilev_density)) #same for _ilev_neutral
        self.dheight = min(np.diff(self._ht_height))
        self.delon = min(np.diff(self._elon_neutral))
        self.delat = min(np.diff(self._elat_neutral))
        #Don't set gridsize here since it is determined by the different coordinate dependencies
        #self.gridSize=len(self._ilev_density)*len(self._lat_density)*len(self._lon_density)
        
        #set domains of plotting coordinate variables
        self.lonrange={'min':0.,'max':360.,'n':73}
        self.latrange={'min':-90.,'max':90.,'n':37}
        self.iprange={'min':self._ilev_density.min(),'max':self._ilev_density.max(),
                          'n':len(self._ilev_density)}
        self.htrange={'min':self._ht_height.min(),'max':self._ht_height.max(),
                          'n':len(self._ht_height)}  #height in km        
        
        # Initialize interpolation values for plotting
        self.plots = dict()
        self.plottype = "" # empty default to force reinterpolation and regeneration
        self.cutVtext, self.cutV = 'IP', 10.   #height cut
        self.cutLtext, self.cutL = 'Lat [deg]', 0.
        self.nt, self.nX, self.nY, self.nZ = 1, 1, 37, 73   #t, height, lat, lon
        self.newx = self.cutV
        self.newy = np.linspace(-90., 90., self.nY)
        self.newz = np.linspace(0., 360., self.nX)
        self.newt = self._time[0]
        self.tunit, self.xunit, self.yunit, self.zunit ='s', '', 'deg', 'deg'
        self.tol = 1.1
        self.plots[self.plottype] = dict(cutVtext=self.cutVtext, cutV=self.cutV, 
                                         cutLtext=self.cutLtext, cutL=self.cutL,
                                         tol=self.tol, nt = self.nt, nX=self.nX, 
                                         nY=self.nY, nZ=self.nZ, newt=self.newt, 
                                         newx=self.newx, newy=self.newy, newz=self.newz)  
    
        # Get grids ready to use
        #self.setup_interpolating_grids()
        

    def define_3d_interpolators(self,units,variable,varname,t,lat,lon):
        '''Define interpolators for 3D variables uniformly'''
        
        rgi = RegularGridInterpolator((t, lat, lon),
                                      variable, bounds_error = False)
        @kamodofy(units = units, data = variable)
        def interpolator(xvec):
            """Interpolates 3d variable without a grid"""
            return rgi(xvec)
        return interpolator

    #define and register a 3D variable
    def register_3D_variable(self, units, variable, varname, file_type):
        """Registers a 3d interpolator with 3d signature"""
        
        #determine coordinate variables by file_type
        if file_type=='density': 
            t, lat, lon = self._time, self._lat_density, self._lon_density
            xvec_dependencies = {'time':'s','lat':'deg','lon':'deg'}
        if file_type=='height': 
            t, lat, lon = self._time, self._lat_height, self._lon_height
            xvec_dependencies = {'time':'s','lat':'deg','lon':'deg'}
        if file_type=='neutral':
            if variable.shape[1] == self._elat_neutral.shape[0]:
                t, lat, lon = self._time,self._elat_neutral, self._elon_neutral  
                xvec_dependencies = {'time':'s','elat':'deg','elon':'deg'}
            else:
                t, lat, lon = self._time, self._lat_neutral, self._lon_neutral  
                xvec_dependencies = {'time':'s','lat':'deg','lon':'deg'}        
        
        #define and register the interpolators
        interpolator = self.define_3d_interpolators(
                                            units,variable,varname,t,lat,lon)
        self.register_interpolators(varname, interpolator, xvec_dependencies)
        return 
    
    def define_4d_interpolators(self,units,variable,varname,t,ht,lat,lon):
        
        rgi = RegularGridInterpolator((t, ht, lat, lon), 
                                      variable, bounds_error = False)
        
        @kamodofy(units = units, data = variable)
        def interpolator(xvec):
            """Interpolates 4d variable without a grid"""
            return rgi(xvec)
        return interpolator
    
    #define and register a 4D variable
    def register_4D_variable(self, units, variable, varname, file_type):
        """Registers a 4d interpolator with 4d signature"""
        
        #determine coordinate variables by file_type
        if file_type=='density': 
            t, z, lat, lon = self._time, self._ilev_density, self._lat_density, self._lon_density
            xvec_dependencies = {'time':'s','ilev':'m/m','lat':'deg','lon':'deg'}
        if file_type=='height':
            t, z, lat, lon = self._time, self._ht_height, self._lat_height, self._lon_height
            xvec_dependencies = {'time':'s','height':'km','lat':'deg','lon':'deg'}
        if file_type=='neutral':
            t, z, lat, lon= self._time, self._ilev_neutral, self._lat_neutral, self._lon_neutral
            xvec_dependencies = {'time':'s','ilev':'m/m','lat':'deg','lon':'deg'}
        
        #define and register the interpolators
        interpolator = self.define_4d_interpolators(
                                            units,variable,varname,t,z,lat,lon)
        self.register_interpolators(varname, interpolator, xvec_dependencies)
        return

    def register_interpolators(self, varname, interpolator, xvec_dependencies):
        '''Register interpolators for each variable'''
        
        self[varname] = interpolator
        self.variables[varname]['xvec'] = xvec_dependencies
        #self.variables[varname]['interpolator']=interpolator
        self._registered += 1  
        return     
    
    '''----------------------- Plotting code below here --------------------'''

    def setup_interpolating_grids(self):
        '''setup the grid for interpolatation'''
        
        tt, xx, yy, zz = np.meshgrid(self.newt,self.newx,self.newy,self.newz, indexing = 'xy')
        self.newgrid = np.array([np.ravel(tt), np.ravel(xx), np.ravel(yy), np.ravel(zz)]).T
        #print('Gridshape', self.newgrid.shape)
        self.plots[self.plottype]['newgrid'] = self.newgrid
        return

    def set_plot(self, plottype="XY", cutV=10, cutL=0, timerange={},
                 lonrange={'min':0,'max':360,'n':73}, latrange={'min':-90,'max':90,'n':37},
                 iprange={'min':1,'max':15,'n':15}, htrange={'min':140,'max':2000,'n':94}):
        '''
\nSet plotting variables for available preset plot types with syntax below.
Choose IP variations for functions that take ilev, and H variations for functions that take height.
Generic syntax for generating a plot (**kwargs not required):
    ctipe.set_plot(plottype, **kwargs)
    plot=ctipe.get_plot(variable_name, colorscale='Viridis')
    iplot(plot)
Example for LonLatIP (cutV is cut in IP; time value plotted is min given):
    ctipe.set_plot('LonLat_IP',cutV=14,latrange={'min':-90,'max':90,'n':181),
                   lonrange={'min':0,'max':360,'n':361),
                   timerange={'min':'2015-03-20 07:50:25'})
Example for LonLatH (cutV is cut in km; time value plotted is min given):
    ctipe.set_plot('LonLat_H',cutV=400,latrange={'min':-90,'max':90,'n':181),
                   lonrange={'min':0,'max':360,'n':361),
                   timerange={'min':'2015-03-20 07:50:25'})
Example for LonIP (cutL is cut in latitude; time value plotted is min given):
    ctipe.set_plot('LonIP',cutL=14,iprange={'min':1,'max':15,'n':30),
                   lonrange={'min':0,'max':360,'n':361),
                   timerange={'min':'2015-03-20 07:50:25'})
Example for LonH (cutL is cut in latitude; time value plotted is min given):
    ctipe.set_plot('LonH',cutL=14,htrange={'min':140,'max':2000,'n':181),
                   lonrange={'min':0,'max':360,'n':361),
                   timerange={'min':'2015-03-20 07:50:25'})                
Example for LatIP (cutL is cut in longitude; time value plotted is min given):
    ctipe.set_plot('LatIP',cutL=14,iprange={'min':1,'max':15,'n':30),
                   latrange={'min':-90,'max':90,'n':181),
                   timerange={'min':'2015-03-20 07:50:25'})
Example for LatH (cutL is cut in longitude; time value plotted is min given):
    ctipe.set_plot('LatH',cutL=14,htrange={'min':140,'max':2000,'n':181),
                   latrange={'min':-90,'max':90,'n':181),
                   timerange={'min':'2015-03-20 07:50:25'}) 
Example for TimeLon_IP (cutV is in IP, cutL is in latitude, time range given is plotted)
    ctipe.set_plot('TimeLon_IP',cutV=10,cutL=50,lonrange={'min':0,'max':360,'n':361),
                  timerange={min:'2015-03-20 01:50:25',max:'2015-03-20 02:50:25','n':100})
Example for TimeLon_H (cutV is in km, cutL is in latitude, time range given is plotted)
    ctipe.set_plot('TimeLon_IP',cutV=400,cutL=50,lonrange={'min':0,'max':360,'n':361),
                  timerange={min:'2015-03-20 01:50:25',max:'2015-03-20 02:50:25','n':100})
Example for TimeLat_IP (cutV is in IP, cutL is in longitude, time range given is plotted)
    ctipe.set_plot('TimeLat_IP',cutV=10,cutL=50,latrange={'min':-90,'max':90,'n':181),
                  timerange={min:'2015-03-20 01:50:25',max:'2015-03-20 02:50:25','n':100})                 
Example for TimeLat_IP (cutV is in km, cutL is in longitude, time range given is plotted)
    ctipe.set_plot('TimeLat_IP',cutV=400,cutL=50,latrange={'min':-90,'max':90,'n':181),
                  timerange={min:'2015-03-20 01:50:25',max:'2015-03-20 02:50:25','n':100})                
Example for TimeIP (cutV is in latitude, cutL is in longitude, time range given is plotted)
    ctipe.set_plot('TimeLat_IP',cutV=10,cutL=50,iprange={'min':1,'max':15,'n':30),
                  timerange={min:'2015-03-20 01:50:25',max:'2015-03-20 02:50:25','n':100})                
Example for TimeH (cutV is in latitude, cutL is in longitude, time range given is plotted)
    ctipe.set_plot('TimeLat_IP',cutV=10,cutL=50,htrange={'min':140,'max':2000,'n':100),
                  timerange={min:'2015-03-20 01:50:25',max:'2015-03-20 02:50:25','n':100})                 
        '''
        
        #set defaults if value(s) not given, esp for timerange
        if 'min' not in timerange.keys():
            timerange['min']=self.datetimes[0]  #keep as strings here
        else:  #check that given time is in range
            testmin = str_to_hrs(timerange['min'], self.filedate)
            if testmin<self._time[0] or testmin>self._time[-1]:  #timestamp values
                timerange['min']=self.timerange['min']  #return min if value given not valid
                print('Given min time is not in file. Returning minimum time in file.')
        if 'max' not in timerange.keys():
            timerange['max']=self.datetimes[1]  #convert to timestamps later
        else:  #check that given time is in range
            testmax = str_to_hrs(timerange['max'], self.filedate)
            if testmax<self._time[0] or testmax>self._time[-1]:  #timestamp values
                timerange['max']=self.timerange['max']  #return max if value given not valid
                print('Given max time is not in file. Returning maximum time in file.')
        if 'n' not in timerange.keys(): timerange['n']=self.timerange['n']
        if 'min' not in lonrange.keys(): lonrange['min']=self.lonrange['min']
        if 'max' not in lonrange.keys(): lonrange['max']=self.lonrange['max']
        if 'n' not in lonrange.keys(): lonrange['n']=self.lonrange['n']
        if 'min' not in latrange.keys(): latrange['min']=self.latrange['min']
        if 'max' not in latrange.keys(): latrange['max']=self.latrange['max']
        if 'n' not in latrange.keys(): latrange['n']=self.latrange['n']
        if 'min' not in iprange.keys(): iprange['min']=self.iprange['min']  
        if 'max' not in iprange.keys(): iprange['max']=self.iprange['max']
        if 'n' not in iprange.keys(): iprange['n']=self.iprange['n']
        if 'min' not in htrange.keys(): htrange['min']=self.htrange['min']  
        if 'max' not in htrange.keys(): htrange['max']=self.htrange['max']
        if 'n' not in htrange.keys(): htrange['n']=self.htrange['n']
        tic = ti.perf_counter()  #start timer
        
        #compare previous values to new values to see if plot needs to be redone
        plotpar_listA = [self.plottype, self.cutV, self.cutL, self.lonrange,\
                         self.latrange, self.timerange, self.htrange, self.iprange]
        plotpar_listB = [plottype, cutV, cutL, lonrange, latrange, timerange,\
                         htrange, iprange]        
        plotpar_comp = [0 if aa==bb else 1 for aa, bb in zip(plotpar_listA, plotpar_listB)]
        if sum(plotpar_comp)==0:
            print('Plottype and variables are unchanged, returning.')
            return
        self.timerange,self.lonrange,self.latrange,self.htrange,self.iprange = \
            timerange,lonrange,latrange,htrange,iprange  #set new values if changed
        self.plottype, self.cutV, self.cutL = plottype, cutV, cutL

        #set arrays/constants for each plot type
        if plottype == "LonLat_IP" or plottype == 'LonLat_H':
            self.plottype = plottype
            if plottype=='LonLat_IP': 
                self.cutVtext, self.dz, self.dzunit, self.numZ = \
                    'IP', self.dilev, '', len(self._ilev_density)
            if plottype=='LonLat_H': 
                self.cutVtext, self.dz, self.dzunit, self.numZ = \
                    'Height [km]', self.dheight, 'km', len(self._ht_height)
            self.cutV, self.newx = cutV, cutV
            self.nt, self.nX, self.nY, self.nZ = 1, 1, latrange['n'], lonrange['n']
            self.newt = str_to_hrs(timerange['min'], self.filedate)
            self.newy = np.linspace(latrange['min'],latrange['max'],latrange['n'])
            self.newz = np.linspace(lonrange['min'],lonrange['max'],lonrange['n'])
        elif plottype == "LonIP":
            self.plottype = plottype
            self.cutLtext, self.cutL, self.newy = 'Lat [deg]', cutL, cutL
            self.dz, self.dzunit, self.numZ = self.dilev, '', len(self._ilev_density)
            self.nt, self.nX, self.nY, self.nZ = 1, iprange['n'], 1, lonrange['n']
            self.newt = str_to_hrs(timerange['min'], self.filedate)
            self.newx=np.linspace(iprange['min'],iprange['max'],iprange['n'])
            self.newz=np.linspace(lonrange['min'],lonrange['max'],lonrange['n'])
        elif plottype == "LonH":
            self.plottype = plottype
            self.cutLtext, self.cutL, self.newy = 'Lat [deg]', cutL, cutL
            self.dz, self.dzunit, self.numZ = self.dheight, 'km', len(self._ht_height)
            self.nt, self.nX, self.nY, self.nZ = 1, htrange['n'], 1, lonrange['n']
            self.newt = str_to_hrs(timerange['min'], self.filedate)
            self.newx=np.linspace(htrange['min'],htrange['max'],htrange['n'])
            self.newz=np.linspace(lonrange['min'],lonrange['max'],lonrange['n'])
        elif plottype == "LatIP":
            self.plottype = plottype
            self.cutLtext, self.cutL, self.newz = 'Lon [deg]', cutL, cutL
            self.dz, self.dzunit, self.numZ = self.dilev, '', len(self._ilev_density)
            self.nt, self.nX, self.nY, self.nZ = 1, iprange['n'], latrange['n'], 1
            self.newt = str_to_hrs(timerange['min'], self.filedate)
            self.newx = np.linspace(iprange['min'],iprange['max'],iprange['n'])
            self.newy = np.linspace(latrange['min'],latrange['max'],latrange['n'])
        elif plottype == "LatH":
            self.plottype = plottype
            self.cutLtext, self.cutL, self.newz = 'Lon [deg]', cutL, cutL
            self.dz, self.dzunit, self.numZ = self.dheight, 'km', len(self._ht_height)
            self.nt, self.nX, self.nY, self.nZ = 1, htrange['n'], latrange['n'], 1
            self.newt = str_to_hrs(timerange['min'], self.filedate)
            self.newx = np.linspace(htrange['min'],htrange['max'],htrange['n'])
            self.newy = np.linspace(latrange['min'],latrange['max'],latrange['n'])
        elif plottype == "TimeLon_IP" or plottype == 'TimeLon_H':   #new
            self.plottype = plottype
            if plottype=='TimeLon_IP': 
                self.cutVtext, self.dz, self.dzunit, self.numZ = \
                    'IP', self.dilev, '', len(self._ilev_density)
            if plottype=='TimeLon_H': 
                self.cutVtext, self.dz, self.dzunit, self.numZ = \
                    'Height [km]', self.dheight, 'km', len(self._ht_height)
            self.cutLtext, self.cutL, self.newy = 'Lat [deg]', cutL, cutL
            self.cutV, self.newx = cutV, cutV
            self.nt, self.nX, self.nY, self.nZ = timerange['n'], 1, 1, lonrange['n']
            self.newt = np.linspace(str_to_hrs(timerange['min'], self.filedate), 
                                    str_to_hrs(timerange['max'], self.filedate),
                                    timerange['n'], dtype=float)
            self.newz = np.linspace(lonrange['min'],lonrange['max'],lonrange['n'])       
        elif plottype == "TimeLat_IP" or plottype == 'TimeLat_H':   #new
            self.plottype = plottype
            if plottype=='TimeLat_IP': 
                self.cutVtext, self.dz, self.dzunit, self.numZ = \
                    'IP', self.dilev, '', len(self._ilev_density)
            if plottype=='TimeLat_H': 
                self.cutVtext, self.dz, self.dzunit, self.numZ = \
                    'Height [km]', self.dheight, 'km', len(self._ht_height)
            self.cutLtext, self.cutL, self.newz = 'Lon [deg]', cutL, cutL
            self.cutV, self.newx = cutV, cutV
            self.nt, self.nX, self.nY, self.nZ = timerange['n'], 1, latrange['n'], 1
            self.newt = np.linspace(str_to_hrs(timerange['min'], self.filedate), 
                                    str_to_hrs(timerange['max'], self.filedate),
                                    timerange['n'], dtype=float)
            self.newy = np.linspace(latrange['min'],latrange['max'],latrange['n'])  
        elif plottype == "TimeIP":   #new
            self.plottype = plottype
            self.cutVtext, self.cutV, self.newy = 'Lat [deg]', cutV, cutV
            self.cutLtext, self.cutL, self.newz = 'Lon [deg]', cutL, cutL
            self.dz, self.dzunit, self.numZ = self.dilev, '', len(self._ilev_density)
            self.nt, self.nX, self.nY, self.nZ = timerange['n'], iprange['n'], 1, 1
            self.newt = np.linspace(str_to_hrs(timerange['min'], self.filedate), 
                                    str_to_hrs(timerange['max'], self.filedate),
                                    timerange['n'], dtype=float)
            self.newx = np.linspace(iprange['min'],iprange['max'],iprange['n'])   
        elif plottype == "TimeH":   #new
            self.plottype = plottype
            self.cutVtext, self.cutV, self.newy = 'Lat [deg]', cutV, cutV
            self.cutLtext, self.cutL, self.newz = 'Lon [deg]', cutL, cutL
            self.dz, self.dzunit, self.numZ = self.dheight, 'km', len(self._ht_height)
            self.nt, self.nX, self.nY, self.nZ = timerange['n'], htrange['n'], 1, 1
            self.newt = np.linspace(str_to_hrs(timerange['min'], self.filedate), 
                                    str_to_hrs(timerange['max'], self.filedate),
                                    timerange['n'], dtype=float)
            self.newx = np.linspace(htrange['min'],htrange['max'],htrange['n'])  
        else:
            print('Error, unknown plottype. ',plottype)
            return   
         
        #store values
        self.plots[plottype] = dict(cutVtext=self.cutVtext, cutV=self.cutV, 
                                    cutLtext=self.cutLtext, cutL=self.cutL,
                                    tol=self.tol, nt=self.nt, nX=self.nX, nY=self.nY, 
                                    nZ=self.nZ, newt=self.newt, newx=self.newx, 
                                    newy=self.newy, newz=self.newz)
        self.setup_interpolating_grids()
        for varname in self.variables: #retrieve interpolators
            self.plots[plottype][varname]=self[varname] #self.variables[varname]['interpolator']
        toc = ti.perf_counter()  #end timer
        print(f"Time resetting plot and precomputing interpolations: {toc - tic:0.4f} seconds")
        return
    
    def get_plot(self, var, colorscale="Viridis",style="linear"):
        '''
        Return a plotly figure object for the available plot types set in set_plot()..
        colorscale = Viridis [default], Cividis, BlueRed or Rainbow
        '''
        
        #need to determine elat/elon vs lat/lon dependence here with gridified var
        #if ('ijk' not in var): arg_dict = getattr(self, var+'_ijk').meta['arg_units']
        #else: arg_dict = getattr(self, var).meta['arg_units']
        arg_dict = self.variables[var]['xvec']
        if 'elon' in arg_dict.keys(): 
            self.dLON, self.nLON = self.delon, len(self._elon_neutral)
        else: 
            self.dLON, self.nLON = self.dlon, len(self._lon_density)
        if 'elat' in arg_dict.keys(): 
            self.dLAT, self.nLAT = self.delat, len(self._elat_neutral)
        else: 
            self.dLAT, self.nLAT = self.dlat, len(self._lat_density)
        self.gridSize = self.nLAT*self.nLON*self.numZ
        
        print(f'set_plot::colorscale={colorscale}, style={style}')
        print(f'Runname: {self.runname}')
        #Set some text strings  
        txtbot=f"Model: CTIPe, {self.gridSize} volume cells, dt={self.dt} hrs, dlat={self.dLAT} deg, "+\
            f"dlon={self.dLON} deg, dz={self.dz} {self.dzunit}."
        if style == "linear":  txtbar=var + " [" + self.variables[var]['units'] + "]"
        if style == "log":  txtbar="log("+var + ") [" + self.variables[var]['units'] + "]"
        
        # Get values from interpolation already computed
        result=self[var](self.newgrid)

        #set variables specific to plot type
        if self.plottype == "LonLat_IP" or self.plottype=="LonLat_H":
            txttop=self.cutVtext+"=" + str(self.plots[self.plottype]['cutV']) + " slice,  Time = " \
                + self.timerange['min'] + 'UTC'
            xint, xunit, nx, xlabel = self.newz, self.zunit, self.nZ, 'Lon [deg]'
            yint, yunit, ny, ylabel = self.newy, self.yunit, self.nY, 'Lat [deg]'
            xformat, yformat, zformat, kamodo_plot = '.2f', '.2f', '.4g', 'XY'
        elif self.plottype == "LonIP":
            txttop=self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice,  Time = " \
                + self.timerange['min'] + 'UTC'
            xint, xunit, nx, xlabel = self.newz, self.zunit, self.nZ, 'Lon [deg]'
            yint, yunit, ny, ylabel = self.newx, self.xunit, self.nX, 'IP '    
            xformat, yformat, zformat, kamodo_plot = '.2f', '.2f', '.4g', 'XZ'
        elif self.plottype == "LonH":    
            txttop=self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice,  Time = " \
                + self.timerange['min'] + 'UTC'
            xint, xunit, nx, xlabel = self.newz, self.zunit, self.nZ, 'Lon [deg]'
            yint, yunit, ny, ylabel = self.newx, self.xunit, self.nX, 'Height [km]'    
            xformat, yformat, zformat, kamodo_plot = '.2f', '.4g', '.4g', 'XZ'      
        elif self.plottype == 'LatIP':
            txttop=self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice,  Time = " \
                + self.timerange['min'] + 'UTC'
            xint, xunit, nx, xlabel = self.newy, self.yunit, self.nY, 'Lat [deg]'
            yint, yunit, ny, ylabel = self.newx, self.xunit, self.nX, 'IP '
            xformat, yformat, zformat, kamodo_plot = '.2f', '.2f', '.4g', 'YZ'
        elif self.plottype == 'LatH':
            txttop=self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice,  Time = " \
                + self.timerange['min'] + 'UTC'
            xint, xunit, nx, xlabel = self.newy, self.yunit, self.nY, 'Lat [deg]'
            yint, yunit, ny, ylabel = self.newx, self.xunit, self.nX, 'Height [km]'
            xformat, yformat, zformat, kamodo_plot = '.2f', '.4g', '.4g', 'YZ'
        elif self.plottype == "TimeLon_IP" or self.plottype=="TimeLon_H":
            txttop=self.cutVtext+"=" + str(self.plots[self.plottype]['cutV']) + " slice, "+\
                self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice"
            xint, xunit, nx = self.newt, self.tunit, self.nt
            xlabel = 'Hours on '+self.filedate.isoformat(sep=' ')[0:10]+' [UTC]'
            yint, yunit, ny, ylabel = self.newz, self.zunit, self.nZ, 'Lon [deg]'
            xformat, yformat, zformat, kamodo_plot = '.2f', '.2f', '.4g', 'TY'
        elif self.plottype == "TimeLat_IP" or self.plottype=="TimeLat_H":
            txttop=self.cutVtext+"=" + str(self.plots[self.plottype]['cutV']) + " slice, "+\
                self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice"
            xint, xunit, nx = self.newt, self.tunit, self.nt
            xlabel = 'Hours on '+self.filedate.isoformat(sep=' ')[0:10]+' [UTC]'
            yint, yunit, ny, ylabel = self.newy, self.yunit, self.nY, 'Lat [deg]'
            xformat, yformat, zformat, kamodo_plot = '.2f', '.2f', '.4g', 'TY'
        elif self.plottype == 'TimeIP':
            txttop=self.cutVtext+"=" + str(self.plots[self.plottype]['cutV']) + " slice, "+\
                self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice"
            xint, xunit, nx = self.newt, self.tunit, self.nt
            xlabel = 'Hours on '+self.filedate.isoformat(sep=' ')[0:10]+' [UTC]'
            yint, yunit, ny, ylabel = self.newx, self.xunit, self.nX, 'IP '
            xformat, yformat, zformat, kamodo_plot = '.2f', '.2f', '.4g', 'YZ'
        elif self.plottype == 'TimeH':
            txttop=self.cutVtext+"=" + str(self.plots[self.plottype]['cutV']) + " slice, "+\
                self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice"
            xint, xunit, nx = self.newt, self.tunit, self.nt
            xlabel = 'Hours on '+self.filedate.isoformat(sep=' ')[0:10]+' [UTC]'
            yint, yunit, ny, ylabel = self.newx, self.xunit, self.nX, 'Height [km]'
            xformat, yformat, zformat, kamodo_plot = '.2f', '.4g', '.4g', 'YZ'
        else:
            print(f'Plot type {self.plottype} not yet working.')
            return
            
        #begin generic code for 2D heat plots
        if len(kamodo_plot)==2:
            xrange={'min':xint.min(),'max':xint.max(),'n':len(xint)}
            yrange={'min':yint.min(),'max':yint.max(),'n':len(yint)}
            if 'TimeLon' in self.plottype or 'TimeLat' in self.plottype:
                result2=np.reshape(result,(nx,ny))  # Reshape interpolated values into 2D
            else:
                result2=np.reshape(result,(ny,nx))        
            if style == "linear":
                def plot_XY(xint = xint, yint = yint):
                    return result2
            if style == "log":
                def plot_XY(xint = xint, yint = yint):
                    return np.log(result2)        
            plotXY = Kamodo(plot_XY = plot_XY)  #func_name = func
            fig = plotXY.plot(plot_XY = dict())        
            if kamodo_plot=='XY':
                fig.update_xaxes(title_text='', scaleanchor='y') # label included in annotations
            if kamodo_plot=='XZ' or kamodo_plot=='YZ' or kamodo_plot=='TY' or kamodo_plot=='TZ':
                fig.update_xaxes(title_text='', scaleanchor=None)
            fig.update_yaxes(title_text=ylabel) 

            # Choose colorscale
            if colorscale == "Rainbow":
                fig.update_traces(
                    colorscale=[[0.00, 'rgb(0,0,255)'],
                                [0.25, 'rgb(0,255,255)'],
                                [0.50, 'rgb(0,255,0)'],
                                [0.75, 'rgb(255,255,0)'],
                                [1.00, 'rgb(255,0,0)']]
                )
            elif colorscale == "BlueRed":
                fig.update_traces(colorscale="RdBu", reversescale=True)
            elif colorscale == "Cividis":
                fig.update_traces(colorscale="Cividis")
            else:
                fig.update_traces(colorscale="Viridis")
            fig.update_traces(ncontours=201,
                colorbar=dict(title=txtbar,tickformat=zformat),
                    hovertemplate="X: %{x:"+xformat+"}<br>Y: %{y:"+yformat+\
                        "}<br><b> %{z:"+zformat+"}</b><extra></extra>",
                contours=dict(coloring="fill", showlines=False)
            )
                
            #set aspect ratio
            if xunit == yunit:    #real
                if xrange['max']-xrange['min'] > yrange['max']-yrange['min']:
                    aspectratio=dict(x=np.asarray([4.,(xrange['max']-xrange['min'])/np.asarray(
                        [1.e-5,(yrange['max']-yrange['min'])]).max()]).min(),y=1)
                else:
                    aspectratio=dict(x=1,y=np.asarray([4,(yrange['max']-yrange['min'])/np.asarray(
                        [1.e-5,(xrange['max']-xrange['min'])]).max()]).min())
                aspectmode='manual'
            else:
                aspectratio=dict(x=1.25,y=1)
                aspectmode='cube'
            
            width=180+300*aspectratio['x']
            height=90+300*aspectratio['y']
            fig.update_layout(
                autosize=False,
                height=height,
                width=width,
                scene=dict(aspectmode=aspectmode),
                margin=dict(t=45,b=45,l=40,r=140),
                title=dict(text=txttop),
                annotations=[
                    dict(text=xlabel, x=0.5, y=-0.12, showarrow=False, xref="paper", 
                         yref="paper", font=dict(size=14)),
                    dict(text=txtbot,
                         font=dict(size=16, family="sans serif", color="#000000"),
                         x=0.1, y=0.0, ax=0, ay=0, xanchor="left", xshift=-65, yshift=-42, 
                         xref="paper", yref="paper") 
                ])
            if 'Lat' in xlabel or 'Lon' in xlabel:
                fig.update_layout(xaxis = dict(tickmode = 'linear',tick0 = 0,dtick = 30))
            if 'Lat' in ylabel or 'Lon' in ylabel:
                fig.update_layout(yaxis = dict(tickmode = 'linear',tick0 = 0,dtick = 30))        
            if self.plots[self.plottype]['cutV'] == 0.:
                fig.update_layout(
                    shapes=[
                        dict(type="circle", xref="x", yref="y", x0=-3, y0=-3, x1=3, y1=3, fillcolor="black", line_color="black"),
                        dict(type="circle", xref="x", yref="y", x0=-1, y0=-1, x1=1, y1=1, fillcolor="black", line_color="white"),
                        dict(type="path", path= self.ellipse_arc(N=30), fillcolor="white", line_color="white")
                    ])
            return fig
         
    
    def ellipse_arc(self, x_center=0, y_center=0, a=1, b=1, start_angle=-np.pi/2, end_angle=np.pi/2, N=100, closed= False):
        '''small function to overlay a partial ellipse on the plot for sun-lit half of earth effect'''
        t = np.linspace(start_angle, end_angle, N)
        x = x_center + a*np.cos(t)
        y = y_center + b*np.sin(t)
        path = f'M {x[0]}, {y[0]}'
        for k in range(1, len(t)):
            path += f'L{x[k]}, {y[k]}'
        if closed:
            path += ' Z'
        return path

