from kamodo import Kamodo, kamodofy
from netCDF4 import Dataset
import numpy as np
#import scipy
from scipy.interpolate import RegularGridInterpolator
from datetime import datetime, timedelta, timezone
import time as ti


#variable name in file: [standardized variable name, descriptive term, units]
iri_varnames = {'Ne':['N_e','electron_density','1/m**3'], 
                'Te':['T_e','electron_temperature','K'],
                'Ti':['T_i','ion_temperature','K'], 
                'Tn':['T_n','neutral_temperature','K'],
                'O+':['N_Oplus','atomic_oxygen_ion_density','1/m**3'],
                'H+':['N_Hplus','atomic_hydrogen_ion_density','1/m**3'],
                'He+':['N_Heplus','atomic_helium_ion_density','1/m**3'],
                'O2+':['N_O2plus','molecular_oxygen_ion_density','1/m**3'],
                'NO+':['N_NOplus','nitric_oxide_ion_density','1/m**3'],
                'N+':['N_Nplus','atomic_nitrogen_ion_density','1/m**3'],
                'TEC':['TEC','total_electron_content','10**16/m**2'],
                'NmF2':['NmF2','max_electron_density','1/m**3'],
                'HmF2':['HmF2','max_electron_density_height','km']}



#times from file converted to seconds since midnight of filedate
#plotting input times will be datetime strings of format 'YYYY-MM-DD HH:mm:ss'
#filedate is self.filedate from iri object
#converts to hours since midnight of filedate for plotting
def dts_to_hrs(datetime_string, filedate):
    return (datetime.strptime(datetime_string, '%Y-%m-%d %H:%M:%S')-filedate).total_seconds()/3600.

class IRI(Kamodo):
    def __init__(self, filename, variables_requested = None, runname = "noname",
                 printfiles=False, filetimes=False, **kwargs): #                 time_index=None, time_seconds=None,
        # Prepare model for function registration for the input argument
        super(IRI, self).__init__(**kwargs)

        #collect filenames
        if '.2D.' in filename:  #require that input filename be for 3D file
            filename2d = filename
            f = filename.replace('.3D.', '.2D.')  #can't replace in place
            filename = f
        else: filename2d=filename.replace('.3D.','.2D.')
        self.filename = filename
        self.filename2d=filename2d
        if printfiles: print('opening IRI NetCDF files %s and %s' % (filename,filename2d))
        
        #establish time attributes first
        self._iri3D = Dataset(filename, 'r')
        self._time = np.array(self._iri3D.variables['time'])/60.  #convert to hours since midnight of file        
        self.filedate = datetime(int(filename[-10:-6]),1,1,0,0,0)+timedelta(days=int(filename[-6:-3])-1)
        self.datetimes=[(self.filedate+timedelta(hours=self._time[0])).isoformat(sep=' '), 
                        (self.filedate+timedelta(hours=self._time[-1])).isoformat(sep=' ')]  #strings
        self.filetimes=[datetime.timestamp(datetime.strptime(dt, '%Y-%m-%d %H:%M:%S').replace(\
            tzinfo=timezone.utc)) for dt in self.datetimes]   #timestamp in seconds, for value matching in wrapper?
        self.timerange={'min':self.datetimes[0], 'max':self.datetimes[1],
                            'n':len(self._time)}     #strings in format = YYYY-MM-DD HH:MM:SS 
        if filetimes: 
            return
                
        #collect data and make dimensional grid from 3D file
        self._iri2D = Dataset(filename2d, 'r')
        self._lon = np.array(self._iri3D.variables['lon'])
        self._lat = np.array(self._iri3D.variables['lat'])
        self._height = np.array(self._iri3D.variables['ht'])
        
        #store a few items in iri object
        self.missing_value = np.NAN
        self._registered = 0
        self.variables={}
        self.runname=runname

        #initialize domains of coordinate variables for plotting  
        self.lonrange={'min':0.,'max':360.,'n':73}
        self.latrange={'min':-90.,'max':90.,'n':37}
        self.htrange={'min':100.,'max':1000.,'n':46}  #height in km
        
        #set grid variables  
        self.gridSize=len(self._height)*len(self._lat)*len(self._lon)#*len(self._time)
        #self.gridMinDx=np.asarray([self._lon[1]-self._lon[0],
        #                           self._lat[1]-self._lat[0]]).min()
        self.dt = min(np.diff(self._time))
        self.dlat = min(np.diff(self._lat))
        self.dlon = min(np.diff(self._lon))
        self.dz = min(np.diff(self._height))
        
        #if variables_requested not given, collect all values from dict above as a list
        if variables_requested is None:
            variables_requested = [value[0] for key,value in iri_varnames.items()]
        #if variables_requested == ['H']: variables_requested = 'N_e'  #need a default for possible variables
        if printfiles: print(f'Requested {len(variables_requested)} variables: {variables_requested}')
            
        #collect list of iri variable name equivalents
        var_names = [key for key, value in iri_varnames.items() if value[0] in variables_requested]
        extra_variables = [var for var in variables_requested if var not in 
                     [value[0] for key, value in iri_varnames.items()]]
        if len(extra_variables)>0:   #pull out variables not allowed and error if not empty
            raise AttributeError("No such variable(s):{}".format(extra_variables))        
        
        #register each variable desired 
        for varname in var_names:
            #determine source file type for variable
            file_type=''
            if varname in self._iri3D.variables.keys(): file_type = '3D'
            elif varname in self._iri2D.variables.keys(): file_type = '2D'
            else:
                raise AttributeError(f"{varname} not found in the files' metadata.")           
                
            #set variables, units
            variable = getattr(self, '_iri'+file_type).variables[varname]#.__array__()  #set data        
            if (len(variable.shape) not in [3,4]): continue  #skip anything not 3D or 4D
            units = iri_varnames[varname][-1]  #units stored as last item in list per varname
            kamodo_varname = iri_varnames[varname][0]
            
            #register allowed 3D and 4D variables
            self.variables[kamodo_varname] = dict(units = units, data = variable)  #register in object
            if len(variable.shape) == 4:  #define and register interpolators for each
                self.register_4D_variable(units, np.array(variable), kamodo_varname)  #len(var.shape) instead of file_type
            elif len(variable.shape) == 3:
                self.register_3D_variable(units, np.array(variable), kamodo_varname)
        if printfiles: print(f'Successfully registered {self._registered} variables.')
                
        # Initialize interpolation values for plotting
        self.plots = dict()
        self.plottype = "" # empty default to force reinterpolation and regeneration
        self.cutVtext, self.cutV = 'Height', 300.   #height cut in km
        self.cutLtext, self.cutL = 'Lat [deg]', 0.
        self.nt, self.nX, self.nY, self.nZ = 1, 1, 37, 73   #t, height, lat, lon
        self.newx = self.cutV
        self.newy = np.linspace(-90., 90., self.nY)
        self.newz = np.linspace(0., 360., self.nX)
        self.newt = self._time[0]
        self.tunit, self.xunit, self.yunit, self.zunit ='s', 'km', 'deg', 'deg'
        self.tol = 1.1
        self.plots[self.plottype] = dict(cutVtext=self.cutVtext, cutV=self.cutV, 
                                         cutLtext=self.cutLtext, cutL=self.cutL,
                                         tol=self.tol, nt = self.nt, nX=self.nX, 
                                         nY=self.nY, nZ=self.nZ, newt=self.newt, 
                                         newx=self.newx, newy=self.newy, newz=self.newz)  
    
        # Get grids ready to use
        self.setup_interpolating_grids()
                
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
    def register_3D_variable(self, units, variable, varname):
        """Registers a 3d interpolator with 3d signature"""
        
        #define and register the interpolators
        xvec_dependencies = {'time':'s','lat':'deg','lon':'deg'}
        interpolator = self.define_3d_interpolators(
                                            units,variable,varname,self._time, 
                                            self._lat, self._lon)
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
    def register_4D_variable(self, units, variable, varname):
        """Registers a 4d interpolator with 4d signature"""
        
        #define and register the interpolators
        xvec_dependencies = {'time':'s','height':'km','lat':'deg','lon':'deg'}
        interpolator = self.define_4d_interpolators(
                                            units,variable,varname,self._time,
                                            self._height,self._lat,self._lon)
        self.register_interpolators(varname, interpolator, xvec_dependencies)
        return

    def register_interpolators(self, varname, interpolator, xvec_dependencies):
        '''Register interpolators for each variable'''
        
        self[varname] = interpolator
        self.variables[varname]['xvec'] = xvec_dependencies
        self._registered += 1  
        return    

    def setup_interpolating_grids(self):
        '''setup the grid to interpolate to, trim to necessary size of source grid, and compute interpolation weights'''

        tt, xx, yy, zz = np.meshgrid(self.newt,self.newx,self.newy,self.newz, indexing = 'xy')
        self.newgrid = np.array([np.ravel(tt), np.ravel(xx), np.ravel(yy), np.ravel(zz)]).T
        #print('Gridshape', self.newgrid.shape)
        self.plots[self.plottype]['newgrid'] = self.newgrid
        return

    def set_plot(self, plottype="XY", cutV=400., cutL=0, 
                 timerange={},
                 lonrange={'min':0.,'max':360.,'n':73},
                 latrange={'min':-90.,'max':90.,'n':37},
                 htrange={'min':100.,'max':1000.,'n':46}):
        '''
\nSet plotting variables for available preset plot types with syntax below.
Generic syntax for generating a plot (**kwargs not required):
    iri.set_plot(plottype, **kwargs)
    plot=iri.get_plot(variable_name, colorscale='Viridis')
    iplot(plot)
Example for LonLat (cutV is cut in km; time value plotted is min given):
    iri.set_plot('LonLat',cutV=400,latrange={'min':-90,'max':90,'n':181},
                   lonrange=dict('min'=0,'max'=360,'n'=361),
                   timerange={'min':'2015-03-20 07:50:25'})
Example for LonH (cutL is cut in latitude; time value plotted is min given):
    iri.set_plot('LonH',cutL=14,htrange=dict('min'=140,'max'=2000,'n'=181),
                   lonrange=dict('min'=0,'max'=360,'n'=361),
                   timerange={'min':'2015-03-20 07:50:25'})                
Example for LatH (cutL is cut in longitude; time value plotted is min given):
    iri.set_plot('LatH',cutL=14,htrange=dict('min'=140,'max'=2000,'n'=181),
                   latrange=dict('min'=-90,'max'=90,'n'=181),
                   timerange={'min':'2015-03-20 07:50:25'}) 
Example for TimeLon (cutV is in km, cutL is in latitude, time range given is plotted)
    iri.set_plot('TimeLon',cutV=400,cutL=50,lonrange=dict('min'=0,'max'=360,'n'=361),
                  timerange={min:'2015-03-20 01:50:25',max:'2015-03-20 02:50:25','n':100})                
Example for TimeLat (cutV is in km, cutL is in longitude, time range given is plotted)
    iri.set_plot('TimeLat',cutV=400,cutL=50,latrange=dict('min'=-90,'max'=90,'n'=181),
                  timerange={min:'2015-03-20 01:50:25',max:'2015-03-20 02:50:25','n':100})                              
Example for TimeH (cutV is in latitude, cutL is in longitude, time range given is plotted)
    iri.set_plot('TimeH',cutV=400,cutL=50,htrange=dict('min'=140,'max'=2000,'n'=100),
                  timerange={min:'2015-03-20 01:50:25',max:'2015-03-20 02:50:25','n':100})                 
        '''
        
        #set defaults if value(s) not given, esp for timerange
        if 'min' not in timerange.keys():
            timerange['min']=self.datetimes[0]   #keep as strings here
        else:  #check that given time is in range
            testmin = dts_to_hrs(timerange['min'], self.filedate)
            if testmin<0. or testmin>self._time[-1]:  #timestamp values
                timerange['min']=self.timerange['min']  #return min if value given not valid
                print('Given min time is not in file. Returning minimum time in file.')
        if 'max' not in timerange.keys():
            timerange['max']=self.datetimes[1]   #convert to timestamps later
        else:  #check that given time is in range
            testmax = dts_to_hrs(timerange['max'], self.filedate)
            if testmax<0. or testmax>self._time[-1]:  #timestamp values
                timerange['max']=self.timerange['max']  #return max if value given not valid
                print('Given max time is not in file. Returning maximum time in file.')
        if 'n' not in timerange.keys(): timerange['n']=self.timerange['n']
        if 'min' not in lonrange.keys(): lonrange['min']=self.lonrange['min']
        if 'max' not in lonrange.keys(): lonrange['max']=self.lonrange['max']
        if 'n' not in lonrange.keys(): lonrange['n']=self.lonrange['n']
        if 'min' not in latrange.keys(): latrange['min']=self.latrange['min']
        if 'max' not in latrange.keys(): latrange['max']=self.latrange['max']
        if 'n' not in latrange.keys(): latrange['n']=self.latrange['n']
        if 'min' not in htrange.keys(): htrange['min']=self.htrange['min']  
        if 'max' not in htrange.keys(): htrange['max']=self.htrange['max']
        if 'n' not in htrange.keys(): htrange['n']=self.htrange['n']
        tic = ti.perf_counter()  #start timer
        
        #compare previous values to new values to see if plot needs to be redone
        plotpar_listA = [self.plottype, self.cutV, self.cutL, self.lonrange,\
                         self.latrange, self.timerange, self.htrange]
        plotpar_listB = [plottype, cutV, cutL, lonrange, latrange, timerange,\
                         htrange]
        plotpar_comp = [0 if aa==bb else 1 for aa, bb in zip(plotpar_listA, plotpar_listB)]
        if sum(plotpar_comp)==0:
            print('Plottype and variables are unchanged, returning.')
            return
        self.timerange,self.lonrange,self.latrange,self.htrange, = \
            timerange,lonrange,latrange,htrange  #set new values if changed
        self.plottype, self.cutV, self.cutL = plottype, cutV, cutL

        #set arrays/constants for each plot type
        if plottype == 'LonLat':
            self.plottype = plottype
            self.cutVtext, self.cutV, self.newx = 'Height [km]', cutV, cutV
            self.nt, self.nX, self.nY, self.nZ = 1, 1, latrange['n'], lonrange['n']
            self.newt = dts_to_hrs(timerange['min'], self.filedate)
            self.newy = np.linspace(latrange['min'],latrange['max'],latrange['n'])
            self.newz = np.linspace(lonrange['min'],lonrange['max'],lonrange['n'])
        elif plottype == "LonH":
            self.plottype = plottype
            self.cutLtext, self.cutL, self.newy = 'Lat [deg]', cutL, cutL
            self.nt, self.nX, self.nY, self.nZ = 1, htrange['n'], 1, lonrange['n']
            self.newt = dts_to_hrs(timerange['min'], self.filedate)
            self.newx=np.linspace(htrange['min'],htrange['max'],htrange['n'])
            self.newz=np.linspace(lonrange['min'],lonrange['max'],lonrange['n'])
        elif plottype == "LatH":
            self.plottype = plottype
            self.cutLtext, self.cutL, self.newz = 'Lon [deg]', cutL, cutL
            self.nt, self.nX, self.nY, self.nZ = 1, htrange['n'], latrange['n'], 1
            self.newt = dts_to_hrs(timerange['min'], self.filedate)
            self.newx = np.linspace(htrange['min'],htrange['max'],htrange['n'])
            self.newy = np.linspace(latrange['min'],latrange['max'],latrange['n'])
        elif plottype == 'TimeLon':   #new
            self.plottype = plottype
            self.cutVtext = 'Height [km]'
            self.cutLtext, self.cutL, self.newy = 'Lat [deg]', cutL, cutL
            self.cutV, self.newx = cutV, cutV
            self.nt, self.nX, self.nY, self.nZ = timerange['n'], 1, 1, lonrange['n']
            self.newt = np.linspace(dts_to_hrs(timerange['min'], self.filedate), 
                                    dts_to_hrs(timerange['max'], self.filedate), 
                                    timerange['n'], dtype=float)
            self.newz = np.linspace(lonrange['min'],lonrange['max'],lonrange['n'])       
        elif plottype == 'TimeLat':   #new
            self.plottype = plottype
            self.cutVtext = 'Height [km]'
            self.cutLtext, self.cutL, self.newz = 'Lon [deg]', cutL, cutL
            self.cutV, self.newx = cutV, cutV
            self.nt, self.nX, self.nY, self.nZ = timerange['n'], 1, latrange['n'], 1
            self.newt = np.linspace(dts_to_hrs(timerange['min'], self.filedate), 
                                    dts_to_hrs(timerange['max'], self.filedate), 
                                    timerange['n'], dtype=float)
            self.newy = np.linspace(latrange['min'],latrange['max'],latrange['n'])  
        elif plottype == "TimeH":   #new
            self.plottype = plottype
            self.cutVtext, self.cutV, self.newy = 'Lat [deg]', cutV, cutV
            self.cutLtext, self.cutL, self.newz = 'Lon [deg]', cutL, cutL
            self.nt, self.nX, self.nY, self.nZ = timerange['n'], htrange['n'], 1, 1
            self.newt = np.linspace(dts_to_hrs(timerange['min'], self.filedate), 
                                    dts_to_hrs(timerange['max'], self.filedate), 
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
            self.plots[plottype][varname]=self[varname]
        toc = ti.perf_counter()  #end timer
        print(f"Time resetting plot and precomputing interpolations: {toc - tic:0.4f} seconds")
        return
    
    def get_plot(self, var, colorscale="Viridis",style="linear"):
        '''
        Return a plotly figure object for the available plot types set in set_plot()..
        colorscale = Viridis [default], Cividis, BlueRed or Rainbow
        '''
        print(f'set_plot::colorscale={colorscale}, style={style}')
        print(f'Run: {self.runname}')
        #Set some text strings
        txtbot=f"Model: IRI, {self.gridSize} volume cells, dt={self.dt} hrs, dlat={self.dlat} deg, "+\
            f"dlon={self.dlon} deg, dz={self.dz} km."
        if style == "linear":  txtbar=var + " [" + self.variables[var]['units'] + "]"
        if style == "log":  txtbar="log("+var + ") [" + self.variables[var]['units'] + "]"
        
        # Get values from interpolation already computed
        result=self[var](self.newgrid)

        #set variables specific to plot type
        if self.plottype=="LonLat":
            txttop=self.cutVtext+"=" + str(self.plots[self.plottype]['cutV']) + " slice,  Time = " \
                + self.timerange['min'] + 'UTC'
            xint, xunit, nx, xlabel = self.newz, self.zunit, self.nZ, 'Lon [deg]'
            yint, yunit, ny, ylabel = self.newy, self.yunit, self.nY, 'Lat [deg]'
            xformat, yformat, zformat, kamodo_plot = '.2f', '.2f', '.4g', 'XY'
        elif self.plottype == "LonH":    
            txttop=self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice,  Time = " \
                + self.timerange['min'] + 'UTC'
            xint, xunit, nx, xlabel = self.newz, self.zunit, self.nZ, 'Lon [deg]'
            yint, yunit, ny, ylabel = self.newx, self.xunit, self.nX, 'Height [km]'    
            xformat, yformat, zformat, kamodo_plot = '.2f', '.4g', '.4g', 'XZ'      
        elif self.plottype == 'LatH':
            txttop=self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice,  Time = " \
                + self.timerange['min'] + 'UTC'
            xint, xunit, nx, xlabel = self.newy, self.yunit, self.nY, 'Lat [deg]'
            yint, yunit, ny, ylabel = self.newx, self.xunit, self.nX, 'Height [km]'
            xformat, yformat, zformat, kamodo_plot = '.2f', '.4g', '.4g', 'YZ'
        elif self.plottype=="TimeLon":
            txttop=self.cutVtext+"=" + str(self.plots[self.plottype]['cutV']) + " slice, "+\
                self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice"
            xint, xunit, nx = self.newt, self.tunit, self.nt
            xlabel = 'Hours on '+self.filedate.isoformat(sep=' ')[0:10]+' [UTC]'
            yint, yunit, ny, ylabel = self.newz, self.zunit, self.nZ, 'Lon [deg]'
            xformat, yformat, zformat, kamodo_plot = '.2f', '.2f', '.4g', 'TY'
        elif self.plottype=="TimeLat":
            txttop=self.cutVtext+"=" + str(self.plots[self.plottype]['cutV']) + " slice, "+\
                self.cutLtext+"=" + str(self.plots[self.plottype]['cutL']) + " slice"
            xint, xunit, nx = self.newt, self.tunit, self.nt
            xlabel = 'Hours on '+self.filedate.isoformat(sep=' ')[0:10]+' [UTC]'
            yint, yunit, ny, ylabel = self.newy, self.yunit, self.nY, 'Lat [deg]'
            xformat, yformat, zformat, kamodo_plot = '.2f', '.2f', '.4g', 'TY'
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
                result2=np.reshape(result,(ny,nx))   #is this the problem with TimeLat?
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
