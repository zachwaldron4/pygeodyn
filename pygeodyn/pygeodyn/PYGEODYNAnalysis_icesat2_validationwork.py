#### ----------------------------------------
#### Import modules:
#### -----------------
import numpy as np
import pandas as pd
    #### Computer function
import os
import os.path
import subprocess
import shutil
import time
    #### modules for reading and converting data
import linecache
from datetime import datetime,timedelta
import copy


#### ----------------------------------------
#### Plotting modules:
#### -----------------
import plotly.graph_objects as go
from plotly.offline import plot, iplot
from plotly.subplots import make_subplots
import plotly.express as px
#### ----------------------------------------
#### ----------------------------------------
#### ----------------------------------------

# Simplify Plotting Schemes:
col1 = px.colors.qualitative.Plotly[0]
col2 = px.colors.qualitative.Plotly[1]
col3 = px.colors.qualitative.Plotly[2]
col4 = px.colors.qualitative.Plotly[3]
col5 = px.colors.qualitative.Plotly[4]
col6 = px.colors.qualitative.Plotly[5]


import sys
sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/')
from common_functions          import Convert_cartesian_to_RSW, Convert_cartesian_to_NTW_getT



def add_arc_background_w_text(fig, y_vals,  arc_date_1, arc_date_2, iarc, opacity_val, arc_text = False):
    '''
    Define the arc parameters for this run to be plotted as background 
    
    This function plots background panels to distinguish the arcs and optionally plots text.
    
    '''
    import datetime

    if iarc % 2 == 0:
        color_bg = "LightSkyBlue"
    else:
        color_bg = "LightSlateGrey" 



    #### Arc Background + Label ####
    fig.add_vrect(
        x0=arc_date_1, x1=arc_date_2,
        fillcolor=color_bg, opacity=opacity_val,
        layer="below", line_width=0,
    )

    if arc_text == True:
        # Create scatter trace of text labels
        fig.add_trace(go.Scatter(
            x=[arc_date_3],
            y=[y_vals],
            text=["Arc "+ str(i_arc+1) ,
                 ],
            mode="text",
            showlegend=False,
            ))

    return(fig)




def legend_as_annotation(fig, den_model_string, color_it, x_annot, y_annot):
    fig.add_annotation(
            x=x_annot,
            y=y_annot,
            xref="x domain",
            yref="y domain",
            showarrow=False,
            text=den_model_string,
            font=dict(
                size=16,
                color="#ffffff"
                ),
            align="center",
            bordercolor="#c7c7c7",
            borderwidth=2,
            borderpad=4,
            bgcolor=color_it,
            opacity=0.9
            )

    return(fig)




def STATS_residuals(residuals,measurement_type):
    import numpy as np
    n = np.size(residuals)
    mean = (1/n)*(np.sum(residuals))
    variance = (1/n)*(np.sum(np.square(residuals)))
    rms = np.sqrt(variance)
    rms_about_zero = np.sqrt((n/(n-1))*variance)
    
                
    print('mean            ',measurement_type ,':',mean)
#     print('variance ',measurement_type ,':',variance)
    print('rms             ',measurement_type ,':',rms)
    print('rms about zero  ',measurement_type ,':',rms_about_zero)
    print()
    
    return(mean,rms,rms_about_zero)





def add_stats_annotation(fig, text_in, col, x_annot, y_annot):
    fig.add_annotation(
            x=x_annot,
            y=y_annot,
            xref="x domain",
            yref="y domain",
            showarrow=False,
            text=text_in,
            font=dict(
                size=13,
                color="#ffffff"
                ),
            align="center",
            bordercolor="#c7c7c7",
            borderwidth=2,
            borderpad=4,
            bgcolor=col,
            opacity=0.9
            )
    return(fig)


# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## # 
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #


def PLOT__intrack_residuals_w_rms_and_CDRatio(fig, obj_m1, plotname, plot_num):
    
    if plot_num == 0:
        col = col1
        x_annot = 1.09
        y_annot1 = 1
        y_annot2 = .9
        m_size = 4
    elif plot_num == 1:
        x_annot = 1.09
        y_annot1 = .8
        y_annot2 = .7
        col = col2
        m_size = 3.5
    elif plot_num == 2:
        x_annot = 1.09
        y_annot1 = .5
        y_annot2 = .5
        col = col3
        m_size = 3.5
    elif plot_num == 3:
        x_annot = 1.09
        y_annot1 = .3
        y_annot2 = .3
        col = col4
        m_size = 3.5
    elif plot_num == 4:
        x_annot = 1.09
        y_annot1 = .1
        y_annot2 = .1
        col = col5
        m_size = 3.5

    import pandas as pd
    import sys
    sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/')
    from common_functions          import Convert_cartesian_to_RSW, Convert_cartesian_to_NTW_getT


    ###### GET THE PCE DATA:
    StateVector_PCE_datafile = '/data/data_geodyn/inputs/icesat2/setups/PCE_ascii.txt'
    SAT_ID = int(obj_m1.__dict__['global_params']['SATID'])
    which_stat = 'CURRENT_VALUE'

    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):#[::2]):
        print(arc)

        ####--------------------- Residual  ---------------------

        arc_first_time  = obj_m1.__dict__['Trajectory_orbfil'][arc]['data_record']['Date_UTC'].iloc[0]
        arc_last_time   = obj_m1.__dict__['Trajectory_orbfil'][arc]['data_record']['Date_UTC'].iloc[-2]

        arc_first_time_str     =  str(arc_first_time)#.replace( "'",' ') 
        arc_last_time_str      =  str(arc_last_time)#.replace( "'",' ') 


        A=[]
        for i,val in enumerate(np.arange(-20,20)):
            A.append(str(pd.to_datetime(arc_first_time)+pd.to_timedelta(val,'s')))
        B=[]
        for i,val in enumerate(np.arange(-20,20)):
            B.append(str(pd.to_datetime(arc_last_time)+pd.to_timedelta(val,'s')))

        ####---------------------------------------------------------
        last_line = False
        with open(StateVector_PCE_datafile, 'r') as f:
            for line_no, line_text in enumerate(f):
                if any(times in line_text for times in A):
                    first_line = line_no
                if any(times in line_text for times in B):
                    last_line = line_no
                    break

            if not last_line:
                last_line = first_line +32220
                print('No matching lastline time: ',arc_last_time_str, last_line )

        ####   IF YOU GET AN ERROR HERE stating that either first_line or last_line is 
        ####    It is probably an issue with the date in the arc not matching up with the dates given in the PCEfile
        PCE_data = pd.read_csv(StateVector_PCE_datafile, 
                    skiprows = first_line, 
                    nrows=last_line-first_line,           
                    sep = '\s+',
                    dtype=str,
                    names = [
                            'Date',
                            'MJDSECs', 
                            'RSECS', #(fractional secs)
                            'GPS offset', # (UTC - GPS offset (secs))
                            'X',
                            'Y',
                            'Z',
                            'X_dot',
                            'Y_dot',
                            'Z_dot',
                            'YYMMDDhhmmss',
                                ],)

        PCE_data['Date_pd'] = pd.to_datetime(PCE_data['Date'])
        orbfil_arc1 = obj_m1.__dict__['Trajectory_orbfil'][arc]['data_record']
        orbfil_arc1['Date_pd'] = pd.to_datetime(orbfil_arc1 ['Date_UTC'])


        ### C_1 is a dataframe containing all data between the two files where the dates match
        C_1 = pd.merge(left=orbfil_arc1, left_on='Date_pd',
             right=PCE_data, right_on='Date_pd')


        ### Convert the ORBIT FILE data to NTW
        X = C_1['Satellite Inertial X coordinate']
        Y = C_1['Satellite Inertial Y coordinate']
        Z = C_1['Satellite Inertial Z coordinate']
        Xdot = C_1['Satellite Inertial X velocity']
        Ydot = C_1['Satellite Inertial Y velocity']
        Zdot = C_1['Satellite Inertial Z velocity']
        state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))
        InTrack_comp_orbfil = [Convert_cartesian_to_NTW_getT(x) for x in state_vector]


        ### Convert the PCE data to NTW
        X = C_1['X'].astype(float)
        Y = C_1['Y'].astype(float)
        Z = C_1['Z'].astype(float)
        Xdot = C_1['X_dot'].astype(float)
        Ydot = C_1['Y_dot'].astype(float)
        Zdot = C_1['Z_dot'].astype(float)
        state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))
        InTrack_comp_PCE = [Convert_cartesian_to_NTW_getT(x) for x in state_vector]

        resid_T = (np.array(InTrack_comp_PCE) - np.array(InTrack_comp_orbfil))




        fig.add_trace(go.Scattergl(x=C_1['Date_pd'],
                                 y=resid_T,
                             name= plotname,
                             mode='markers',
                             marker=dict(color=col,
                             size=m_size,),
                             showlegend=False,
                             ),
                             secondary_y=False,
                             row=1, col=1,
                             )

        (mean,rms,rms_about_zero) = STATS_residuals(resid_T, 'in-track')

        fig = add_stats_annotation(fig, plotname+'<br>Mean='+ str(np.round(mean,4))+'<br>RMS='+ str(np.round(rms_about_zero,4)), col , x_annot, y_annot1)
#         fig = add_stats_annotation(fig, ' , col , x_annot, y_annot2)

    fig.update_yaxes( title="Residuals (m)",exponentformat= 'power',row=1, col=1)
    fig.update_xaxes( title="Date", row=1, col=1)


    return(fig)




# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #



def PLOT__intrack_residuals_w_rms(fig, obj_m1, plotname, plot_num):
    
    if plot_num == 0:
        col = col1
        x_annot = 1.09
        y_annot1 = 1
        y_annot2 = .9
        m_size = 3.5
    elif plot_num == 1:
        x_annot = 1.09
        y_annot1 = .8
        y_annot2 = .7
        col = col2
        m_size = 3.5
    elif plot_num == 2:
        x_annot = 1.09
        y_annot1 = .5
        y_annot2 = .5
        col = col3
        m_size = 3.5
    elif plot_num == 3:
        x_annot = 1.09
        y_annot1 = .2
        y_annot2 = .3
        col = col4
        m_size = 3.5
    elif plot_num == 4:
        x_annot = 1.09
        y_annot1 =  0 
        y_annot2 = .1
        col = col5
        m_size = 3.5


    import pandas as pd
    import sys
    sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/')
    from common_functions          import Convert_cartesian_to_RSW, Convert_cartesian_to_NTW_getT


    ###### GET THE PCE DATA:
    StateVector_PCE_datafile = '/data/data_geodyn/inputs/icesat2/setups/PCE_ascii.txt'
    SAT_ID = int(obj_m1.__dict__['global_params']['SATID'])
    which_stat = 'CURRENT_VALUE'

    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):#[::2]):
        print(arc)

        ####--------------------- Residual  ---------------------

        arc_first_time  = obj_m1.__dict__['Trajectory_orbfil'][arc]['data_record']['Date_UTC'].iloc[0]
        arc_last_time   = obj_m1.__dict__['Trajectory_orbfil'][arc]['data_record']['Date_UTC'].iloc[-2]

        arc_first_time_str     =  str(arc_first_time)#.replace( "'",' ') 
        arc_last_time_str      =  str(arc_last_time)#.replace( "'",' ') 


        A=[]
        for i,val in enumerate(np.arange(-20,20)):
            A.append(str(pd.to_datetime(arc_first_time)+pd.to_timedelta(val,'s')))
        B=[]
        for i,val in enumerate(np.arange(-20,20)):
            B.append(str(pd.to_datetime(arc_last_time)+pd.to_timedelta(val,'s')))

        ####---------------------------------------------------------
        last_line = False
        with open(StateVector_PCE_datafile, 'r') as f:
            for line_no, line_text in enumerate(f):
                if any(times in line_text for times in A):
                    first_line = line_no
                if any(times in line_text for times in B):
                    last_line = line_no
                    break

            if not last_line:
                last_line = first_line +32220
                print('No matching lastline time: ',arc_last_time_str, last_line )

        ####   IF YOU GET AN ERROR HERE stating that either first_line or last_line is 
        ####    It is probably an issue with the date in the arc not matching up with the dates given in the PCEfile
        PCE_data = pd.read_csv(StateVector_PCE_datafile, 
                    skiprows = first_line, 
                    nrows=last_line-first_line,           
                    sep = '\s+',
                    dtype=str,
                    names = [
                            'Date',
                            'MJDSECs', 
                            'RSECS', #(fractional secs)
                            'GPS offset', # (UTC - GPS offset (secs))
                            'X',
                            'Y',
                            'Z',
                            'X_dot',
                            'Y_dot',
                            'Z_dot',
                            'YYMMDDhhmmss',
                                ],)

        PCE_data['Date_pd'] = pd.to_datetime(PCE_data['Date'])
        orbfil_arc1 = obj_m1.__dict__['Trajectory_orbfil'][arc]['data_record']
        orbfil_arc1['Date_pd'] = pd.to_datetime(orbfil_arc1 ['Date_UTC'])


        ### C_1 is a dataframe containing all data between the two files where the dates match
        C_1 = pd.merge(left=orbfil_arc1, left_on='Date_pd',
             right=PCE_data, right_on='Date_pd')


        ### Convert the ORBIT FILE data to NTW
        X = C_1['Satellite Inertial X coordinate']
        Y = C_1['Satellite Inertial Y coordinate']
        Z = C_1['Satellite Inertial Z coordinate']
        Xdot = C_1['Satellite Inertial X velocity']
        Ydot = C_1['Satellite Inertial Y velocity']
        Zdot = C_1['Satellite Inertial Z velocity']
        state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))
        InTrack_comp_orbfil = [Convert_cartesian_to_NTW_getT(x) for x in state_vector]


        ### Convert the PCE data to NTW
        X = C_1['X'].astype(float)
        Y = C_1['Y'].astype(float)
        Z = C_1['Z'].astype(float)
        Xdot = C_1['X_dot'].astype(float)
        Ydot = C_1['Y_dot'].astype(float)
        Zdot = C_1['Z_dot'].astype(float)
        state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))
        InTrack_comp_PCE = [Convert_cartesian_to_NTW_getT(x) for x in state_vector]

        resid_T = (np.array(InTrack_comp_PCE) - np.array(InTrack_comp_orbfil))




        fig.add_trace(go.Scattergl(x=C_1['Date_pd'],
                                 y=resid_T,
                             name= plotname,
                             mode='markers',
                             marker=dict(color=col,
                             size=m_size,),
                             showlegend=True,
                             ),
                             secondary_y=False,
                             row=1, col=1,
                             )

        (mean,rms,rms_about_zero) = STATS_residuals(resid_T, 'in-track')

        fig = add_stats_annotation(fig, plotname+'<br>Mean='+ str(np.round(mean,4))+'<br>RMS='+ str(np.round(rms_about_zero,4)), col , x_annot, y_annot1)
#         fig = add_stats_annotation(fig, ' , col , x_annot, y_annot2)

    fig.update_yaxes( title="Residuals (m)",exponentformat= 'power',row=1, col=1)
    fig.update_xaxes( title="Date", row=1, col=1)


    return(fig)




# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #




def PLOT__residuals_observed(fig, obj_m1, plot_num):
            # obj_m1 = Obj_Geodyn
            # fig = make_subplots(rows=3, cols=1, 
            #         subplot_titles=(['PCE X', 'PCE Y', 'PCE Z']),
            #         vertical_spacing = 0.1,
            #                     )
#     arc_list = [obj_m1.__dict__['run_parameters']['ARC']]
    model_m1 = obj_m1.__dict__['global_params']['den_model']
#     iter_list = [ obj_m1.__dict__['run_parameters']['str_iteration']]

    if plot_num == 0:
        col = col1
        x_annot = 1.09
        y_annot = .90
        m_size = 4
    elif plot_num == 1:
        x_annot = 1.09
        y_annot = .7
        col = col2
        m_size = 3.5
    elif plot_num == 2:
        x_annot = 1.09
        y_annot = .5
        col = col3
        m_size = 2.5
    elif plot_num == 3:
        x_annot = 1.09
        y_annot = .3
        col = col4
        m_size = 1.5
    elif plot_num == 4:
        x_annot = 1.09
        y_annot = .1
        col = col5
        m_size = 1
        
#     print(arc_list)
    data_skip = 40
    for i,  arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        i_arc = i+1
        
        str_run_param = 'run_parameters'+ arc
        final_iter = obj_m1.__dict__[str_run_param]['str_iteration']

        index_pce_x = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE X    '
        index_pce_y = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE Y    '
        index_pce_z = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE Z    '

        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_x][::data_skip],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_x][::data_skip]*1e2,
                                 name= 'PCE X',
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=m_size,),
                                 showlegend=False,
                                 ),
                                 row=1, col=1,
                                 )
        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_y][::data_skip],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_y][::data_skip]*1e2,
                                 name= 'PCE Y',
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=m_size,),
                                 showlegend=False,
                                 ),
                                 row=2, col=1,
                                 )
        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_z][::data_skip],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_z][::data_skip]*1e2,
                                 name= 'PCE Z',
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=m_size,),
                                 showlegend=False,
                                 ),
                                 row=3, col=1,
                                 )
        arc_date_1 = obj_m1.Residuals_obs[arc]['Date'].iloc[0]
        arc_date_2 = obj_m1.Residuals_obs[arc]['Date'].iloc[-1]

        fig = add_arc_background_w_text(fig, 1.1*np.max(obj_m1.Residuals_obs[arc]['Residual'] ),arc_date_1, arc_date_2,i_arc, 0.1, False)

        fig = legend_as_annotation(fig, obj_m1.__dict__['global_params']['den_model'], col, x_annot, y_annot)

#     fig.update_layout(title="Observation Residuals (T.O.R.)")
    fig.update_layout(
                    autosize=False,
                    width=900,
                    height=900,
                    font=dict(size=14),
                    legend= {'itemsizing': 'constant'})

    fig.update_yaxes( title="Residuals (cm)",exponentformat= 'power',row=1, col=1)
    fig.update_yaxes( title="Residuals (cm)",exponentformat= 'power',row=2, col=1)
    fig.update_yaxes( title="Residuals (cm)",exponentformat= 'power',row=3, col=1)
    fig.update_xaxes( title="Date", row=3, col=1)
    return(fig)



























