#### ----------------------------------------
#### Import modules:
#### -----------------
import numpy as np
import pandas as pd
    #### Computer function
import os
import os.path
import sys
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





def add_arc_background_w_text(fig, y_vals,  arc_date_1, arc_date_2, iarc, arc_text = False):
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
        fillcolor=color_bg, opacity=0.3,
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



def plot_residuals_observed(fig, obj_m1, plot_num):
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
        x_annot = 1.05
        y_annot = .97
        m_size = 2.5
    elif plot_num == 1:
        x_annot = 1.05
        y_annot = .8
        col = col2
        m_size = 2.5
    elif plot_num == 2:
        x_annot = 1.05
        y_annot = .55 
        col = col3    
        m_size = 2.5
        
#     print(arc_list)

    for i,  arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        i_arc = i+1
        
        str_run_param = 'run_parameters'+ arc
        final_iter = obj_m1.__dict__[str_run_param]['str_iteration']

        index_pce_x = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE X    '
        index_pce_y = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE Y    '
        index_pce_z = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE Z    '

        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_x][::15],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_x][::15]*1e2,
                                 name= 'PCE X',
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=m_size,),
                                 showlegend=False,
                                 ),
                                 row=1, col=1,
                                 )
        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_y][::15],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_y][::15]*1e2,
                                 name= 'PCE Y',
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=m_size,),
                                 showlegend=False,
                                 ),
                                 row=2, col=1,
                                 )
        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_z][::10],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_z][::10]*1e2,
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

        fig = add_arc_background_w_text(fig, 1.1*np.max(obj_m1.Residuals_obs[arc]['Residual'] ),arc_date_1, arc_date_2,i_arc, False)

        fig = legend_as_annotation(fig, obj_m1.__dict__['global_params']['den_model'], col, x_annot, y_annot)

    fig.update_layout(title="Observation Residuals")
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




# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## # 
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #



def Calc_Cd_percent_diff_apriori(obj_m1):
    '''
    CALCULATE:  
    '''

    SAT_ID = int(obj_m1.__dict__['global_params']['SATID'])
    which_stat = 'CURRENT_VALUE'
    model_m1 = obj_m1.__dict__['global_params']['den_model']

    all_cd_m1    = []
    all_dates_m1 = []

    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        i_arc = ii+1
#         print(obj_m1.AdjustedParams[arc])
        last_iter = list(obj_m1.AdjustedParams[arc].keys())[-1]
        labels = list(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'].keys())

        for i,val in enumerate(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'].keys()):
            all_cd_m1.append(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'][val][which_stat])
            all_dates_m1.append(labels[i])

    # take % difference from a priori
    cd_apriori= 2.2
    percdiff_cd_m1 = ((np.array(all_cd_m1) - cd_apriori)/ cd_apriori)*100

    obj_m1_stats = {}
    obj_m1_stats['cd_apriori'] = cd_apriori
    obj_m1_stats['cd_percdiff_from_apriori'] = percdiff_cd_m1
    obj_m1_stats['all_cd'] = all_cd_m1
    obj_m1_stats['all_dates'] = all_dates_m1

    return(obj_m1_stats)



def plot_cd_and_percdiff_from_apriori(fig, obj_m1, plot_num):

    obj_m1_stats = Calc_Cd_percent_diff_apriori(obj_m1)

    
    if plot_num == 0:
        col = col1
        x_annot = 1.05
        y_annot = .90
        m_size = 2
        
    elif plot_num == 1:
        x_annot = 1.05
        y_annot = .8
        col = col2
        m_size = 2
        
    elif plot_num == 2:
        x_annot = 1.05
        y_annot = .65 
        col = col3    
        m_size = 2
        
    SAT_ID = int(obj_m1.__dict__['global_params']['SATID'])
    which_stat = 'CURRENT_VALUE'


    model_m1 = obj_m1.__dict__['global_params']['den_model']
######     make plot:
#     fig = make_subplots(
#         rows=2, cols=1,
#         subplot_titles=(["Timeseries of Cd", "Percent difference from a priori (Cd=2.2)"]),
#         vertical_spacing = 0.1,
#         )

    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
#         print(arc)

        str_run_param = 'run_parameters'+ arc
        final_iter = obj_m1.__dict__[str_run_param]['str_iteration']

        i_arc = ii+1
        last_iter = list(obj_m1.AdjustedParams[arc].keys())[-1]
        time_dep_cd_dates = list(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'].keys())
        val_list_1 = []
        for i in obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'].keys():
            val_list_1.append(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'][i][which_stat])
        

        #### FIRST PLOT (CD TIMESERIES and APRIORI CD)
        fig.add_trace(go.Scattergl(x=time_dep_cd_dates,
                                   y=val_list_1,
                                   name= model_m1 ,#+ ' |  Arc ' +str(i_arc) +' | Iters: '+ str(last_iter),
                                   mode='markers',
                                   marker=dict(
                                   color=col,
                                   size=7,),
                                   showlegend=False,
                                   ),
                                   row=1, col=1,
                                   )
        fig.add_trace(go.Scattergl(x=time_dep_cd_dates,
                                   y=obj_m1_stats['cd_apriori']*np.ones(np.size(time_dep_cd_dates)),
                                   name= 'A priori Cd',
                                   mode='lines',
                                   marker=dict(
                                   color='black',
                                   size=1,),
                                   showlegend=False,
                                   ),
                                   row=1, col=1,
                                   )
        arc_date_1 = obj_m1.__dict__['global_params']['arc_input'][ii]
        
        arc_date_1 = datetime.strptime(arc_date_1, '%Y.%j')
        arc_date_2 = arc_date_1 + (pd.to_timedelta(24,'h'))
        fig = add_arc_background_w_text(fig, 2.2, arc_date_1, arc_date_2, i_arc, False)

    #### SECOND PLOT (PERC diff b/w apriori and Cd)
    fig = legend_as_annotation(fig, obj_m1.__dict__['global_params']['den_model'], col, x_annot, y_annot)

    fig.add_trace(go.Scattergl(x=obj_m1_stats['all_dates'],
                             y=obj_m1_stats['cd_percdiff_from_apriori'],
                            name= model_m1,
                            mode='markers',
                            marker=dict(
                            color=col,
                            size=7,
                            ),
                            showlegend=False,
                            ),
                            row=2, col=1,)


    # fix layout info:
    fig.update_yaxes( title="Cd ",exponentformat= 'power',row=1, col=1)
    fig.update_yaxes( title="% difference",exponentformat= 'power',row=2, col=1)
    fig.update_xaxes( title="Date", row=2, col=1)

    fig.update_layout(title="Time Dependent Drag Coefficient ")
    fig.update_layout(
                    autosize=False,
                    width=900,
                    height=900,
                    font=dict(size=14))

#     iplot(fig)
    return(fig)






def ARCOVERLAP_2arcs_ObsResids_XYZ(fig, obj_m1, plot_num, arc1, arc2):
    
 
    df1 = obj_m1.Residuals_obs[arc1]
    df2 = obj_m1.Residuals_obs[arc2]

    overlap_start =  df2['Date'].iloc[0]
    overlap_end   =  df1['Date'].iloc[-1]

    sliceoverlap_front = df1.Date[df1.Date < overlap_start].iloc[-1]
    indx_front_overlap = df1.Date[df1.Date==sliceoverlap_front].index.unique()[0]

    sliceoverlap_end = df2.Date[df2.Date > overlap_end].iloc[1]
    indx_end_overlap = df2.Date[df2.Date==sliceoverlap_end].index.unique()[0]

    df1_overlap = df1[indx_front_overlap:]
    df2_overlap = df2[:indx_end_overlap]

    model_m1 = obj_m1.__dict__['global_params']['den_model']

    if plot_num == 0:
        col = col1
        x_annot = 1.05
        y_annot = .97
        m_size = 1.5
    elif plot_num == 1:
        x_annot = 1.05
        y_annot = .8
        col = col2
        m_size = 1.5
    elif plot_num == 2:
        x_annot = 1.05
        y_annot = .55 
        col = col3    
        m_size = 1.5


    for i, arc in enumerate([arc1 , arc2]):
        i_arc = i+1

#         if i_arc == 1:
#             col = col1
#             x_annot = 1.05
#             y_annot = .97
#             m_size = 2.5

#         elif i_arc == 2:
#             x_annot = 1.05
#             y_annot = .8
#             col = col2
#             m_size = 2.5

        index_pce_x = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE X    '
        index_pce_y = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE Y    '
        index_pce_z = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE Z    '
        data_skip = 50
        ####--------------------- PCE X  ---------------------

        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_x][::data_skip],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_x][::data_skip]*1e2,
                                 name= 'Arc '+ str(i_arc),
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=m_size,),
                                 showlegend=False,
                                 ),
                                 secondary_y=False,
                                 row=1, col=1,
                                 )

        ####--------------------- PCE Y ---------------------

        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_y][::data_skip],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_y][::data_skip]*1e2,
                                 name= 'Arc '+ str(i_arc),
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=m_size,),
                                 showlegend=False,
                                 ),
                                 secondary_y=False,
                                 row=2, col=1,
                                 )

        ####--------------------- PCE Z ---------------------

        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_z][::data_skip],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_z][::data_skip]*1e2,
                                 name= 'Arc '+ str(i_arc),
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=m_size,),
                                 showlegend=False,
                                 ),
                                 secondary_y=False,
                                 row=3, col=1,
                                 )


    indexoverlap_pce_X_1 = df1_overlap['StatSatConfig'] == 'PCE X    '
    indexoverlap_pce_X_2 = df2_overlap['StatSatConfig'] == 'PCE X    '
    resids_X = pd.merge(left=df1_overlap[indexoverlap_pce_X_1], left_on='Date',
             right=df2_overlap[indexoverlap_pce_X_2], right_on='Date')# 
    residualoverlap_resid_X = resids_X['Residual_x'] - resids_X['Residual_y']
    fig.add_trace(go.Scattergl(x=resids_X['Date'],
                               y=residualoverlap_resid_X*1e2,
                               name= 'Arc '+ str(i_arc),
                               mode='markers',
                               marker=dict(color=col,
                               size=4,),
                               showlegend=False,
                               ),
                               secondary_y=True,
                               row=1, col=1,
                               )

    indexoverlap_pce_Y_1 = df1_overlap['StatSatConfig'] == 'PCE Y    '
    indexoverlap_pce_Y_2 = df2_overlap['StatSatConfig'] == 'PCE Y    '
    resids_y = pd.merge(left=df1_overlap[indexoverlap_pce_Y_1], left_on='Date',
             right=df2_overlap[indexoverlap_pce_Y_2], right_on='Date')# 
    residualoverlap_resid_Y = resids_y['Residual_x'] - resids_y['Residual_y']
    fig.add_trace(go.Scattergl(x=resids_y['Date'],
                               y=residualoverlap_resid_Y*1e2,
                               name= 'Arc '+ str(i_arc),
                               mode='markers',
                               marker=dict(color=col,
                               size=4,),
                               showlegend=False,
                               ),
                               secondary_y=True,
                               row=2, col=1,
                               )



    indexoverlap_pce_Z_1 = df1_overlap['StatSatConfig'] == 'PCE Z    '
    indexoverlap_pce_Z_2 = df2_overlap['StatSatConfig'] == 'PCE Z    '

    resids_Z = pd.merge(left  = df1_overlap[indexoverlap_pce_Z_1], left_on='Date',
                        right = df2_overlap[indexoverlap_pce_Z_2], right_on='Date')# 
    residualoverlap_resid_Z = resids_Z['Residual_x'] - resids_Z['Residual_y']
    fig.add_trace(go.Scattergl(x=resids_Z['Date'],
                               y=residualoverlap_resid_Z*1e2,
                               name= 'Arc '+ str(i_arc),
                               mode='markers',
                               marker=dict(color=col,
                               size=4,),
                               showlegend=False,
                               ),
                               secondary_y=True,
                               row=3, col=1,
                               )

    #     fig = legend_as_annotation(fig, obj_m1.__dict__['global_params']['den_model'], col, x_annot, y_annot)



    ### Start of second arc
    overlap_start =   obj_m1.__dict__['Residuals_obs'][arc2]['Date'].iloc[0] 
    ### End of first arc
    overlap_end = obj_m1.__dict__['Residuals_obs'][arc1]['Date'].iloc[-1]
#     print('overlap_start: ',overlap_start)
#     print('overlap_end: ',overlap_end)
#     color_overlap = "LightSlateGrey"
#     color_prediction = "LightSkyBlue"
    fig.add_vrect(  x0=overlap_start, x1=overlap_end,
                    fillcolor='LightSkyBlue', opacity=0.3,
                    layer="below", line_width=0)


    fig.update_xaxes(range=[overlap_start - pd.to_timedelta(2,'h'), overlap_end + pd.to_timedelta(2,'h')])


    fig.update_layout(title="Arc Overlap Analysis: Remove PCE from Observation Residuals" +'\n'+ '(Arc1 - Arc2)')
    fig.update_layout(
                    autosize=False,
                    width=800,
                    height=800,
                    font=dict(size=12),
                    legend= {'itemsizing': 'constant'})

    fig.update_yaxes( title="J2000 Cartesian (m)",exponentformat= 'power',row=1, col=1)
    fig.update_yaxes( title="J2000 Cartesian (m)",exponentformat= 'power',row=2, col=1)
    fig.update_yaxes( title="J2000 Cartesian (m)",exponentformat= 'power',row=3, col=1)
    fig.update_xaxes( title="Date", row=3, col=1)

    fig.update_yaxes(title_text="Residuals (cm)", row=1, col=1, secondary_y=True, color='SkyBlue')
    fig.update_yaxes(title_text="Residuals (cm)", row=2, col=1, secondary_y=True, color='SkyBlue')
    fig.update_yaxes(title_text="Residuals (cm)", row=3, col=1, secondary_y=True, color='SkyBlue')


    return(fig)

# fig.show(config=config)






def ARCOVERLAP_2arcs_ObsResids_RSW_radial(fig, obj_m1, plot_num, arc1, arc2):
    
    ###### GET THE PCE DATA:
    StateVector_epochs_datafile = '/data/data_geodyn/inputs/icesat2/setups/StateVector_epochs.txt'
    first_arc = arc1
    last_arc  = arc2
    first_arc_first_time = obj_m1.__dict__['Trajectory_orbfil'][first_arc]['data_record']['Date'].iloc[3],
    last_arc_last_time   = obj_m1.__dict__['Trajectory_orbfil'][last_arc]['data_record']['Date'].iloc[-2]
    first_arc_first_time_str =  str(first_arc_first_time[0])#.replace( "'",' ') 
    last_arc_last_time =  str(last_arc_last_time)#.replace( "'",' ') 

    with open(StateVector_epochs_datafile, 'r') as f:
        for line_no, line_text in enumerate(f):
            if first_arc_first_time_str in line_text:
                first_line = line_no
            elif last_arc_last_time in line_text:
                last_line = line_no
                break

    PCE_data = pd.read_csv(StateVector_epochs_datafile, 
                skiprows = first_line, 
                nrows=last_line- first_line,           
                sep = '\s+',
                dtype=str,
                names = [
                        'Date',
                        'X',
                        'Y',
                        'Z',
                        'X_dot',
                        'Y_dot',
                        'Z_dot',
                          ],)
    
    PCE_data['Date_pd'] = pd.to_datetime(PCE_data['Date'])

    orbfil_arc1 = obj_m1.__dict__['Trajectory_orbfil'][arc1]['data_record']
    orbfil_arc1['Date_pd'] = pd.to_datetime(orbfil_arc1 ['Date'])

    orbfil_arc2 = obj_m1.__dict__['Trajectory_orbfil'][arc2]['data_record']
    orbfil_arc2['Date_pd'] = pd.to_datetime(orbfil_arc2 ['Date'])

    
    
    C_1 = pd.merge(left=orbfil_arc1, left_on='Date_pd',
         right=PCE_data, right_on='Date_pd')
    C_2 = pd.merge(left=orbfil_arc2, left_on='Date_pd',
             right=PCE_data, right_on='Date_pd')

#     C_1['X_resid (PCE-orbfil)'] = C_1['X'].astype(float) - C_1['Satellite Inertial X coordinate'].astype(float)
#     C_2['X_resid (PCE-orbfil)'] = C_2['X'].astype(float) - C_2['Satellite Inertial X coordinate'].astype(float)

#     C_1['Y_resid (PCE-orbfil)'] = C_1['Y'].astype(float) - C_1['Satellite Inertial Y coordinate'].astype(float)
#     C_2['Y_resid (PCE-orbfil)'] = C_2['Y'].astype(float) - C_2['Satellite Inertial Y coordinate'].astype(float)

#     C_1['Z_resid (PCE-orbfil)'] = C_1['Z'].astype(float) - C_1['Satellite Inertial Z coordinate'].astype(float)
#     C_2['Z_resid (PCE-orbfil)'] = C_2['Z'].astype(float) - C_2['Satellite Inertial Z coordinate'].astype(float)

    # arc_date_1 =  pd.to_datetime( '181109-210109', format='%y%m%d-%H%M%S') 
    # arc_date_2 =  pd.to_datetime( '181110-024159', format='%y%m%d-%H%M%S') 
    
    

    def Convert_cartesian_to_RSW(state_vector):
        '''
        ###### The Satellite Coordinate System:
        ### often used to describe orbital errors, relative positions, and displacements of satellite orbits. 
        ### The RSW system moves with the satellite and is sometimes given the letters RTN (radial, transverse, and normal). 
        ### The R axis always points from the Earth’s center along the radius vector toward the satellite as it moves through the orbit. 
        ### The S axis points in the direction of (but not necessarily parallel to) the velocity vector and is perpendicular to the radius vector—an important distinction.
        ### The S axis is usually not aligned with the velocity vector except for circular orbits or for elliptical orbits at apogee and perigee. In

        ### Radial positions and displacements are parallel to the position vector (along the R axis).
        ### Along-track or transverse displacements are normal to the position vector (along the S axis).
        ### Finally, cross-track positions are normal to the plane defined by the current position and velocity vectors (along the W axis).


        ##### Radial component:
        ###    Radial positions and displacements are parallel
        ###    to the position vector (along the R axis).


        ##### Along track or Transverse
        ###   normal to the position vector (along the S axis)

        ##### Cross track
        ###   normal to the plane defined by the current 
        ###   position and velocity vectors (along the W axis)

        '''

        r_vec = state_vector[:3]    # np.array([X,   Y,   Z   ])
        v_vec = state_vector[-3:]    # np.array([Xdot,Ydot,Zdot])
        r_vec_norm = np.linalg.norm(r_vec)

        R_hat = r_vec/r_vec_norm
        W_hat = (np.cross(r_vec,v_vec)) / np.linalg.norm(np.cross(r_vec,v_vec))
        S_hat = np.cross(R_hat, W_hat)


        ###  
        ##         R_vec_ijk = T * R_vec_rsw
        ###
        ###    where, T = [ R_hat | S_hat | W_hat ]
        ###
        ###    so...
        ###
        ###        R_vec_rsw =   R_vec_ijk * T^-1
        transmat_RSW =  np.transpose(np.array([R_hat, W_hat, S_hat]))
        inverse_transmat_RSW = np.linalg.inv(transmat_RSW)


        r_vec_RSW = np.matmul(r_vec, inverse_transmat_RSW )
        r = r_vec_RSW[0]
        s = r_vec_RSW[1]
        w = r_vec_RSW[2]
        return r


    X = C_1['Satellite Inertial X coordinate']
    Y = C_1['Satellite Inertial Y coordinate']
    Z = C_1['Satellite Inertial Z coordinate']
    Xdot = C_1['Satellite Inertial X velocity']
    Ydot = C_1['Satellite Inertial Y velocity']
    Zdot = C_1['Satellite Inertial Z velocity']
    state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))

    Radial_comp_orbfil = [Convert_cartesian_to_RSW(x) for x in state_vector]

    X = C_1['X'].astype(float)
    Y = C_1['Y'].astype(float)
    Z = C_1['Z'].astype(float)
    Xdot = C_1['X_dot'].astype(float)
    Ydot = C_1['Y_dot'].astype(float)
    Zdot = C_1['Z_dot'].astype(float)
    state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))

    Radial_comp_PCE = [Convert_cartesian_to_RSW(x) for x in state_vector]
    
    model_m1 = obj_m1.__dict__['global_params']['den_model']

    if plot_num == 0:
        col = col1
        x_annot = 1.05
        y_annot = .97
        m_size = 2
    elif plot_num == 1:
        x_annot = 1.05
        y_annot = .8
        col = col2
        m_size = 2
    elif plot_num == 2:
        x_annot = 1.05
        y_annot = .55 
        col = col3    
        m_size = 2


#     for i, arc in enumerate([arc1 , arc2]):
#         i_arc = i+1
    data_skip = 7
    ####--------------------- Radial Component  ---------------------

    fig.add_trace(go.Scattergl(x=C_1['Date_pd'][::data_skip],
                             y=Radial_comp_orbfil[::data_skip],
                             name= 'Orbfil',
                             mode='markers',
                             marker=dict(color=col,
                             size=m_size,),
                             showlegend=False,
                             ),
                             secondary_y=False,
                             row=1, col=1,
                             )

    fig.add_trace(go.Scattergl(x=C_1['Date_pd'][::data_skip],
                             y=Radial_comp_PCE[::data_skip],
                             name= 'PCE',
                             mode='markers',
                             marker=dict(color=col, opacity=0.3,
                             size=m_size,),
                             showlegend=False,
                             ),
                             secondary_y=False,
                             row=1, col=1,
                             )        
    ####--------------------- Residual  ---------------------

    resid = np.array(Radial_comp_orbfil) - np.array(Radial_comp_PCE)
    
    fig.add_trace(go.Scattergl(x=C_1['Date_pd'][::data_skip],
                               y=resid[::data_skip],
                             name= '(PCE-orbfil)',
                             mode='markers',
                             marker=dict(color=col,
                             size=m_size,),
                             showlegend=False,
                             ),
                             secondary_y=False,
                             row=2, col=1,
                             )

    ### Start of second arc
    overlap_start = obj_m1.__dict__['Trajectory_orbfil'][arc2]['data_record']['Date'].iloc[0]
    ### End of first arc
    overlap_end   = obj_m1.__dict__['Trajectory_orbfil'][arc1]['data_record']['Date'].iloc[-1]
    fig.add_vrect(  x0=overlap_start, x1=overlap_end,
                    fillcolor='LightSkyBlue', opacity=0.2,
                    layer="below", line_width=0)

    fig.update_layout(title="Arc Overlap Analysis: Radial Component + Predicted Window (light blue window)")
    fig.update_layout(
                    autosize=False,
                    width=800,
                    height=600,
                    font=dict(size=12),
                    legend= {'itemsizing': 'constant'})

    fig.update_yaxes( title=" Radial (meters)",exponentformat= 'power',row=1, col=1)
    fig.update_yaxes( title=" residual (meters",       exponentformat= 'power',row=2, col=1)

    fig.update_xaxes( title="Date", row=2, col=1)

    fig.update_yaxes(title_text="Residuals (cm)", row=1, col=1, secondary_y=True, color='SkyBlue')


    return(fig)






def plot_residual_meas_summary(fig, obj_m1, plot_num):

    if plot_num == 0:
        col = col1
        x_annot = 1.05
        y_annot = .9
    elif plot_num == 1:
        x_annot = 1.05
        y_annot = .8
        col = col2
    elif plot_num == 2:
        x_annot = 1.05
        y_annot = .7 
        col = col3    
    mark_size = 10



    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        i_arc = ii+1

        arc_date = np.datetime64(datetime.strptime(arc, '%Y.%j') + (pd.to_timedelta(12,'h')))
        iter_index =  int(obj_m1.Residuals_summary[arc]['Iter'].values[-1])-1

        mean_arc = obj_m1.Residuals_summary[arc]['MEAN'].values.astype(float)[iter_index]*1e2
        rms_arc = obj_m1.Residuals_summary[arc]['RMS'].values.astype(float)[iter_index]

        fig.add_trace(go.Scattergl(x=[arc_date],
                                 y=[mean_arc],
                                 name= 'Arc: '+str(i_arc),
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=10,),
                                 showlegend=False,
                                 ),
                                  row=1, col=1,
                                 )
        fig.add_trace(go.Scattergl(x=[arc_date],
                             y=[rms_arc],
                             name= 'Arc: '+str(i_arc),
                             mode='markers',
                             marker=dict(color=col,
                             size=10,),
                             showlegend=False,
                             ),
                              row=2, col=1,
                             )


        arc_date_1 = obj_m1.Residuals_obs[arc]['Date'].iloc[0]
        arc_date_2 = obj_m1.Residuals_obs[arc]['Date'].iloc[-1]
        
        fig = add_arc_background_w_text(fig, 1.1*np.max(obj_m1.Residuals_obs[arc]['Residual'] ),arc_date_1, arc_date_2,i_arc, False)

    fig = legend_as_annotation(fig, obj_m1.__dict__['global_params']['den_model'], col, x_annot, y_annot)


    fig.update_layout(
        title="Residual Measurement Summary on Final Iteration",
        )
    fig.update_yaxes(title_text="Residual [cm]", row=1, col=1)
    fig.update_yaxes(title_text="RMS", row=2, col=1)
    fig.update_xaxes(title_text="Arc Dates", row=2, col=1)

    fig.update_layout(legend= {'itemsizing': 'constant'})
    fig.update_xaxes(tickangle=0)

    fig.update_layout(
        font=dict(size=15),
        autosize=False,
        width=  750,
        height= 750,
            )
    return(fig)




def rms_summary_table(Obj_list):
            ##### --------------------------------------------------
            ######      Add the mean overall RMS to the plot
            ###### --------------------------------------------------

#     print('+------------------+--------------------+--------------+')
#     print('|  Density Model   | Mean Residual (cm) |  RMS of Fit  |')
#     print('+------------------+--------------------+--------------+')

    col1_header = '   Density Model   ' 
    col2_header = '   Mean Residual (cm)    ' 
    col3_header = '   RMS of Fit   ' 
    
    col1_size = len(col1_header) 
    col2_size = len(col2_header) 
    col3_size = len(col3_header) 

    print('+','—'*col1_size,'+','—'*col2_size,'+','—'*col3_size, '+',sep = '')
    print('|    ', 'Summary Across all Arcs'.center(col1_size+col2_size+col3_size-2), '|',sep = '')
    print('+','—'*col1_size,'+','—'*col2_size,'+','—'*col3_size, '+',sep = '')
    print('+',col1_header,'+',col2_header, '+',col3_header, '+',sep = '')
    print('+','-'*col1_size,'+','-'*col2_size,'+','-'*col3_size, '+',sep = '')

    
    column_avg_rms_m1   = []
    column_avg_resid_m1 = []
    column_den_model    = []
    
    num_objs = np.size(Obj_list)
   
    for i_obj, val_obj in enumerate(Obj_list):
        avg_rms_m1 = []
        avg_resid_m1 = []
        for ii,arc in enumerate(val_obj.__dict__['global_params']['arc_input']):
            iter_index =  int(val_obj.Residuals_summary[arc]['Iter'].values[-1])-1

            mean_arc = val_obj.Residuals_summary[arc]['MEAN'].values.astype(float)[iter_index]*1e2
            rms_arc = val_obj.Residuals_summary[arc]['RMS'].values.astype(float)[iter_index]

            avg_rms_m1.append(rms_arc)
            avg_resid_m1.append(mean_arc)

        avg_rms_m1 =   "{:.5e}".format(np.mean(avg_rms_m1))  
        avg_resid_m1 = "{:.5e}".format(np.mean(avg_resid_m1)*1e2)

        den_model = val_obj.__dict__['global_params']['den_model']

        column_avg_rms_m1.append(avg_rms_m1)
        column_avg_resid_m1.append(avg_resid_m1)
        column_den_model.append(den_model)


        print('+', column_den_model[i_obj].center(col1_size-2),
              '+',  column_avg_resid_m1[i_obj].center(col2_size-2),
              '+', column_avg_rms_m1[i_obj].center(col3_size-2),'+')
    
    
    print('+','—'*col1_size,'+','—'*col2_size,'+','—'*col3_size, '+',sep = '')

    return
















def scale_density_with_cdadjustment(obj_m1):
    obj_m1_stats = Calc_Cd_percent_diff_apriori(obj_m1)
    cd_windows = obj_m1_stats['all_dates']
    cd_scaling = obj_m1_stats['cd_percdiff_from_apriori']/100
    
    save_dens  = []
    save_dates = []
    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        
        dates = obj_m1.Density[arc]['Date']
        dens  = obj_m1.Density[arc]['rho (kg/m**3)']

        for i,val in enumerate(cd_windows):
            
            window = np.logical_and(dates>cd_windows[i], dates < cd_windows[i] + timedelta(hours=9))  #hours=8))
            density_scaled = np.add( dens[window], np.multiply(dens[window],cd_scaling[i]))
            
            save_dates.append( dates[window])
            save_dens.append( density_scaled)
    return(save_dates, save_dens)



# def plot_ScaleDensity_with_CdScaleFactor__1(fig, obj_m1, plot_num, decimation):


#     model_m1 = obj_m1.__dict__['global_params']['den_model']
#     if plot_num == 0:
#         col = col1
#         x_annot = 1.05
#         y_annot = .9
#     elif plot_num == 1:
#         x_annot = 1.05
#         y_annot = .8
#         col = col2
#     elif plot_num == 2:
#         x_annot = 1.05
#         y_annot = .7 
#         col = col3

#     data_nums_1 = 1000
#     data_nums_2 = decimation
    
#     dates_scal, dens_scal = scale_density_with_cdadjustment(obj_m1)
#     obj_m1_stats = Calc_Cd_percent_diff_apriori(obj_m1)

# #         fig = make_subplots(rows=2, cols=1, 
# #                             specs=[
# #                             [{"secondary_y": False}],
# #                             [{"secondary_y": True}], ])                  



#     for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
#         str_run_param = 'run_parameters'+ arc
#         final_iter = obj_m1.__dict__[str_run_param]['str_iteration']
#         i_arc = ii+1
        
#         for i_plot, dens in enumerate(dens_scal):
#             fig.add_trace(go.Scattergl(x=dates_scal[i_plot][::data_nums_2],
#                                  y=dens_scal[i_plot][::data_nums_2],
#         #                          name = geodyn_run_params_m1['den_model'],
#                                  mode='markers',
#                                     marker=dict(color='limegreen',size=5),
#                                   showlegend=False,
#                                    ),
#                                    secondary_y=False,
#                                    row=1, col=1,
#                                    )
#         fig.add_trace(go.Scattergl(x=obj_m1.Density[arc]['Date'][::data_nums_2],
#                                      y=obj_m1.Density[arc]['rho (kg/m**3)'][::data_nums_2],
#             #                          name = geodyn_run_params_m1['den_model'],
#                                      mode='markers',
#                                         marker=dict(color='blue',
#                                         size=3,
#                                         opacity = .6,
#                                         ),
#                                       showlegend=False,
#                                        ),
#                                       secondary_y=False,
#                                        row=1, col=1,
#                                        )
#         arc_date_1 = obj_m1.Density[arc]['Date'].iloc[0]
#         arc_date_2 = obj_m1.Density[arc]['Date'].iloc[-1]
#         fig = add_arc_background_w_text(fig, 1.1*np.max(obj_m1.Residuals_obs[arc]['Residual'] ),
#                                             arc_date_1, arc_date_2, i_arc, False)

#     fig.add_trace(go.Scattergl(x=obj_m1_stats['all_dates'],
#                   y=obj_m1_stats['cd_percdiff_from_apriori']/100,
# #                     mode='lines+markers',
#                     mode='lines+markers',
#                             marker=dict(color=col3,
#                             size=4,
#                             ),
#                           showlegend=False,
#                            ),
#                           secondary_y=False,
#                            row=2, col=1,
#                            )


#     fig.update_layout(
#         title="Density along ICESat2's Orbit",
#         )
#     fig.update_yaxes(type="log", exponentformat= 'power', row=1, col=1)
#     fig.update_yaxes(type="linear", exponentformat= 'power', row=2, col=1)
#     fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
#     fig.update_yaxes(title_text="unitless", row=2, col=1)
#     fig.update_xaxes(title_text="Date", row=2, col=1)

#     fig.update_layout(
#         autosize=False,
#         width=900,
#         height=700,
#     )
#     fig.update_layout(
#         font=dict(
#             size=14,
#                  ),)
#     return(fig)



def plot_ScaleDensity_with_CdScaleFactor__2(fig, obj_m1, plot_num, decimation):


    model_m1 = obj_m1.__dict__['global_params']['den_model']
    if plot_num == 0:
        col = col1
        x_annot = 1.05
        y_annot = .9
    elif plot_num == 1:
        x_annot = 1.05
        y_annot = .8
        col = col2
    elif plot_num == 2:
        x_annot = 1.05
        y_annot = .7 
        col = col3

    data_nums_2 = decimation
    dates_scal, dens_scal = scale_density_with_cdadjustment(obj_m1)
    obj_m1_stats = Calc_Cd_percent_diff_apriori(obj_m1)


    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        str_run_param = 'run_parameters'+ arc
        final_iter = obj_m1.__dict__[str_run_param]['str_iteration']
        i_arc = ii+1
        
        fig.add_trace(go.Scattergl(  x=obj_m1.Density[arc]['Date'][::data_nums_2],
                                     y=obj_m1.Density[arc]['rho (kg/m**3)'][::data_nums_2],
                                     name = obj_m1.__dict__['global_params']['den_model'],
                                     mode='markers',
                                        marker=dict(color=col,
                                        size=4,
                                        ),
                                      showlegend=False,
                                       ),
                                      secondary_y=False,
                                       row=1, col=1,
                                       )
        
        
        for i_plot, dens in enumerate(dens_scal):
            fig.add_trace(go.Scattergl(x = dates_scal[i_plot][::data_nums_2],
                                       y = dens_scal[i_plot][::data_nums_2],
                                name = 'Scaled '+ obj_m1.__dict__['global_params']['den_model'],
                                 mode='markers',
                                    marker=dict(color=col,size=4),
                                  showlegend=False,
                                   ),
                                   secondary_y=False,
                                   row=2, col=1,
                                   )

        
        arc_date_1 = obj_m1.Density[arc]['Date'].iloc[0]
        arc_date_2 = obj_m1.Density[arc]['Date'].iloc[-1]
        
        fig = add_arc_background_w_text(fig, np.max(obj_m1.Density[arc]['Date']),
                                            arc_date_1, arc_date_2, i_arc, False)


    fig.update_layout(
        title="Density along ICESat2's Orbit",
        )
    fig.update_yaxes(type="log", exponentformat= 'power', row=1, col=1)
    fig.update_yaxes(type="log", exponentformat= 'power', row=2, col=1)

    fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
    fig.update_yaxes(title_text="kg/m^3", row=2, col=1)
    fig.update_xaxes(title_text="Date", row=2, col=1)

    fig.update_layout(
        autosize=False,
        width=900,
        height=700,
    )
    fig.update_layout(
        font=dict(
            size=14,
                 ),)
    return(fig)








