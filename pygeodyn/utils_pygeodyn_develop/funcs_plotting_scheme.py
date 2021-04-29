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



def help_plot_colors():
    col1 = px.colors.qualitative.Plotly[0]
    col2 = px.colors.qualitative.Plotly[1]
    col3 = px.colors.qualitative.Plotly[2]
    return(col1, col2, col3)



def add_arc_background_w_text(fig, y_vals, arc_list, arc_text = True):
    '''
    Define the arc parameters for this run to be plotted as background 
    
    This function plots background panels to distinguish the arcs and optionally plots text.
    
    '''
   
    import datetime
    
    count_shade = 0
#     print(arc_list)
    
    for i_arc,val in enumerate(arc_list):
#         print(i_arc,val)
#         print(val[:6])

        arc_date_1 = np.datetime64(datetime.datetime.strptime(val[:6], '%y%m%d'))
        arc_date_2 = np.datetime64(datetime.datetime.strptime(val[:6], '%y%m%d') + datetime.timedelta(days=14))
        arc_date_3 = np.datetime64(datetime.datetime.strptime(val[:6], '%y%m%d') + datetime.timedelta(days=7))

        if count_shade == 0:
            color_bg = "LightSkyBlue"
            count_shade += 1

        elif count_shade == 1:
            color_bg = "LightSlateGrey" 
            count_shade=0 


        
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
        
        
        
        
        
        
#     #### Arc Background + Label ####
#     fig.add_vrect(
#         x0="2003-09-14", x1="2003-09-28",
#         fillcolor="LightSkyBlue", opacity=0.3,
#         layer="below", line_width=0,
#     )

#     # Create scatter trace of text labels
#     fig.add_trace(go.Scatter(
#         x=["2003-09-20"],
#         y=[y_vals],
#         text=["Arc 1",
#              ],
#         mode="text",
#         showlegend=False,

#     ))


#     #### Arc Background + Label ####
#     fig.add_vrect(
#         x0="2003-09-28", x1="2003-10-12",
#         fillcolor="LightSlateGrey", opacity=0.3,
#         layer="below", line_width=0,
#     )
#     # Create scatter trace of text labels
#     fig.add_trace(go.Scatter(
#         x=["2003-10-4"],
#         y=[y_vals],
#         text=["Arc 2",
#              ],
#         mode="text",
#         showlegend=False,
#     ))

#     # '031012_2wk', '031026_2wk'
#     #### Arc Background + Label ####
#     fig.add_vrect(
#         x0="2003-10-12", x1="2003-10-26",
#         fillcolor="LightSkyBlue", opacity=0.3,
#         layer="below", line_width=0,
#     )
#     # Create scatter trace of text labels
#     fig.add_trace(go.Scatter(
#         x=["2003-10-18"],
#         y=[y_vals],
#         text=["Arc 3",
#              ],
#         mode="text",
#         showlegend=False,
#     ))

#     # '031012_2wk', '031026_2wk'
#     #### Arc Background + Label ####
#     fig.add_vrect(
#         x0="2003-10-26", x1="2003-11-09",
#         fillcolor="LightSlateGrey", opacity=0.3,
#         layer="below", line_width=0,
#     )
#     # Create scatter trace of text labels
#     fig.add_trace(go.Scatter(
#         x=["2003-11-03"],
#         y=[y_vals],
#         text=["Arc 4",
#              ],
#         mode="text",
#         showlegend=False,
#     ))
    
# #  - Arc 2 --------  14 days ------ 03/09/28 - 03/10/12
# #  - Arc 3 --------  14 days ------ 03/10/12 - 03/10/26
# #  - Arc 4 --------  14 days ------ 03/10/26 - 03/11/09
# #  - Arc 5 --------  14 days ------ 03/11/09 - 03/11/23
# #  - Arc 6 --------  14 days ------ 03/11/23 - 03/12/07
# #  - Arc 7 --------  14 days ------ 03/12/07 - 03/12/21
# #  - Arc 8 --------  14 days ------ 03/12/21 - 04/01/5


#     fig.add_vrect(
#     x0="2003-11-09", x1="2003-11-23",
#     fillcolor="LightSkyBlue", opacity=0.3,
#     layer="below", line_width=0,
#     )
    
#     fig.add_vrect(
#     x0="2003-11-23", x1="2003-12-07",
#     fillcolor="LightSlateGrey", opacity=0.3,
#     layer="below", line_width=0,
#     )
    
#     fig.add_vrect(
#     x0="2003-12-07", x1="2003-12-21",
#     fillcolor="LightSkyBlue", opacity=0.3,
#     layer="below", line_width=0,
#     )
    return(fig)




def legend_as_annotation(fig):
    fig.add_annotation(
            x=1.05,
            y=.9,
            xref="x domain",
            yref="y domain",
            showarrow=False,
            text="MSISe86",
            font=dict(
                size=16,
                color="#ffffff"
                ),
            align="center",
            bordercolor="#c7c7c7",
            borderwidth=2,
            borderpad=4,
            bgcolor=px.colors.qualitative.Plotly[0],
            opacity=0.9
            )

    fig.add_annotation(
            x=1.05,
            y=.8,
            xref="x domain",
            yref="y domain",
            showarrow=False,
            text="MSISe00",
            font=dict(
                size=16,
                color="#ffffff"
                ),
            align="center",
            bordercolor="#c7c7c7",
            borderwidth=2,
            borderpad=4,
            bgcolor=px.colors.qualitative.Plotly[1],
            opacity=0.9
            )
    
    fig.add_annotation(
        x=1.05,
        y=.7,
        xref="x domain",
        yref="y domain",
        showarrow=False,
        text="MSISe2",
        font=dict(
            size=16,
            color="#ffffff"
            ),
        align="center",
        bordercolor="#c7c7c7",
        borderwidth=2,
        borderpad=4,
        bgcolor=px.colors.qualitative.Plotly[2],
        opacity=0.9
        )
    return(fig)
