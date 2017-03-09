import plotly.plotly as py
import plotly.graph_objs as go
import numpy as np

import json
import sys

from shortid import ShortId

idgen = ShortId()

f_vis_name = sys.argv[1]

with open(f_vis_name + ".cache", 'r') as f_vis: 
    est = np.array(json.loads(f_vis.read()))

    dates       = est[:,0]
    est_lower   = est[:,1]
    est_middle  = est[:,2]
    est_upper   = est[:,3]

    trace1 = go.Scatter(
        x=dates,
        y=est_lower,
        fill='tonexty',
        mode='lines',
        line=dict(width=0.5, color='rgb(111, 231, 219)'),
        name='20% estimate'
    )
    trace2 = go.Scatter(
        x=dates,
        y=est_middle,
        fill='tonexty',
        mode='lines',
        line=dict(width=0.5, color='rgb(127, 166, 238)'),
        name='50% estimate'
    )
    trace3 = go.Scatter(
        x=dates,
        y=est_upper,
        fill='tonexty',
        mode='lines',
        line=dict(width=0.5, color='rgb(131, 90, 241)'),
        name='80% estimate'
    )

    data = [trace1, trace2, trace3]
    title = 'AlphaSheets completion estimates over time (' + idgen.generate() + ')'
    layout = go.Layout(
        showlegend=True,
        xaxis=dict(
            type='category',
        ),
        yaxis=dict(
            type='linear',
        ),
        title=title
    )
    fig = go.Figure(data=data, layout=layout)
    url = py.plot(fig, filename='stacked-area-plot', auto_open=False)

    with open(f_vis_name + ".url", 'w') as f_url:
        f_url.write(url)        
    