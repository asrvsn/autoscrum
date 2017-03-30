import plotly.plotly as py
import plotly.graph_objs as go
import numpy as np

import json
import sys

from shortid import ShortId
from dateutil.parser import parse
import datetime

idgen = ShortId()

f_vis_name = sys.argv[1]

def to_unix_time(dt):
    print dt
    epoch =  datetime.datetime.utcfromtimestamp(0)
    return (dt - epoch).total_seconds() * 1000

with open(f_vis_name + ".cache", 'r') as f_vis: 
    est = np.array(json.loads(f_vis.read()))

    dates       = [parse(d) for d in est[:,0]]
    est_lower   = est[:,1]
    est_middle  = est[:,2]
    est_upper   = est[:,3]

    # FIXME anand: I just removed the other traces since we don't care 
    # about the confidence interval

    # trace1 = go.Scatter(
    #     x=dates,
    #     y=est_lower,
    #     fill='tonexty',
    #     mode='lines',
    #     line=dict(width=0.5, color='rgb(111, 231, 219)'),
    #     name='20% estimate'
    # )
    # trace2 = go.Scatter(
    #     x=dates,
    #     y=est_middle,
    #     fill='tonexty',
    #     mode='lines',
    #     line=dict(width=0.5, color='rgb(127, 166, 238)'),
    #     name='50% estimate'
    # )
    # trace3 = go.Scatter(
    #     x=dates,
    #     y=est_upper,
    #     fill='tonexty',
    #     mode='lines',
    #     line=dict(width=0.5, color='rgb(131, 90, 241)'),
    #     name='80% estimate'
    # )

    # data = [trace1, trace2, trace3]

    trace = go.Scatter(
        x=dates,
        y=est_lower,
        fill='tonexty',
        mode='lines',
        line=dict(width=0.5, color='rgb(111, 231, 219)'),
        name='Dev completion time (days)'
    )
    data = [trace]

    title = 'AlphaSheets completion estimates over time (' + idgen.generate() + ')'
    layout = go.Layout(
        showlegend=True,
        xaxis=dict(
            type='category',
            range=[to_unix_time(dates[0]), to_unix_time(dates[-1])]
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
    