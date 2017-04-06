import plotly.plotly as py
import plotly.graph_objs as go
import numpy as np

import json
import sys

from shortid import ShortId
from dateutil.parser import parse
from dateutil.tz import tzutc
import datetime

idgen = ShortId()

f_vis_name = sys.argv[1]

thr_name = "UNSPECIFIED"
if (len(sys.argv) > 2):
    thr_name = sys.argv[2]

def to_unix_time(dt):
    epoch =  datetime.datetime.utcfromtimestamp(0).replace(tzinfo=tzutc())
    delta = (dt- epoch).total_seconds() * 1000
    return delta

with open(f_vis_name + ".cache", 'r') as f_vis: 
    my_id = idgen.generate()

    est = json.loads(f_vis.read())

    dates       = [parse(elem[0]) for elem in est]
    est_lower   = [elem[1] for elem in est]
    est_middle  = [elem[2] for elem in est]
    est_upper   = [elem[3] for elem in est]

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
        y=est_middle,
        text=[str(e) for e in est_middle],
        fill='tozeroy',
        mode='lines+markers+text',
        line=dict(width=0.5, color='rgb(111, 231, 219)'),
        name='Days to completion',
        textposition='top'
    )
    data = [trace]

    title = 'Completion estimate for {' + thr_name + '}, with 3x fudge factor'
    layout = go.Layout(
        showlegend=True,
        yaxis=dict(
            type='linear',
        ),
        title=title
    )
    fig = go.Figure(data=data, layout=layout)
    url = py.plot(fig, filename='stacked-area-plot-'+my_id, auto_open=False)

    with open(f_vis_name + ".url", 'w') as f_url:
        f_url.write(url)        
    