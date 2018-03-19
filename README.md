# What is this? 

This is an automated task assignment system I designed for our remote development team at [AlphaSheets](https://github.com/alphasheets-development/). This tool takes an [Airtable](https://airtable.com) project containing tasks and developers to be assigned to them, and computes a greedy schedule of task assignments, taking into account the following: 

* developer specialization (via tags like `frontend` or `backend` or `server-side rendering`)
* probabilistic dependencies between tasks (if I am assigned "Edit the config", there's a 50% chance I'll have to "Debug any issues with config" as well)  
* observed developer velocity (in story pts / sprint)

The tool infers marginal probabilities of task inclusion in a sprint by modeling the tasks as a bayes net, then samples a set of tasks and assigns all necessary dependencies to developers by greedy assignment. 

It then generates a Gantt chart like this: 

<a href="https://plot.ly/~anandtech1532/102/?share_key=YnKO7dsUNQ2TY9VWaa7p8i" target="_blank" title="marker-h-bar-YvIbIIx0" style="display: block; text-align: center;"><img src="https://plot.ly/~anandtech1532/102.png?share_key=YnKO7dsUNQ2TY9VWaa7p8i" alt="marker-h-bar-YvIbIIx0" style="max-width: 100%;width: 600px;"  width="600" onerror="this.onerror=null;this.src='https://plot.ly/404.png';" /></a>


We used this for a while to augment our sprint planning. Eventually we replaced this solution by just having more architecture meetings, which tended to get everyone on the same page better. For us it was a failed experiment, but you may find interesting use cases for this tool. 

Future work: support for automatically updating developer velocity estimates over time. 

## Pre-build 

In `Constants.hs`, set the following variables: 

```haskell
-- | Source Airtable Base from which to pull data (tasks, developers, etc.)
app_id :: String
app_id = undefined

-- | Destination Airtable Base to send schedule computation to
dash_app_id :: String
dash_app_id = undefined

-- | Airtable API key
api_key :: String
api_key = undefined
```

Ensure your plotly credentials are configured in `~/.plotly/`.

## Build 

From the project directory, do
```bash
stack install --fast
stack exec airtable-exe
```
