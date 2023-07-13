# JAOPuTo
This package contains a number of functions to download, transform and visualize datasets from the JAO Publication Tool.
The JAO Publication Tool is used by the Core Transmission System Operators to publish, on a daily basis, a number of datasets related to day-ahead and intraday capacity calculation (and allocation), and can be accessed via https://publicationtool.jao.eu/core/.

The JAOPuTo package contains functions to download the following datasets:
- allocation constraints (for BE, NL and PL)
- the D2CF (two day-ahead congestion forecasts), including load, generation and net positions forecasts
- the bilateral exchange restrictions
- the final (presolved) domains
- intraday ATC and NTC values
- long-term allocated volumes
- maximum bilateral exchanges
- maximum Core import and export positions
- Core net positions
- price spreads on Core and non-Core borders
- scheduled exchanges between Core bidding zones
- active constraints and shadow prices

Additionally, a function to visualize the flow-based domain, returning a ggplot2 object and the corresponding dataframes, is included:

```{r include = FALSE}
JAOPuTo_domainvisualizations(as.POSIXct("2022-06-09 00:00", "CET"), "Belgium", "France", "MCP", TRUE)
```
