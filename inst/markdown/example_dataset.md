# Our Example

In this example we will be using the data from the Portal Project which uses baited traps in a experimental setup to get counts for various rodent species (Ernest, 2018).

The dataset contains the following variables:

time - Time step for given observation (e.g. 1, 2, ..., t)
series - factor indicator of the time series - maps to the target species
captures - total captures across all control plots
mintemp - monthly mean minimum temperature
ndvi\_ma12 - 12 month moving average of the mean Normalised Difference Vegetation Index

To learn more visit the [Portal Project](https://portal.weecology.org/) website or type ?mvgam::portal\_data to learn more.

The goal for this walkthrough is to build a GAM which can predict captures based on our other variables.
