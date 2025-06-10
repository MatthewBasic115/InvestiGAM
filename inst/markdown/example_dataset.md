# Our Example

In this example we will be using the data from the Portal Project which uses baited traps in a experimental setup to get counts for various rodent species (Ernest, 2018).

The dataset contains the following variables:

_time_ - Time step for given observation (e.g. 1, 2, ..., t)

_series_ - factor indicator of the time series - maps to the different species. Different species have different populations and may have different reactions to the other predictors in this dataset.

This dataset has four species. Ord's kangaroo rat (DO), Bailey's pocket mouse (PB), Merriam's kangaroo rat (DM) and the Desert pocket mouse (PP).

_captures_ - total captures across all control plots

_mintemp_ - monthly mean minimum temperature. It is expected that the rodent species in this dataset will be less active during the winter.

_ndvi\_ma12_ - 12 month moving average of the mean Normalised Difference Vegetation Index - which is a measure of how dense the vegetation is in an area.

To learn more visit the [Portal Project](https://portal.weecology.org/) website or type ?mvgam::portal\_data to learn more.

The goal for this walkthrough is to build a GAM which can predict captures based on our other variables.
