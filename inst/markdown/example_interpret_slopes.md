The below graphs use the _plot\_slopes()_ function from _marginaleffects_ to plot the slopes (i.e. partial derivative) for the given variable of interest. See the [plot\_slopes doco](https://marginaleffects.com/man/r/plot\_slopes.html) for more details. 

For this example, the _slope_ argument is always dydx, but you can use other options provided by _marginaleffects_ in the Interpret module which allows for the calculation of elasticity. Similar to plot\_predictions, variables not specified are held at their mean or mode depending on datatype.

In the example below, slopes on the original data were calculated and then averaged across all species. Then, the slope of the variable of interest (mintemp) is shown on the y axis. This shows how the average slope for mintemp changes for each species with confidence intervals.
