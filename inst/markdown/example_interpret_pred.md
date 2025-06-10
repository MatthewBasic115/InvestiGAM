The below plot is built using the plot\_predictions function from _marginaleffects_. This function can plot conditional and marginal estimates from the models predictors.

In the default example below you can see the predicted captures for each species based on the time predictor. The main lines represents the prediction at the given time on the x axes. The lighter band outside those lines represent the 95% confidence interval. 

In our example below, you can see how different species had different counts over time. By plotting the count for each species, we can quickly interpret results for our gam which are not visible from the summmary() method. One pattern which can be seen is what a potential seasonal trend for the PP species.

In this simple example, all predictors which are not specified are held at their mean for numerics or mode for factor variables. In the Interpret module you are given more advanced options to specify different scenarios to plot.

Go ahead and try different options and combinations in these plots. When doing this, please note that marginal and conditional estimates are mutually exclusive, so you can't input values for both fields. If both fields contain values, then the new plot won't generate until one is empty.
