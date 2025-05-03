# Plot Predictions

Plot predictions uses marginaleffects to plot model predictions. The two main types of predictions offered are 'conditional' and 'marginal' predictions (Arel-Bundock, 2024). 

Warning: Please ensure that one of the 'conditional' or 'by' fields are empty. The two options are mutually exclusive. If you have entered values for both you can simply delete the entries for one with backspace and the plot will regenerate.


#### Conditional Predictions

Conditional predictions are those that are made on user specified values. For this app it is the covariates which are input into the 'Select Conditional Features' field. For these covariates, a grid of values are generated based on the data type. This is the mean for numeric and mode for categorical variables.

I have also offered a 'Advanced' option which allows you to more precisely specify how sequences for your conditional predictions are specified. This allows you to use non-default methods. e.g. for a numeric, you could specify a range such that: numeric1=200:220, numeric2=range, numeric3="minmax" Simply provide the values with a comma seperation, no escape characters required for strings.

You can specify a maximum of four variables for the conditional plots.

See: https://marginaleffects.com/bonus/plot.html for more details on passing specific conditions.


#### Marginal (By) Predictions

Marginal predictions will predict based on the average for the level of the factor/s passed into the function. You can pass a maximum of three variables for the marginal predictions plot.
