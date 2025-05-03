# Interpret Introduction

The interpret tab allows you to quickly generate plots using the marginaleffects (Arel-Bundock, 2024) package to interpret your GAM. Each section uses a different feature from marginaleffects and allows for very quick interpretation results for your model. Across all plots there are some common features.

### Basic Smooth and Basis Function plots

The first two plotting sections provide use gratia (Simpson, 2024) to draw basic plots for your GAMs smooths and their basis functions. The first offers the partial effect of smooths (link scale) and can give a quick visualisation for a given smooth. To plot on the response scale or for more powerful predictions, use the predictions section. 

The second plotting section allows you to visualise the basis functions for a given smooth. This can be useful to provide a lower level view of a given smooth. See the GAM section in the learn page to learn about basis functions and how they are defined.

### General Plotting Options

The 'rug' and 'points' options shows observed values for your data. Plotting these can make the graph less readable, so make sure you try turning these options off if you are having difficulty reading it.

When plotting you can choose to plot on the link or response scale. In general, it is recommended that you plot on the response scale as it provides more intuitive results. Especially in cases such as the Gamma() distribution, the transformation done by the log link may be non-linear, which may make interpretation for practical outcomes more difficult (Clark, 2024).

### 'By' and 'Conditional'

When using marginaleffect plots you will be given inputs for both 'marginal' and 'conditional' predictions. 'marginal' is for factor (categorical) predictors when you want the average prediction for each factor level. 'conditional' predictions are made on a grid of user values. The two arguments are mutually exclusive, so you will get an error if you input values for both of them. Simply clear one of the fields and your plot will regenerate. In some cases, an 'advanced' panel which provides a text input for specifying the functions or values used to generate a data sequence for given variables.
