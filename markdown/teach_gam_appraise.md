# Appraising GAMs

The Appraisal tab provides a quick way to appraise the effectiveness of the your GAM. It uses three functions split across three sections to do this.

Similar to a standard GLM or linear model, there are assumptions that should be validated.

#### Summary

The summary section provides the summary() output that is common to statistical models in R. It provides information on model hyperparameters and provides summary statistics for both smooth terms and parametric coefficients. It also provides the formula which you can copy and paste into the raw formula if you want to quickly regenerate your model.

#### gam.check()

This section uses mgcv's gam.check() to provide diagnostic information for the model. gam.check() provides a heuristic check to see if the value of _k_ is appropriate. This is a useful tool but is not a guarentee that _k_ is correct based solely on this output.

k' may not align exactly with _k_ depending on the spline selected. In some cases splines have a 'intercept' which may remove one or two knots from k'.

You will want to be looking at the k-index for your model. Values below 1 may indicate that your model requires a higher value of _k_. This is particularly true if your effective degrees of freedom (edf) is close to _k_. Remember to recheck. It is not uncommon that changing one smooths _k_ value can impact anothers significance, so try andd iteratively improve until all smooths are acceptable.

Gav Simpsons video 1:48h into the vid. Consider implementing his code.

Time series of spatial data - might be modelling autocorrelation. Be careful there.

Take residuals from the model. Then redo model with them as the response. They should be gaussian for most models. Refit with the smoothers, if they detect non-linearity then you have have extra wiggliness you are missing.

#### Appraisal Plots

Appraisal plots use the gratia appraise() function to generate appraisal plots for the current model. The 'simulate' option will use the method="simulate" argument to give confidence intervals to the QQ plot. This is useful for model evaluation as points lying outside the confidence bands may indicate an issue with the model. These plots are implemented to be the same as the plots you would get in gam.check(). 

*QQ Plot:* You should look for unexpected results when evaluating QQ plots for a typical linear model or GLM. If you use the 'simulate' option, looking for points outside the confidence interval may indicate a problem. It is expected that you will have random variation in the QQ plot.

*Residuals vs Linear Predictor:* Ensure you check for heteroskedascity in the model. You should be expecting constant variance across all of the graph. For some count data (e.g. when using Poisson distribution) you may see a pattern (looks like a line or arc) emerge for low counts. This is acceptable so long as a constant variance is generally kept elsewhere.

*Histogram:* The Histogram of residuals should be roughly normal. Be aware when using the Poisson or Binomial distributions as valid models may not follow a normal distribution for this graph.

*Response vs Fitted Values:* In this graph you are looking for a rough 1-1 for observed vs fitted data. Similarly to other graphs the Poisson distribution may cluster at low counts before spreading out at higher values. This is acceptable so long as it seems to follow a general trend.
