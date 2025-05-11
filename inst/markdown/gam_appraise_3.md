# Too Far?

For this next model, k has been increased to 20 for the s(time, by=series) term. Observing the result in summary() we can see that the UBRE has decreased again to 0.18 and the deviance explained increased to 93.4%. All terms are now statistically significant.

*gam.check()*

In gam.check() the k-index has now increased enough for most terms such that the p-values are no longer below the threshold. Similarly to the last model the s(ndvi_ma12) term still has a low p-value despite not using any of the additional degrees of freedom. It seems like this model is what we need!

*Appraisal*

Despite the great results from the summary() and gam.check() we can see issues once we check the appraisal plots. Unlike the previous result the QQ plot now shows a significant number of points outside the confidence interval and drifting from the theoretical trend. The residuals vs linear predictor plot shows an increase in the outlier from ~-4 to ~-25 which also makes it more difficult to read the plot. However, the histogram and observed vs fitted results are not so bad.

These plots seem to indicate that the model 3 has been overfit. The tightening of the tails in the residual histogram and lower UBRE indicate that the model has improved at predicting which supports an overfit result. The perfect 0 deviance for the outlier in the residual vs linear predictor plot indicates that the GAM used the additional degrees of freedom to perfectly fit the outlier rather then the overall pattern. Finally, the worse results on the QQ plot indicates that the model is reducing its loss at the cost of better general results.
