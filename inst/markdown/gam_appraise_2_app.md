With our changes we can see much improved appraisal plots for our model.

Most points on the QQ plot are now within the confidence interval and are very close to the expected distribution which indicates a valid model.

The residual vs linear predictor plot also shows no heteroskedasticity (although a potential outlier at x=-4?) or other concerning patterns. The lines which appear to form are expected due to the nature of the Poisson distribution which deals with count data so are not concerning.

The histogram of residuals is roughly normal and the tails are not too heavy which is a good result. The Poisson distribution can also cause some distortion around the center of the histogram so there is no need to be too concerned about the mode not being 0.

Finally, the observed vs fitted values follows a good trend which doesn't indicate any problems. As we are working with count data it common that the response and fitted values are much tighter at the lower counts before beginning to spread out at the less common higher counts.
