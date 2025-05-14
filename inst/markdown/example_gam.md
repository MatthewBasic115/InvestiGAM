# Building Our GAM

This page provides details on our GAM.

To build the GAM, we are using the _mgcv_ package (Wood, 2025) with it's gam() function.

`model_1 <- mgcv::gam(
  captures ~ 
    series + 
    s(time, by = series, k = 6) +
    s(ndvi_ma12, k = 4) +
    s(ndvi_ma12, series, k = 4, bs = 'sz') +
    s(mintemp, k = 4) +
    s(mintemp, series, k = 4, bs = 'sz'),
  family = poisson,
  data = portal_data
)`

For our formula, the response variable is captures which we are trying to predict. We can use series as a parametric factor term before adding our spline terms.

Then we use the s() spline term to build all our spline terms for the model.

The first term s(time, by=series, k=6) uses the default thin plate regression spline (ts) with the time variable to create it's smooth. As series is a factor variable, the by=series argument means that a replicate for the smooth is built for each factor level. Using series in the by argument is also why we want to add series in the parametric term(?). Finally, we have set k=6 to set the number of basis functions. In this example it has been set quite low to demonstrate how we can identify issues in the model.

Then there are two sets of similar terms where both ndvi\_ma12 and mintemp have a standard spline where k=4 and another which uses the 'sz' basis. The 'sz' basis is a factor smooth interactions basis which are similar to using the 'by' argument but are constraint to represent deviations from the main effect smooth (Wood, 2025). In our case smooths for both ndvi\_ma12 and mintemp are produced for each factor level of series.

Finally, the model is using the Poisson family which will default to a log link function. The Poisson distribution is useful as we are trying to predict the counts of different species conditional on our predictors.

Why did we do it this way?
