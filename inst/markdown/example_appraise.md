# Appraisal

Let's appraise our model. These are the outputs you would find in the Appraise tab in the module selection menu.

*Summary* 

First, open the Summary section. Here you can see the output of summary(model) which is similar to the summary output you would get for linear models. You can see the selected family, link function and the formula used to generate the GAM. Estimates and p-values for parametric terms can be interpreted as you would for any other parametric model (e.g. linear). Below this are the estimates for the smooth terms. These can be harder to interpret due to the wiggliness and complexity of smooths. However, the summary does provide important information around degrees of freedom, the chi-squared test statistic and p-values.

In this example, we can see that every smooth term is statistically significant well below the standard $\alpha=0.05$ tolerance. As we are using the Poisson family, we know our scale parameter $\phi = 1$, so we are given the Un-biased Risk Estimator (UBRE) which we can use to compare models. When comparing models we want to aim for the lowest possible UBRE value (minimum of 0). Simply using the UBRE to select our model will not be sufficient as we still need to ensure that our model assumptions are accurate. 
We also need to determine whether we our selection of _k_ for our smooth terms is appropriate. While summary() provides the edf and Ref.df columns which can help with this, the gam.check() function in the next section is a more useful tool.

