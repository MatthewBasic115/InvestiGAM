# Appraisal

Let's appraise our model. These are the outputs you would find in the Appraise tab in the module selection menu.

*Summary* 

First, open the Summary section. Here you can see the output of summary(model) which is similar to the summary output you would get for linear models. You can see the selected family, link function and the formula used to generate the GAM. Estimates and p-values for parametric terms can be interpreted as you would for any other parametric model (e.g. linear). Below this are the estimates for the smooth terms. These can be harder to interpret due to the wiggliness and complexity of smooths. However, the summary does provide important information around degrees of freedom, the chi-squared test statistic and p-values.

In this example, we can see that every smooth term is statistically significant well below the standard $\alpha=0.05$ tolerance. As we are using the Poisson family, we know our scale parameter $\phi = 1$, so we are given the Un-biased Risk Estimator (UBRE) which we can use to compare models. When comparing models we want to aim for the lowest possible UBRE value (minimum of 0). Simply using the UBRE to select our model will not be sufficient as we still need to ensure that our model assumptions are accurate. 
We also need to determine whether we our selection of _k_ for our smooth terms is appropriate. While summary() provides the edf and Ref.df columns which can help with this, the gam.check() function in the next section is a more useful tool.

*gam.check()*

gam.check() is a function provided by _mgcv_ to help users determine an appropriate value for _k_ in their smooth terms.The edf column shows the effective degrees of freedom which must be smaller then k'. If edf is close to k' then the smooth is using most of the degrees of freedom that is provided by the smooths _k_ value. If the edf is well below k', then the smooth penalty is reducing the wiggliness of the smooth such that the higher _k_ value has little impact. The k-index and it's accompaying p-value, is a heuristic that indicates if _k_ is too low for a given smooth. As stated in the gam.check() output if the k-index is low (<1) then _k_ may be too small. In our example we can see the k-index is well below 1 for each term. Not only that, but our edf is quite close to k' for most terms. This indicates that we should increase _k_ and see if it improves the model.

gam.check() also provides some useful plots to evaluate our GAM. InvestiGAM uses the appraise() function from _gratia_ to plot these. Let's check these plots in the next section before changing our model.

*Appraise*

The appraisal section provides plots to check if our GAM is valid. Observing the QQ plot we can see a problem with our model. In the lower bounds we can see points well below the theoretical line and outside our confidence interval. This indicates that something is wrong with our model. On the next page, we'll increase _k_ on some of our terms to see if that improves our model.
