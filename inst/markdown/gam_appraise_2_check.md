After increasing _k_ we can see large changes to the gam.check() output. The k-index has increased for all our terms, even those for which we didn't directly increase _k_. This is a good example of how changing _k_ is not as simple as it seems and that gam.check() is a heuristic rather then a hard and fast rule. Observing this set of results, it seems that there is still room for _k_ to increase on the s(time, by=series) term due to our low k-index, p-values and how close k' is to edf (particularly for the PB and PP factor levels).

Another interesting result is for the s(ndvi\_ma12) term. Although the k-index and p-value is low, we can see that the smooth penalty has shrunk the edf to a low value of 1. This indicates that the problem with this term may not be in it's _k_ value but elsewhere in the model. It is also important to note that the term was not statistically significant in summary() which may indicate that the term is not minimally impacting the model.

Although the s(mintemp) term is still has a small difference between k' and edf, it has the highest k-index and is not significant at the 5% level. Perhaps increasing _k_ further for s(time, by=series) will push it's output closer to that of s(ndvi\_ma12)?

Before making any further changes, we need to check the appraise() output to see if this model improved our QQ plot.
