# What Now?

Based on the earlier result, we can see that while the 3rd model scored better, it was likely overfit. This shows the danger of relying too heavily on the k-index heuristic in gam.check() without checking the it's plotting component. From here you could continue to experiment with different values of _k_ for different terms. Perhaps the sweet spot for the s(time, by=series) term is in between 12 and 20? Or maybe it is time to experiment with other terms first? Maybe the output for s(ndvi\_ma12) in gam.check() indicates that there are some other problems with the model? 

For the purposes of this walkthrough we will stick with model 2. If you would like a challenge then you can try and build a better model in the main application!
