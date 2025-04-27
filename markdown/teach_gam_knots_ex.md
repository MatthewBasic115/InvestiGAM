# Example

This section adapts code provided by Wood (2017, p.165-166) for plotting the tent function basis.

You can select any number of knots between $k=2,k=3,...,k=9$ to see how the knots impact the function output.

Although the default value of 6 seems appropriate, when k=9 is selected, we can see an overfit model. This raises the question of how to select an appropriate value of k through regularisation. This requires the introduction of the regularisation hyperparameter $\lambda$ which will be explained in the next section.
