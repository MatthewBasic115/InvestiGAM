# Smoothing Splines

The earlier knot and basis function section provided a brief introduction to smooth functions and their construction in the context of a piecewise linear smoother (tent function basis). However in practice it is likely you will use smooth functions constructed from splines such as the default Thin plate regression splines which are the default for the s() smooth term. 

Similarly to the $\lambda$ estimation methods, a brief introduction to the different smoothing splines will be provided with considerbly less theory then introduced earlier. As always, the theory for the different smoothing splines can be found in Wood (2017). This is done in the interest of expediency so that a basic understanding of the different smooth basis can be attained and used in the application.

## Smoothing Splines

Explanation of the following smoothing splines are sourced from the mgcv doco in the smooth.terms sections (Wood, 2025) . Please read the documentation for the full description. This is not an exhaustive list, just common and powerful options offered by mgcv.

*Thin plate regression splines* bs="tp": The default smooth for s() terms as in some contexts they are the optimal smoother for any given basis dimension. These smoothers do not use knots in the traditional sense, so k determines the basis dimension. The option "ts" is a modification of "tp" which modifies the smooth penalty such that the whole term can be shrunk to 0.

*Duchon splines* bs="ds". Generalised thin plate splines.

*Cubic Regression Splines* bs="cr" Cubic spline basis splines. See chapter 5 of Wood (2017, p.195) for theory and an excellent explanation. Cubic spline basis is defined by knots evenly spaced over the covariaites. Penalised by the second derivative to control wiggliness. bs="cs" is the shrinkage version of "cr" and "cc" is the cyclic cubic regression spline.

*P-splines* bs="bs". B-spline basis with discrete penalty on basis coefficients. Useful in some non-standard applications where mixing different orders of basis and penalties is required. Outside of this, you will often be served better by the above splines.
