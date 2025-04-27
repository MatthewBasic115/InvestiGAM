# Generalized Linear Models (GLMs)

To more easily understand GAMs, it is useful to understand GLMs of which GAMs are an extension (Hastie & Tibshirani, 1986).

A standard linear model assumes that given a random variable Y (our response variable) conditional on X (our covariates) is normally distributed such that $Y|X \sim N(X \beta, \epsilon_\sigma^2)$. Where the mean is the predicted value and the variance is the variance on the error terms, which are normally distributed with a mean of 0 (White, 2018; Wood, 2017, p. 2).

If were were to predict the random variable Y_i drawn from the distribution defined above, we can define the expected value as $\mu_i \equiv E(Y_i) = E(Y|X) = X\beta$. We can then define the model $Y_i = \mu_i + \epsilon_i$ where $\epsilon_i \sim N(0, \epsilon_\sigma^2)$. The key constraint of this model is the reliance on the Normal distribution. Although the response variable itself does not need to belong to the Normal distribution, $Y|X$ does.

GLMs attempt to generalise the linear model such that $E(Y_i)$ can belong to members of the exponential family rather then solely the normal distribution. However, this introduces a problem. While the range of the linear predictor $X \beta$ is $(-\infty, \infty)$, some distributions in the exponential family do not support this. One example is Poisson which is typical used to model count data and supports $0,1,2,3...$. In order to ensure that $\mu_i$ is predicted on the correct scale a link function is required.


### Link Function

The link function $g$ is defined such that $g(\mu_i) = X_i\beta$ (Wood, 2017, p. 101). The link function chosen for a model then depends on the distribution family chosen for the GLM. For the purposes of this application, it is recommended that you use the canonical link function for a given distribution. This will be automatically selected when you choose your family when building your model. A table has also been provided in the accordion below for reference.


### Fitting

GLMs are fit by calculating maximum likelihood through iterative least squares (Wood, 2017, p. 102). This application supports different estimation methods for advanced users but recommends using a reasonable default that will be explained in the GAM section. As such, it is recommended that interested people read Section 3.1 of Simon Wood's textbook for details in theory.

