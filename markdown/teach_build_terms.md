# Formula Terms

### Formula Preview

The formula preview is shown here. Note that the response variable is not included so it just displays the covariate terms. Click on the 'Undo' button to remove the last term added (including interactions).

### Operators

This lets you add the standard R operators to your formula.

### Parametric Terms

This simple dropdown allows you to add parametric terms to your GAM. You can select a given covariate and TODO (number e.g. plant^2).

### Raw Formula

This allows you to paste in a raw text formula which will be passed to mgcv with as.formula(). If you do find the GUI cumbersome and are familiar with writing formulas for GAMs, then this will allow you to more quickly build your GAM. It also allows you to use features which are accepted by mgcv but are not provided in the GUI. However, these are not supported so expect janky behaviour.

### Smooth Terms

The smooth term tab allows for the construction of smooths for the GAM. The first item is the Smooth term itself which can be s, te, ti and t2 terms. s is the default for a smooth with te being a tensor smooth. ti produces a tensor product interaction rather then the full tensor product smooth given by te. t2 is an alternative for te that is useable with the gamm4 package (see mgcv doco for more details). If in doubt, use the default 's' option.

The covariates option allows for the selection of covariates to be used in the smooth. Supports the use of multiple covariates for a single smooth.


#### by

Allows smooths to 'interact' with factors or parametric terms. Must be a numeric or factor variable (the GUI should only offer these).

This allows for modeling of the following

$$E(y_i) = \beta_0 + f(x_i)z_i$$ (Wood, ????, p.84)

where _f_ is the smooth and $z_i$ a numeric. This ensures $z$ is multiplied by the smooth function. When using factor variables in the by argument, it is recommended that the factor variable is included in the parametric terms.

#### knots

The dimension of the basis for the smooth term. Set to -1 for the default value, which will likely need to be refined. See the knots section in the learn tab for more information. mgcv provides useful diagnostic tools to determine an appropriate k. It is important to remember that as _k_ determines the number of basis functions, increasing it will increase computational cost and the possibility of overfitting. If the smooth function you are trying to predict can be represented with a lower _k_, then it is ideal to do so. The tutorial in the appraisal section will provide information on how to do this. You can also read the _choose.k_ section in the mgcv doco.

#### Smooth Class

The smoothing basis to use. Defaults to 'tp' which is the Thin plate regression spline and should be appropriate for general use cases. See the help button, Smoothing Splines section in learn GAM page or the mgcv doco to get a full list of smooth classes.


