Used to replicate a smooth for each factor level when a categorical variable is input.

From the _mgcv_ [doco](https://cran.r-project.org/web/packages/mgcv/mgcv.pdf#Rfn.s) (Wood, 2025):

"a numeric or factor variable of the same dimension as each covariate. In the
numeric vector case the elements multiply the smooth, evaluated at the corre-
sponding covariate values (a ‘varying coefficient model’ results). For the nu-
meric by variable case the resulting smooth is not usually subject to a centering
constraint (so the by variable should not be added as an additional main ef-
fect). In the factor by variable case a replicate of the smooth is produced for
each factor level (these smooths will be centered, so the factor usually needs to
be added as a main effect as well). See gam.models for further detail"
