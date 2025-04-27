# Basis Functions and Knots

In the context of GAMs, basis functions are the constituent functions of the smooth function _f_ and taken together form it's basis. We can define the $j^th$ basis function as $b_j(x)$ and then define _f_ as the following:
$$f(x) \sum_{j=1}^k b_j(x) \beta_j$$


Wood (2017) provides a simple example of a basis with polynomials. He defines a space of order 4 polynomials and below such that $b_1(x)=1, b_2(x)=x, b_3(x)=x^2, b_4(x)=x^3, b_5(x)=x^4$

Using these 5 basis functions, we can input them into the above formula to define _f_.

$$f(x)=\beta_1 + x\beta_2 + x^2 \beta_3 + x^3 \beta_4 + x^4 \beta_5$$

This defines _f_ using a polynomial basis. Although useful for demonstrating how individual basis functions can form a smooth, it would be possible to use this formula in a simple linear model. However, more complex basis functions can be defined.

### Tent Function Basis and Knots

Wood (2017) provides another using the tent function basis which implements a piecewise linear regression. This introduces the concept of _knots_ which in the context of the tent function basis, determine the meeting point of each of the piecewise linear functions. In the context of building GAMs in this application, knots are an important tool for building smooths as they provide some control over basis functions in a given smooth. Different smooth classes (basis) will use knots differently. Some such as the tent basis function or natural cublic splines determine where the values of basis functions will meet. For others such as the cyclic cubic regression spline, it determines the location at which the basis function will take a value (and 0 at all other knots) (Wood, 2017, p.203). In some cases such as the thin-plate regressioon spline there are no conventional knots but the knots argument can be provided to manipulate how the basis is calculated.

Although this tutorial will demonstrate the tent function basis to provide a conceptual understanding of knots and basis functions, users will need to be aware of how their chosen basis will use knots or the knots argument in the applicatioon. For practical uses, see the mgcv documentation, for theoretical understanding Simon Wood's (2017) textbook is recommended.

### Tent Function Basis Continued

Before providing the practical code example provided by Wood (2017), the definition of the knots for the tent function bbasis must first be provided.

Wood (2017, p.164) denotes the knots as ${x_j^* : j=1,...,k}$ with the constraint that $x_j^* > x_{j-1}^*$.

For $$b_1(x) = \begin{cases} (x_2^*-x)/(x_2^*-x_1^*) & x < x_2^* \\ 0, & \text{otherwise} \end{cases}$$

For $j=2,...,k-1$ 

$$b_j(x) = \begin{cases} (x-x_{j-1}^*) / (x_j^*-x_{j-1}^*), & x_{j-1}^* < x < x_j^* \\ (x_{i+j}^* -x) /(x_{j+1}^*-x_j^*), & x_j* < x < x_{j+1}^* \\ 0, & \text{otherwise} \end{cases}$$

Finally

$$b_k(x) = \begin{cases} (x-x_{k-1}^*) / (x_k^*-x_{k-1}^*), & x > x_{k-1}^* \\ 0, & \text{otherwise} \end{cases}$$

When plotted, you can see that each basis function seems to appear as a tent due too it being valued at 0 except for loocation and adjacent location of its knot. Move onto the next section for a practical example.
