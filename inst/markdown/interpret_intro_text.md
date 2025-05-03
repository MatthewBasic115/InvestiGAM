This tab uses the marginaleffects package to help you create plots for interpreting your GAM.

There are two families of plots in this menu. The first two use the gratia (Simpson, 2024) package to plot the partial effects of smooths and their basis functions. These are designed to allow you to quickly visualise your smooths and the basis functions that build them. It is important to note that the basic smooth plotting occurs on the *link* scale and is not impacted by the menu option at the top of the page.

If you want to plot on the response scale, use the Predictions tab and ensure that 'response' is selected in the dropdown.

There are multiple different types of plots you can use for different scenarios. These plots have common features which will be briefly explained now.

The first is the 'conditional' variable/s, this specifies variables which will have data generated for them in a datagrid.

This allows the plot to perform operations such as predictions for scenarios which are not present in the training dataset.

The second is the 'by' input. This input takes factor (categorical) variables and averages them per factor level. This is performed on the original dataset (NOT generated data).

You cannot use both the factor and condition inputs for the same plot unless otherwise specified.
