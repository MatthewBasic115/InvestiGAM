# Plot Comparisons

Similar to both predictions and slopes, the comparisons section allows for plot conditional or marginal comparisons in relation to a variable of interest. Comparisons provides more options then predictions and slopes as custom comparisons can be specified for the variable of interest rather then just the conditional variables as per the predictions plots.

*Datagrid Generation*

Datagrid generation allows advanced users to manually build a marginaleffects datagrid. See datagrid in the marginaleffects doco for more information. In this section you can specify the grid type which can take the following values (Arel-Bundock et al. 2024):

    "mean_or_mode": Character, factor, logical, and binary variables are set to their modes. Numeric, integer, and other variables are set to their means.

    "balanced": Each unique level of character, factor, logical, and binary variables are preserved. Numeric, integer, and other variables are set to their means. Warning: When there are many variables and many levels per variable, a balanced grid can be very large. In those cases, it is better to use grid_type=“mean_or_mode” and to specify the unique levels of a subset of named variables explicitly.

    "counterfactual": the entire dataset is duplicated for each combination of the variable values specified in …. Variables not explicitly supplied to datagrid() are set to their observed values in the original dataset.

To specify values or methods for specific variables, you use the raw text input. To do this, simply type in the variables and their generation function or raw values. For example:

> conc=200:500, treatment=unique, type=unique

You do not need to quote variable or function names for datagrid generation (unless specifying a string) however you will need to for other sections. Once you have specified a datagrid it will be used for *all* comparisons plots until you clear it.

*Basic Conditional and Marginal Comparisons*

Two basic and easy to use plots are provided first. These use the same inputs as prior basic plots where generated data is calculated at the mean or mode based on the data type. These plots are particularly useful if you have provided a datagrid and do not require any further customisation. The advanced tabs are much more powerful and are what allows for custom comparisons.

*Advanced Conditional and Marginal Plots*

The advanced plot panels allow you to specify the values at which to perform the comparison. For example, you may want to compare outcomes between different factor levels or different values for continuous values. 

Only one of the raw text input or drop down menu will be used with the text field taking priority. If you wish to use the dropdown menu then please ensure the text field is empty. The dropdown allows you to provide variables for the plot in the same way as the basic plots. For example, if you only want to provide detailed information for the condition input, then you can simply select the variables of interest in the dropdown.

The raw text input allows for a much wider variety of inputs. You will need to quote variable names when building the string. An example is provided below:

> "conc"=200:1000, "treatment"=unique, "type"="Quebec"

Note that functions are not quoted. For functions which are accessed via a shortcut (e.g. "threenum"), you will need to quote those functions to access the shortcut.
