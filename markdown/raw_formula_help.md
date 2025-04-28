The raw formula input allows you to write a raw formula without needing to use the GUI. This has two main advantages

1) If you know the formula you want, then you don't need to click through the GUI to build terms and can quickly build a GAM for interpretation.

2) This allows you to use some features of mgcv which are not present in the GUI. For example, any unsupported smooths or extra options/arguments which the GUI does not cater for. An example of this would be the 'xp' argument for a given smooth. Note that as these options are unsupported, you will use them at your own risk.

When entering the raw formula, treat it as you would the building the formula through the command line. The only exception to this is that you do not provide the response or '~' char in the text input.

e.g. for the formula 

> uptake ~ treatment * type + s(plant, bs = "re") + s(conc, by = treatment, k = 7)

you would only enter

> treatment * type + s(plant, bs = "re") + s(conc, by = treatment, k = 7)

The response is entered via the dropdown menu in the main GUI.

Once you have entered your hyparameters, the response and the raw text input you can click on the 'Build GAM' button to build your model.
