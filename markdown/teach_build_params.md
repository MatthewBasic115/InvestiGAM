# Hyperparameter Build Menu

This section provides information on setting the hyperparameters for your GAM.

### Help Buttons

Each item in the hyperparameter build menu has a help button which will provide you information so that you do not have to return to this page.

### Family

The first item is the distribution family. You can learn the technical background for this in the 'GLM' selection in the teach menu. This sets the distribution that is used when fitting the GAM. When an item is selected, the options in the 'Link' dropdown are changed so only valid link functions can be selected. The canonical link for the given family is then auto-selected for the link selection.

### Link

The link function menu provides valid options for the given family as mentioned above. The canonical link function is chosen by default, so you should not have to change the selection once you have chosen your family. However, as there are multiple valid link functions for a given family, the option to choose has been provided.

### Method

Method for estimating the smoothing parameter. This defaults to REML however other options are provided. The Help button will provide further information from the mgcv doco.

### Response Variable

This allows you to select the response variable for your GAM from the loaded dataset. If you are entering a raw formula, please include the response variable as this field will be ignored.
