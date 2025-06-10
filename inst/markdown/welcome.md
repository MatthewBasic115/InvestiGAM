# Welcome

Welcome to InvestiGAM!

InvestiGAM is a Shiny application which aims to teach it's users about Generalised Additive Models (GAMs) and provide tools to aid in interpreting their output. The GAMs are built using Simon Wood's mgcv package (Wood, 2025) and the interpretation tools make use of the marginaleffects package (Arel-Bundock et al., 2024). InvestiGAM consists of four primary components: build, appraise, interpret and learn.

Please rezise the application window to your convinience.

### Build

The Build section allows the user to build a GAM via a GUI with the mgcv package. This allows beginners to build GAMs without any need to code. Furthermore, there are many Help buttons which provide technical details a given input to teach users how to build their GAM. The build menu does not implement the full suite of options provided by mgcv in order to reduce complexity. However, experienced users can use input the raw formula to utilise options not present in the GUI and avoid cumbersome UI elements.

### Load

The Load page allows you to load in a dataset and define the data type for the columns. The vroom package is used to load and transform the data. This app does not offer easy ways to transform data (outside of determining its type when loading), so you will need to clean and prepare your data outside this app.

### Appraise

Once the model has been built, appraisal plots from the gratia package are displayed to assist in model evaluation. 

Other helpful tools from mgcv such as gam.check() and summary are also provided.

### Interpret

The Interpret tab uses the marginaleffects package to provide tools for interpreting your GAM. For beginners, this allows you to learn to interpret GAMs first without needing to learn additional R packages. For experienced users, this allows you to quickly visualise and interpret without the need to generate swathes of boilerplate code. More information is provided on the introduction page for the section.

### Learn

The learn section is designed for students who are using this application as a learning tool. It introduces core GAM concepts at a high level and how to use InvestiGAM to build and interpret GAMs. This introduction aims to provide a conceptual understanding of core GAM components rather then teach technical details. As such, if you are not learning underlying theory through coursework, then I reccomend Simon Wood's fantastic _Generalized Additive Models: An Introduction with R_ which was the source material for the teaching section in this module.

### Important Notes

As InvestiGAM is designed to act as a teaching tool, it has a limited scope in supported GAM functionality. While experienced users wiill find use in the Interpret module, for more advanced GAMs you will need to write your own code.

### Acknowledgements


The marginaleffects package (Arel-Bundock, 2024) is the core of the Interpret module. If you would like to learn how to generate the plots and simulations used in this application, then please access the marginaleffects website seen in the Citations below.

Simon Wood is the creator of the mgcv package and the book  _Generalized Additive Models: An Introduction with R_ (Wood, 2017) which s the primary source of the teaching material provided in this application. This application would not be possble without these two resources. Thank you Simon!

Gavin Simpson, creator of the _gratia_ (Simpson, 2024) package which was used throughout the InvestiGAM to make it easier to work with GAMs. Gavin also has great youtube videos on learning how to use GAMs (see References tab for a link).

Thank you to Dr. Nick Clark at the University of Queensland for supervising, guiding and reviewing this project.

### Citations

See Reference tab for full reference list for the application.
