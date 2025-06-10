This input allows you to select the dimension of your basis which is the _k_ parameter for gam() in _mgcv_. In simpler terms, it lets you select the number of basis functions that will make up your smooth. Your first selection for this parameter will be mostly arbirtrary and you will need to experiment to find an appropriate value of _k_.

The Appraise module will help you with this by providing the output of gam.check(). See the Student Walkthrough and the Learn module for more details.

_mgcv_ also provides useful information in the doco in [choose.k](https://cran.r-project.org/web/packages/mgcv/mgcv.pdf#Rfn.choose.k) and [gam.check](https://cran.r-project.org/web/packages/mgcv/mgcv.pdf#Rfn.gam.check).
