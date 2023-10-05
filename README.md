## Introduction

**rarestR** is an R package of rarefaction-based species richness
estimator. This package is designed to calculate rarefaction-based *α*-
and *β*-diversity. It also offers parametric extrapolation to estimate
the total expected species in a single community and the total expected
shared species between two communities. The package also provides
visualization of the curve-fitting for these estimators.

## Installation

    # Stable version
    install.packages('rarestR')
    # Development version
    remotes::install_github('pzhaonet/rarestR')

## Load rarestR and the demo dataset

    library(rarestR)
    data("share")

The dataset **share** is a matrix with 3 rows and 142 columns. It
comprises three samples randomly drawn from three simulated communities.
Every community consists of 100 species with approximately 100,000
individuals following a log-normal distribution (mean = 6.5, SD = 1).
Setting the first community as control group, the second and third
community shared a total of 25 and 50 species with the control. A more
detailed description of the control and scenario groups can be found in
Zou and Axmacher (2021). The share dataset represents a random subsample
of 100, 150 and 200 individuals from three three communities, containing
58, 57 and 74 species, respectively.

## Calculate the Expected Species

    es(share, m = 100)

    ##        1        2        3 
    ## 58.00000 47.77653 53.00568

    es(share, method = "b", m = 100)

    ##        1        2        3 
    ## 43.51041 40.74378 46.19118

    # When the m is larger than the total sample size, "NA" will be filled:
    es(share, m = 150)

    ## Warning in es(y, m, method): m can not be larger than the total sample size

    ##        1        2        3 
    ##       NA 57.00000 65.24147

## Compute dissimilarity estimates between two samples based on Expected Species Shared (ESS)-measures

    ess(share)

    ##           1         2
    ## 2 0.7970962          
    ## 3 0.6359703 0.7642330

    ess(share, m = 100)

    ##           1         2
    ## 2 0.8566624          
    ## 3 0.7308390 0.8229221

    ess(share, m = 100, index = "ESS")

    ##          1        2
    ## 2 13.01735         
    ## 3 22.65674 13.23924

## Calculate and visualize the Total Expected Species base on ESa, ESb and their average value

    Output_tes <- tes(share[1,])
    Output_tes

    ##          est est.sd model.par
    ## TESa  138.50   2.46  logistic
    ## TESb   92.63  32.65   Weibull
    ## TESab 115.56  16.37      <NA>

    plot(Output_tes)

## Calculate and visualize the Total number of Expected Shared Species between two samples

    Output_tess <- tess(share[1:2,])
    Output_tess

    ##     est est.sd model.par
    ## 1 23.28   2.59  logistic

    plot(Output_tess)

## References

Zou, Y, & Axmacher, JC (2021). Estimating the number of species shared
by incompletely sampled communities. *Ecography*, *44*(7),
1098-1108.[doi:10.1111/ecog.05625](https://doi.org/10.1111/ecog.05625)

Zou Y, Zhao P, Axmacher JC (2023). Estimating total species richness:
Fitting rarefaction by asymptotic approximation. *Ecosphere*, *14*(1),
e4363. [doi:10.1002/ecs2.4363](https://doi.org/10.1002/ecs2.4363).
