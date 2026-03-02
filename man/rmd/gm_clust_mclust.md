For this engine, there is a single mode: partition

## Tuning Parameters

This model has 6 tuning parameters:

-   `num_clusters`: \# Clusters (type: integer, default: no default)

-   `circular`: Circular cluster shapes? (type: logical, default: TRUE)

-   `zero_covariance`: Zero covariance between predictors? (type:
    logical, default: TRUE)

-   `shared_orientation`: Shared orientation between clusters? (type:
    logical, default: TRUE)

-   `shared_shape`: Shared shape between clusters? (type: logical,
    default: TRUE)

-   `shared_size`: Shared size between clusters? (type: logical,
    default: TRUE)

## Translation from tidyclust to the original package (partition)

    gm_clust(num_clusters = 3, circular = FALSE, zero_covariance = FALSE) %>% 
      set_engine("mclust") %>% 
      set_mode("partition") %>% 
      translate_tidyclust()

    ## GMM Clustering Specification (partition)
    ## 
    ## Main Arguments:
    ##   num_clusters = 3
    ##   circular = FALSE
    ##   zero_covariance = FALSE
    ##   shared_orientation = TRUE
    ##   shared_shape = TRUE
    ##   shared_size = TRUE
    ## 
    ## Computational engine: mclust 
    ## 
    ## Model fit template:
    ## tidyclust::.gm_clust_fit_mclust(x = missing_arg(), G = missing_arg(), 
    ##     circular = missing_arg(), zero_covariance = missing_arg(), 
    ##     shared_orientation = missing_arg(), shared_shape = missing_arg(), 
    ##     shared_size = missing_arg(), G = 3, circular = FALSE, zero_covariance = FALSE, 
    ##     shared_orientation = TRUE, shared_shape = TRUE, shared_size = TRUE)

## Preprocessing requirements

Gaussian Mixture Models should be fit with only quantitative predictors
and without any categorical predictors. No scaling is required since the
variance-covariance matrices of the Gaussian distributions account for
the unequal variances between predictors and their covariances.

## References

-   Banfield, J. D., & Raftery, A. E. (1993). Model-Based Gaussian and
    Non-Gaussian Clustering. Biometrics, 49(3), 803.
    <https://doi.org/10.2307/2532201>

-   Celeux, G., & Govaert, G. (1995). Gaussian parsimonious clustering
    models. Pattern Recognition, 28(5), 781–793.
    <https://doi.org/10.1016/0031-3203(94)00125-6>

-   Dempster, A. P., Laird, N. M., & Rubin, D. B. (1977). Maximum
    Likelihood from Incomplete Data via the EM Algorithm.

-   McNicholas, P. D. (2016). Model-Based clustering. Journal of
    Classification, 33(3), 331–373.
    <https://doi.org/10.1007/s00357-016-9211-9>

-   Scrucca, L., Fop, M., Murphy, T., Brendan, & Raftery, A., E. (2016).
    Mclust 5: Clustering, Classification and Density Estimation Using
    Gaussian Finite Mixture Models. The R Journal, 8(1), 289.
    <https://doi.org/10.32614/RJ-2016-021>

-   Scrucca, L., Fraley, C., Murphy, T. B., & Raftery, A. E. (2023).
    Model-based clustering, classification, and density estimation using
    mclust in R. Chapman; Hall/CRC. https:
    //doi.org/10.1201/9781003277965
