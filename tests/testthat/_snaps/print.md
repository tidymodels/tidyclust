# print.cluster_spec() works

    Code
      print(k_means(num_clusters = 3))
    Output
      K Means Cluster Specification (partition)
      
      Main Arguments:
        num_clusters = 3
      
      Computational engine: stats 
      

# print.cluster_spec() works with engine set

    Code
      print(set_engine(k_means(num_clusters = 3), "stats"))
    Output
      K Means Cluster Specification (partition)
      
      Main Arguments:
        num_clusters = 3
      
      Computational engine: stats 
      

# print.cluster_fit() works

    Code
      print(fit)
    Output
      tidyclust cluster object
      
      K-means clustering with 3 clusters of sizes 7, 11, 14
      
      Cluster means:
             mpg cyl     disp        hp     drat       wt     qsec        vs
      1 19.74286   6 183.3143 122.28571 3.585714 3.117143 17.97714 0.5714286
      3 26.66364   4 105.1364  82.63636 4.070909 2.285727 19.13727 0.9090909
      2 15.10000   8 353.1000 209.21429 3.229286 3.999214 16.77214 0.0000000
               am     gear     carb
      1 0.4285714 3.857143 3.428571
      3 0.7272727 4.090909 1.545455
      2 0.1428571 3.285714 3.500000
      
      Clustering vector:
                Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                        1                   1                   2                   1 
        Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                        3                   1                   3                   2 
                 Merc 230            Merc 280           Merc 280C          Merc 450SE 
                        2                   1                   1                   3 
               Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                        3                   3                   3                   3 
        Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                        3                   2                   2                   2 
            Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                        2                   3                   3                   3 
         Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                        3                   2                   2                   2 
           Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                        3                   1                   3                   2 
      
      Within cluster sum of squares by cluster:
      [1] 13954.34 11848.37 93643.90
       (between_SS / total_SS =  80.8 %)
      
      Available components:
      
      [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
      [6] "betweenss"    "size"         "iter"         "ifault"      

# print.cluster_fit() works for hier_clust

    Code
      print(fit)
    Output
      tidyclust cluster object
      
      
      Call:
      stats::hclust(d = stats::as.dist(dmat), method = linkage_method)
      
      Cluster method   : complete 
      Number of objects: 32 
      

