# k_means translation for stats engine

    Code
      translate_tidyclust(spec)
    Output
      K Means Cluster Specification (partition)
      
      Main Arguments:
        num_clusters = 3
      
      Computational engine: stats 
      
      Model fit template:
      tidyclust::.k_means_fit_stats(x = missing_arg(), centers = missing_arg(), 
          centers = 3)

# k_means translation for ClusterR engine

    Code
      translate_tidyclust(spec)
    Output
      K Means Cluster Specification (partition)
      
      Main Arguments:
        num_clusters = 3
      
      Computational engine: ClusterR 
      
      Model fit template:
      tidyclust::.k_means_fit_ClusterR(data = missing_arg(), clusters = missing_arg(), 
          clusters = 3)

# k_means translation for klaR engine

    Code
      translate_tidyclust(spec)
    Output
      K Means Cluster Specification (partition)
      
      Main Arguments:
        num_clusters = 3
      
      Computational engine: klaR 
      
      Model fit template:
      tidyclust::.k_means_fit_klaR(data = missing_arg(), modes = missing_arg(), 
          modes = 3)

# k_means translation for clustMixType engine

    Code
      translate_tidyclust(spec)
    Output
      K Means Cluster Specification (partition)
      
      Main Arguments:
        num_clusters = 3
      
      Computational engine: clustMixType 
      
      Model fit template:
      tidyclust::.k_means_fit_clustMixType(x = missing_arg(), k = missing_arg(), 
          keep.data = missing_arg(), k = 3, keep.data = TRUE, verbose = FALSE)

# hier_clust translation for stats engine

    Code
      translate_tidyclust(spec)
    Output
      Hierarchical Clustering Specification (partition)
      
      Main Arguments:
        num_clusters = 3
        linkage_method = complete
      
      Computational engine: stats 
      
      Model fit template:
      tidyclust::.hier_clust_fit_stats(data = missing_arg(), num_clusters = 3, 
          linkage_method = "complete")

# hier_clust translation with linkage_method

    Code
      translate_tidyclust(spec)
    Output
      Hierarchical Clustering Specification (partition)
      
      Main Arguments:
        num_clusters = 3
        linkage_method = ward.D
      
      Computational engine: stats 
      
      Model fit template:
      tidyclust::.hier_clust_fit_stats(data = missing_arg(), num_clusters = 3, 
          linkage_method = "ward.D")

