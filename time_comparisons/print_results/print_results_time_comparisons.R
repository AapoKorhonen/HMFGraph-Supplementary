###############################
# Tests run with:
# CPU: Intel Core i7-11700F
# RAM: 32 GB
###############################
library(microbenchmark)


#=================================================================================
# p = 100
#=================================================================================


load(file="time_comparisons/p_100/benchmark_Result100_HMFGraph.RData")
benchmark_Result100_HMFGraph
# Unit: milliseconds
#  median : 299.5302

load(file="time_comparisons/p_100/benchmark_Result100_HMFGraph_P.RData")
benchmark_Result100_HMFGraph_P
# Unit: seconds
#  median : 3.949834

load(file="time_comparisons/p_100/benchmark_Result100_HMFGraph_P_alpha.RData")
benchmark_Result100_HMFGraph_P_alpha
# Unit: seconds
#  median : 5.314875

load(file="time_comparisons/p_100/benchmark_Result100_HMFGraph_P_alpha_WP.RData")
benchmark_Result100_HMFGraph_P_alpha_WP
# Unit: seconds
#  median : 11.81925

load(file="time_comparisons/p_100/benchmark_Result100_HMFGraph_Gibbs.RData")
benchmark_Result100_HMFGraph_Gibbs
# Unit: seconds
#  median : 14.37286

load(file="time_comparisons/p_100/benchmark_Result100_BGGM.RData")
benchmark_Result100_BGGM
# Unit: seconds
#  median : 12.02836

load(file="time_comparisons/p_100/benchmark_Result100_G_wishart.RData")
benchmark_Result100_G_wishart
# Unit: seconds
#  median : 16.69958

#=================================================================================
# p = 200
#=================================================================================


load(file="time_comparisons/p_200/benchmark_Result200_HMFGraph.RData")
benchmark_Result200_HMFGraph
# Unit: milliseconds
#  median : 299.5302

load(file="time_comparisons/p_200/benchmark_Result200_HMFGraph_P.RData")
benchmark_Result200_HMFGraph_P
# Unit: seconds
#  median : 3.949834

load(file="time_comparisons/p_200/benchmark_Result200_HMFGraph_P_alpha.RData")
benchmark_Result200_HMFGraph_P_alpha
# Unit: seconds
#  median : 5.314875

load(file="time_comparisons/p_200/benchmark_Result200_HMFGraph_P_alpha_WP.RData")
benchmark_Result200_HMFGraph_P_alpha_WP
# Unit: seconds
#  median : 11.81925

load(file="time_comparisons/p_200/benchmark_Result200_HMFGraph_Gibbs.RData")
benchmark_Result200_HMFGraph_Gibbs
# Unit: seconds
#  median : 14.37286

load(file="time_comparisons/p_200/benchmark_Result200_BGGM.RData")
benchmark_Result200_BGGM
# Unit: seconds
#  median : 12.02836

load(file="time_comparisons/p_200/benchmark_Result200_G_wishart.RData")
benchmark_Result200_G_wishart
# Unit: seconds
#  median : 16.69958


#=================================================================================
# p = 300
#=================================================================================


load(file="time_comparisons/p_300/benchmark_Result300_HMFGraph.RData")
benchmark_Result300_HMFGraph
# Unit: milliseconds
#  median : 299.5302

load(file="time_comparisons/p_300/benchmark_Result300_HMFGraph_P.RData")
benchmark_Result300_HMFGraph_P
# Unit: seconds
#  median : 3.949834

load(file="time_comparisons/p_300/benchmark_Result300_HMFGraph_P_alpha.RData")
benchmark_Result300_HMFGraph_P_alpha
# Unit: seconds
#  median : 5.314875

load(file="time_comparisons/p_300/benchmark_Result300_HMFGraph_P_alpha_WP.RData")
benchmark_Result300_HMFGraph_P_alpha_WP
# Unit: seconds
#  median : 11.81925

load(file="time_comparisons/p_300/benchmark_Result300_HMFGraph_Gibbs.RData")
benchmark_Result300_HMFGraph_Gibbs
# Unit: seconds
#  median : 14.37286

load(file="time_comparisons/p_300/benchmark_Result300_BGGM.RData")
benchmark_Result300_BGGM
# Unit: seconds
#  median : 12.02836

load(file="time_comparisons/p_300/benchmark_Result300_G_wishart.RData")
benchmark_Result300_G_wishart
# Unit: seconds
#  median : 16.69958

#=================================================================================
# p = 400
#=================================================================================


load(file="time_comparisons/p_400/benchmark_Result400_HMFGraph.RData")
benchmark_Result400_HMFGraph
# Unit: milliseconds
#  median : 299.5302

load(file="time_comparisons/p_400/benchmark_Result400_HMFGraph_P.RData")
benchmark_Result400_HMFGraph_P
# Unit: seconds
#  median : 3.949834

load(file="time_comparisons/p_400/benchmark_Result400_HMFGraph_P_alpha.RData")
benchmark_Result400_HMFGraph_P_alpha
# Unit: seconds
#  median : 5.314875

load(file="time_comparisons/p_400/benchmark_Result400_HMFGraph_P_alpha_WP.RData")
benchmark_Result400_HMFGraph_P_alpha_WP
# Unit: seconds
#  median : 11.81925

load(file="time_comparisons/p_400/benchmark_Result400_HMFGraph_Gibbs.RData")
benchmark_Result400_HMFGraph_Gibbs
# Unit: seconds
#  median : 14.37286

load(file="time_comparisons/p_400/benchmark_Result400_BGGM.RData")
benchmark_Result400_BGGM
# Unit: seconds
#  median : 12.02836

load(file="time_comparisons/p_400/benchmark_Result400_G_wishart.RData")
benchmark_Result400_G_wishart
# Unit: seconds
#  median : 16.69958