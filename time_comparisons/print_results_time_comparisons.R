###############################
# Tests run with:
# CPU: Intel Core i7-11700F
# RAM: 32 GB
###############################
library(microbenchmark)

load(file="time_comparisons/p_100/benchmark_Result100.RData")
benchmark_Result100

# Unit: milliseconds
# expr        min         lq      mean    median         uq        max neval
# HMFGraph_GEM(graph_data$data, alpha = alpha)   305.9367   309.2537   313.494   309.761   318.2655   324.2529     5
# HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data,      alpha = alpha), number_of_permutations = 50, parallel = T)  4044.8906  4060.7896  4077.604  4067.184  4103.2992  4111.8558     5
# HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data),      number_of_permutations = 50, parallel = T)  5297.5081  5297.7118  5319.576  5307.388  5334.3978  5360.8722     5
# HMFGraph_gibbs_sampler(graph_data$data, alpha = alpha, iters = 5000,      burn_in = 50) 12653.3935 12655.9936 12727.711 12701.920 12705.8790 12921.3676     5
# bdgraph(data = graph_data$data, n = n, verbose = FALSE, iter = 5000,      burnin = 50) 16866.4176 17100.7807 17291.646 17388.406 17476.7672 17625.8582     5
# BGGM::estimate(graph_data$data, type = "continuous", iter = 5000) 12427.0711 12429.9241 12442.964 12435.594 12455.0920 12467.1407     5

load(file="time_comparisons/p_200/benchmark_Result200.RData")
benchmark_Result200

# Unit: seconds
# expr       min       lq      mean    median        uq       max neval
# HMFGraph_GEM(graph_data$data, alpha = alpha)  1.490922  1.49464  1.499779  1.503442  1.504227  1.505666     5
# HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data,      alpha = alpha), number_of_permutations = 50, parallel = T) 12.351532 12.35161 12.492125 12.421603 12.611858 12.724028     5
# HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data),      number_of_permutations = 50, parallel = T) 19.011047 19.18365 19.211646 19.264284 19.282508 19.316737     5
# HMFGraph_gibbs_sampler(graph_data$data, alpha = alpha, iters = 5000,      burn_in = 50) 94.669439 94.77099 94.800344 94.826270 94.827388 94.907635     5
# bdgraph(data = graph_data$data, n = n, verbose = FALSE, iter = 5000,      burnin = 50) 89.584286 89.96984 90.570977 91.039188 91.100939 91.160627     5
# BGGM::estimate(graph_data$data, type = "continuous", iter = 5000) 92.402262 92.47456 92.538318 92.475317 92.627575 92.711872     5


load(file="time_comparisons/p_300/benchmark_Result300.RData")
benchmark_Result300

# Unit: seconds
#                                                                                                                                                 expr        min         lq       mean     median         uq        max neval
# HMFGraph_GEM(graph_data$data, alpha = alpha)                                                                                                    5.667685   5.669779   5.687856   5.698592   5.700008   5.703216     5
# HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data,      alpha = alpha), number_of_permutations = 50, parallel = T)        38.298518  38.350310  38.445221  38.376594  38.404726  38.795957     5
# HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data),      number_of_permutations = 50, parallel = T)                       68.930847  68.987362  69.085592  69.009823  69.128201  69.371726     5
# HMFGraph_gibbs_sampler(graph_data$data, alpha = alpha, iters = 5000,      burn_in = 50)                                                         314.083319 314.255153 316.755027 314.308362 319.807600 321.320700     5
# bdgraph(data = graph_data$data, n = n, verbose = FALSE, iter = 5000,      burnin = 50)                                                          505.906044 506.451402 511.154808 506.655145 507.788975 528.972477     5
# BGGM::estimate(graph_data$data, type = "continuous", iter = 5000,      )                                                                        307.379667 307.387561 308.188138 307.903413 308.158830 310.111217     5


load(file="time_comparisons/p_400/benchmark_Result400.RData")
benchmark_Result400


# Unit: seconds
# expr        min         lq       mean     median         uq        max neval
# HMFGraph_GEM(graph_data$data, alpha = alpha)   14.90920   14.91072   14.95288   14.94871   14.96716   15.02865     5
# HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data,      alpha = alpha), number_of_permutations = 50, parallel = T)   92.99534   93.01906   93.30750   93.09629   93.62317   93.80362     5
# HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data),      number_of_permutations = 50, parallel = T)  137.28552  137.31899  137.62219  137.40077  137.65237  138.45328     5
# HMFGraph_gibbs_sampler(graph_data$data, alpha = alpha, iters = 5000,      burn_in = 50)  757.41186  758.86350  758.88311  759.14335  759.21768  759.77917     5
# bdgraph(data = graph_data$data, n = n, verbose = FALSE, iter = 5050,      burnin = 50) 1926.14183 1930.55150 1936.09735 1936.85358 1942.47844 1944.46142     5
# BGGM::estimate(graph_data$data, type = "continuous", iter = 5000)  736.31341  743.60486  754.39601  750.49177  765.90146  775.66854     5
