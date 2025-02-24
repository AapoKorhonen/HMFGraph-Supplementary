###############################
# Tests run with:
# CPU: AMD Ryzen 7 PRO 6850U
# RAM: 32 GB
###############################


load(file="time_comparisons/p_100/benchmark_Result100.RData")
benchmark_Result100

# Unit: milliseconds
# expr        min         lq
# HMFGraph_GEM(graph_data$dat, alpha = 0.7)   202.8608   206.6106
# HMFGraph_GEM_permutations(graph_data$dat, HMFGraph_GEM(graph_data$dat,      alpha = 0.7), number_of_permutations = 50, parallel = T)  4337.7764  4488.5184
# HMFGraph_GEM_permutations(graph_data$dat, HMFGraph_GEM(graph_data$dat,      alpha = alpha_selector_eigen_par(graph_data$dat, 0.3)), number_of_permutations = 50,      parallel = T)  9446.6744  9481.4253
# HMFGraph_gibbs_sampler(graph_data$dat, alpha = 0.7, iters = 5000,      burn_in = 50) 10565.7266 10604.0323
# bdgraph(data = graph_data$dat, n = n, verbose = FALSE, iter = 5000,      burnin = 50) 16797.3450 17317.2012
# BGGM::estimate(graph_data$dat, type = "continuous", iter = 5000) 11038.1512 11064.7574
# mean     median         uq        max neval
# 209.5701   211.4741   211.9377   214.9673     5
# 4704.4531  4521.6336  4575.7675  5598.5696     5
# 9552.0528  9536.0568  9548.6850  9747.4223     5
# 10652.8927 10638.4026 10721.7856 10734.5162     5
# 17591.2079 17478.4488 17722.0618 18640.9829     5
# 11138.2476 11086.9459 11233.0906 11268.2931     5

load(file="time_comparisons/p_200/benchmark_Result200.RData")
benchmark_Result200

# Unit: seconds
# expr       min        lq
# HMFGraph_GEM(graph_data$dat, alpha = 0.65)  1.092218  1.104329
# HMFGraph_GEM_permutations(graph_data$dat, HMFGraph_GEM(graph_data$dat,      alpha = 0.65), number_of_permutations = 50, parallel = T) 12.831000 12.885562
# HMFGraph_GEM_permutations(graph_data$dat, HMFGraph_GEM(graph_data$dat,      alpha = alpha_selector_eigen_par(graph_data$dat, 0.3)), number_of_permutations = 50,      parallel = T) 26.555210 26.646629
# HMFGraph_gibbs_sampler(graph_data$dat, alpha = 0.65, iters = 5000,      burn_in = 50) 75.905436 76.028170
# bdgraph(data = graph_data$dat, n = n, verbose = FALSE, iter = 5000,      burnin = 50) 94.916426 96.022575
# BGGM::estimate(graph_data$dat, type = "continuous", iter = 5000) 78.195715 78.504263
# mean    median        uq        max neval
# 1.105949  1.105693  1.111997   1.115509     5
# 13.722135 13.803364 14.304675  14.786077     5
# 26.772007 26.754865 26.770710  27.132620     5
# 76.492494 76.390288 76.921639  77.216939     5
# 98.149892 97.955481 99.315802 102.539176     5
# 78.739029 78.727255 78.893728  79.374182     5


load(file="time_comparisons/p_300/benchmark_Result300.RData")
benchmark_Result300

load(file="time_comparisons/p_400/benchmark_Result400.RData")
benchmark_Result400