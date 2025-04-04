
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HMFGraph-Supplementary

All R packages that are required for reproducing the results in the
repository:

``` r
install.packages("devtools")
install.packages("Matrix")
install.packages("aricode")
install.packages("pulsar")
install.packages("huge")
install.packages("BDgraph")
install.packages("igraph")
install.packages("qgraph")
install.packages("microbenchmark")
install.packages("parallel")
install.packages("foreach")
install.packages("doSNOW")
install.packages("progress")
install.packages("BGGM")
install.packages("hdi")
install.packages("flare")
install.packages("rags2ridges")
install.packages("BiocManager")
install.packages("RColorBrewer")
BiocManager::install("phyloseq")
devtools::install_github("zdk123/SpiecEasi")
devtools::install_github("gleday/beam")
devtools::install_github("apatrone2/thav.glasso")
devtools::install_github("AapoKorhonen/HMFGraph")
```

# Guide

All functions require the working directory to be the main directory
(where this README lies). The files are designed such that the user does
not need to edit any files. All results can be replicated by running
each file once found in this repository. The directory “functions”
include functions the are used in other files. All figures are presented
in the directory “figures”.

## The real datasets

The results with riboflavin dataset can be found in the file
“real_data/ribloflavinv100_graph_results.R”. If you run the whole file,
it will save the plot as a esp-file (Figure 1).

The results with the gut dataset can be found in the file
“real_data/gut_data_graph_results.R”. If you run the whole file, it will
save the plot as a esp-file (Figure 2).

## Simulating datasets

Simulated datasets are generated with R packages huge and BDgraph. Run
files all R-files in directory “simulated_data”. The datasets are saved
to directories “simulated_data/bdgraph” and “simulated_data/huge”.

## The main results

Run all R files in directory “results”. The results are saved to
directories “results/bdgraph” and “results/huge”. You can examine the
results with R files in the directory “results/print_results”.

## Parameter comparisons (alpha)

Run all R files in directory “parameter_comparisons/alpha”. The results
are saved to directories “parameter_comparisons/alpha/results”. You can
plot the results with R file “plot_alpha_results.R” in the directory
“parameter_comparisons/alpha/comparison”. (Figures S3 and S4)

## Parameter comparisons (beta)

Run all R files in directory “parameter_comparisons/beta”. The results
are saved to directories “parameter_comparisons/beta/results”. You can
plot the results with R file “plot_alpha_results.R” in the directory
“parameter_comparisons/beta/comparison”. (Figures S1 and S2)

## Selected alpha values

Run all R files in directory “selected_alpha_values”. The results are
saved to directories “selected_alpha_values/results”. You can plot the
results with R file “selected_alpha_plot.R” in the directory
“selected_alpha_values/results/plot_results”. (Figure S5)

## Comparisons of credibel intervals

Credible interval comparisons and the figures depicting the comparisons
are produced with files found from directory “CI_comparisons”. (Figures
S6 and S7)

## FDR control

The file “FDR_control.R” in the directory “FDR_control” produces the
results for investigating the FDR control. The results are save to the
directory “FDR_control/results”. The figure depicting the results can be
produced with the r file “FDR_control_plot.R”. (Figures S8)

## Network comparisons

Run all R files in directory “network_comparisons”. The files will save
the plot as esp-files. (Figures S9, S10, S11 and S12)

## Computation times

All R-files in the directory “time_comparisons” have to be run. The
results are save to directories “p_100”, “p_200”, “p_300” and “p_400”.
The current results are run with CPU i7-11700f with 32 GB system memory.
The results can be view with file “print_results_time_comparisons.R” in
the directory “time_comparisons/print_results”.
