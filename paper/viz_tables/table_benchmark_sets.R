# Creates the table describing benchmark sets in the appendix.
library(data.table)
library(ggplot2)
library(mlr3misc)
library(latex2exp)
library(RJSONIO)

data_path = "/home/flo/LRZ Sync+Share/multifidelity_data"

out = discard(map(list.files(data_path, full.names = TRUE), list.files, pattern = "onnx.csv", full.names = TRUE), function(x) length(x) == 0)
instance_stats = rbindlist(map(out, function(file) {
    dt = fread(file)
    dt = dt[instance != "all", ]
    dt[, scenario := basename(dirname(file))]
    dt[scenario == "nb301", instance := "CIFAR10"]
    return(dt)
}))

overview = fread("paper/viz_tables/overview.csv")



replace_longnames = function(x) {
    map_chr(x, function(x) {
        if (x == "test_balanced_accuracy") {
            x = "test_bac"
        } 
        if (x == "test_cross_entropy") {
            x = "test_ce"
        } 
        return(x)
    })
}

tabs = map(list.files(paste0(data_path, "/benchmark_suites/v0.1"), full.names = TRUE), function(path) {
    ll = RJSONIO::fromJSON(path)
    dt = rbindlist(map(ll, function(x) {
        xx = mean(merge(as.data.table(x), instance_stats, on = c("instance", "scenario", "target"))$spearman)
        dt = data.table(x$scenario, x$instance, paste0(replace_longnames(x$target), collapse = ","))
        dt[, rho := xx]  
    }))
    colnames(dt) = c("scenario", "instance", "target(s)", "$\\rho$")
    return(dt)
})

tabs = map(tabs, function(x) {
    merge(x, y = overview, by.x = "scenario", by.y = "Scenario")[,c(1:4, 10)]
})

xx1 = xtable::xtable(tabs[[2]], format = "latex", digits = 2, label = "tab:yahposo", 
caption = "\\textbf{YAHPO-SO} (v1): Collection of single-objective benchmark instances. test\\_bac = test\\_balanced\\_accuracy. We indicate surrogate approximation quality using Spearman's $\\rho$.")
xx2 = xtable::xtable(tabs[[1]], format = "latex", digits = 2, label = "tab:yahpomo", 
  caption = "\\textbf{YAHPO-MO} (v1): Collection of multi-objective benchmark instances. test\\_bac = test\\_balanced\\_accuracy, test\\_ce: test\\_cross\\_entropy. We indicate surrogate approximation quality using Spearman's $\\rho$.")


con = file("paper/viz_tables/tables_benchmark_instances.tex", open = "wt")
writeLines(print(xx1, booktabs = TRUE, sanitize.colnames.function = identity), con)
close(con)
con = file("paper/viz_tables/tables_benchmark_instances.tex", open = "at")
writeLines(print(xx2, booktabs = TRUE, sanitize.colnames.function = identity), con)
close(con)



