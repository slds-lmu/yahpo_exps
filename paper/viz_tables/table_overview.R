library(data.table)
library(ggplot2)
library(mlr3misc)
library(xtable)
# reticulate::conda_install(envname = "yahpo_gym", pip=TRUE, packages="'git+https://github.com/pfistfl/yahpo_gym#egg=yahpo_gym&subdirectory=yahpo_gym'")

yahpo_path = "~/Documents/repos/yahpo_gym/yahpo_gym_r/"
data_path = "~/Documents/repos/yahpo_data/"
devtools::load_all(yahpo_path)


gym = reticulate::import("yahpo_gym")
configs = gym$configuration$config_dict$configs

cf_to_space = function(cf) {
    space = ifelse(length(cf$cont_names) & (length(cf$cat_names) -1), "Mixed", "Continuous")
    dep = ifelse(cf$hierarchical, "+Deps", "")
    paste0(space, dep)
}
fixup_fidelity = function(fid) {
    ifelse(fid == "trainsize", "frac", fid)
}

over = rbindlist(map(configs, function(cf) {data.table(
    "Scenario" = cf$config_id,
    "#HPs" = length(setdiff(c(cf$cont_names, cf$cat_names), cf$fidelity)) - 1L,
    "#Targets" = length(cf$y_names),
    "#Instances" = length(BenchmarkSet$new(cf$config_id)$instances),
    "Space" = cf_to_space(cf),
    "Fidelity" = fixup_fidelity(cf$fidelity[1])
)}))

xtb = xtable::xtable(over, 
caption = "Overview of Scenarios in YAHPO Gym. Scenarios have between 4 and 35 hyperparameters, 2-12 targets and up to 114 instances.\\
  Mixed = mixed search space, Deps = hierarchical search space, frac = dataset fraction",
label = "tab:overview",
format = "latex"
)

print(xtb, booktabs = TRUE, include.rownames=FALSE)
# For README's
knitr::kable(over)


for (cf in map(configs, "config_id")) {
  length(BenchmarkSet$new(cf)$instances)
}
