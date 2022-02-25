library(data.table)
library(ggplot2)
library(mlr3misc)
library(latex2exp)

# data_path = "~/Documents/repos/yahpo_data"
data_path = "/home/flo/LRZ Sync+Share/multifidelity_data"
out = discard(map(list.files(data_path, full.names = TRUE), list.files, pattern = "onnx.csv", full.names = TRUE), function(x) length(x) == 0)
 
global_stats = rbindlist(map(out, function(file) {
    dt = fread(file)
    dt = dt[instance == "all" | instance == "all/CIFAR10", ]
    dt[, instance := NULL][, scenario := basename(dirname(file))]
    return(dt)
}))

fwrite(global_stats, "paper/viz_tables/global_surrogate_stats.csv")

statstab = global_stats[scenario != "fcnet", c(1, 4, 5)]
statstab = statstab[!(target %in% c("rampredict", "ramtrain", "timepredict"))]
statstab[, spearman := round(spearman, 2)]
statstab[target == "test_cross_entropy", target := "test_ce"]
statstab[target == "val_cross_entropy", target := "val_ce"]
statstab[target == "test_balanced_accuracy", target := "test_bac"]
statstab[target == "val_balanced_accuracy", target := "val_bac"]

xx = statstab[, paste0(target, ":", spearman, collapse = ","), by = "scenario"]
colnames(xx) = c("Scenario", "$\\rho$")
xtb = xtable::xtable(xx, label="tab:surr_metrics", caption="Average surrogate performance (Spearman's rho) across all instances per scenario/target. We abbreviate cross\\_entropy (ce) and balanced\\_accuracy(bac) for brevity.")
print(xtb, style = "booktabs", include.rownames=FALSE, sanitize.colnames.function = identity)



instance_stats = rbindlist(map(out, function(file) {
    dt = fread(file)
    dt = dt[instance != "all", ]
    dt[, scenario := basename(dirname(file))]
    return(dt)
}))

make_names = function(x) {
    x = gsub("balanced_", "b", x)
    x = gsub("cross_entropy", "ce", x)
    x = gsub("accuracy", "acc", x)
    x = gsub("test", "test", x)
    return(x)
}

# Decide which instances to drop:
rho_cutoff = 0.7
drop_idx = instance_stats[, list(sp = mean(spearman), scenario = unique(scenario)), by = "instance"][sp < rho_cutoff]
# lcbench : 167083, 167184

instance_stats = instance_stats[!drop_idx, on = c("instance", "scenario")]
instance_stats[, target := make_names(target)]
instance_stats = instance_stats[!(target %in% c("timepredict", "rampredict" ,"ramtrain", 'f1')) & instance != "167083" & scenario != "fcnet",]
p = ggplot(instance_stats, aes(x=target, y = spearman)) + 
  geom_boxplot(outlier.shape =NA) +
  geom_jitter(width = .1, alpha = .3) +
  theme_bw() +
  xlab("") +
  ylab(TeX("Spearman's $\\rho$")) +
  facet_wrap(~scenario, scales = "free_x") +
  coord_cartesian(ylim = c(0.5, 1)) +
  theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(size = 12, angle = -45)
  )
ggsave("paper/viz_tables/boxplot_surrogate_rho.pdf")
  

head(instance_stats)

