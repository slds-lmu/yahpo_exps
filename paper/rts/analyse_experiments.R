library(data.table)  # 1.14.2
library(ggplot2)  # 3.3.5
library(ggpubr)  # 0.4.0
library(pammtools)  # 0.5.7
library(mlr3misc)  # 0.10.0

results_branin = readRDS("results/rts_results_branin.rds")
results_currin = readRDS("results/rts_results_currin.rds")
results_hartmann3d = readRDS("results/rts_results_hartmann3d.rds")
results_hartmann6d = readRDS("results/rts_results_hartmann6d.rds")
results_borehole = readRDS("results/rts_results_borehole.rds")
measures_synthetic = readRDS("results/measures_synthetic.rds")
names(measures_synthetic) = c("Branin2D", "Currin2D", "Hartmann3D", "Hartmann6D", "Borehole8D")
ylims = list(c(0, 0.02), c(0, 0.01), c(0, 0.3), c(0, 0.75), c(0, 0.75))
names(ylims) = c("Branin2D", "Currin2D", "Hartmann3D", "Hartmann6D", "Borehole8D")

values = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3")
labels = c("BO_GP_RS", "BO_GP_DF", "BO_NN_RS", "BO_NN_DF", "BO_RF_RS", "BO_RF_DF", "HB", "RS")

plot_results = function(results, log = FALSE, instance, init) {
  results$method = factor(results$method, labels = c("Real", "Surrogate", "Tabular"), levels = c("real", "surrogate", "tabular"))
  title = bquote(.(instance) ~~ R^2 == .(round(measures_synthetic[[instance]]$rsq, 2)) ~~ rho == .(round(measures_synthetic[[instance]]$srho, 2)) ~~ tau == .(round(measures_synthetic[[instance]]$ktau, 2)))

  if (log) {
  ggplot(aes(x = cumbudget, y = mean_normalized_regret, colour = optimizer, fill = optimizer), data = results) +
    geom_step() +
    geom_stepribbon(aes(min = mean_normalized_regret - se_normalized_regret, max = mean_normalized_regret + se_normalized_regret), colour = NA, alpha = 0.3) +
    ylab("log Mean Normalized Regret") +
    xlab("Cumulative Budget") +
    labs(title = title, colour = guide_legend(title = "HPO Method"), fill = guide_legend(title = "HPO Method")) +
    facet_grid(~ method) +
    scale_y_log10() +
    scale_colour_manual(labels = labels, values = values) +
    scale_fill_manual(labels = labels, values = values) +
    geom_vline(xintercept = init, colour = "grey", linetype = 2) +
    theme_minimal()
  } else {
  ggplot(aes(x = cumbudget, y = mean_normalized_regret, colour = optimizer, fill = optimizer), data = results) +
    geom_step() +
    geom_stepribbon(aes(min = mean_normalized_regret - se_normalized_regret, max = mean_normalized_regret + se_normalized_regret), colour = NA, alpha = 0.3) +
    ylab("Mean Normalized Regret") +
    xlab("Cumulative Budget") +
    labs(title = title, colour = guide_legend(title = "HPO Method"), fill = guide_legend(title = "HPO Method")) +
    facet_grid(~ method) +
    scale_colour_manual(labels = labels, values = values) +
    scale_fill_manual(labels = labels, values = values) +
    geom_vline(xintercept = init, colour = "grey", linetype = 2) +
    ylim(ylims[[instance]]) +
    theme_minimal()
  }
}

plt1 = imap(list(Hartmann6D = results_hartmann6d, Borehole8D = results_borehole), function(x, nm) {
  init = switch(nm, Branin2D = 10, Currin2D = 10, Hartmann3D = 15, Hartmann6D = 30, Borehole8D = 40)
  plot_results(x, log = FALSE, instance = nm, init = init)
})

plt2 = imap(list(Branin2D = results_branin, Currin2D = results_currin, Hartmann3D = results_hartmann3d), function(x, nm) {
  init = switch(nm, Branin2D = 10, Currin2D = 10, Hartmann3D = 15, Hartmann6D = 30, Borehole8D = 40)
  plot_results(x, log = FALSE, instance = nm, init = init)
})



results_rank_branin = readRDS("results/rts_results_rank_branin.rds")
results_rank_currin = readRDS("results/rts_results_rank_currin.rds")
results_rank_hartmann3d = readRDS("results/rts_results_rank_hartmann3d.rds")
results_rank_hartmann6d = readRDS("results/rts_results_rank_hartmann6d.rds")
results_rank_borehole = readRDS("results/rts_results_rank_borehole.rds")

plot_ranks = function(results, instance, init, title = TRUE) {
  results$method = factor(results$method, labels = c("Real", "Surrogate", "Tabular"), levels = c("real", "surrogate", "tabular"))
  title = if (title) {
    bquote(.(instance) ~~ R^2 == .(round(measures_synthetic[[instance]]$rsq, 2)) ~~ rho == .(round(measures_synthetic[[instance]]$srho, 2)) ~~ tau == .(round(measures_synthetic[[instance]]$ktau, 2)))
  } else {
    waiver()
  }
  ggplot(aes(x = cbx, y = mean_rank, colour = optimizer, fill = optimizer), data = results) +
    geom_line() +
    geom_ribbon(aes(min = mean_rank - se_rank, max = mean_rank + se_rank), colour = NA, alpha = 0.3) +
    facet_grid(~ method) +
    ylab("Mean Rank") +
    xlab("Cumulative Budget") +
    labs(title = title, colour = guide_legend(title = "HPO Method"), fill = guide_legend(title = "HPO Method")) +
    scale_colour_manual(labels = labels, values = values) +
    scale_fill_manual(labels = labels, values = values) +
    xlim(c(init, 100)) +
    theme_minimal()
}

plr1 = imap(list(Hartmann6D = results_rank_hartmann6d, Borehole8D = results_rank_borehole), function(x, nm) {
  init = switch(nm, Branin2D = 10, Currin2D = 10, Hartmann3D = 15, Hartmann6D = 30, Borehole8D = 40)
  plot_ranks(x, instance = nm, init = init, title = FALSE)
})

plr2 = imap(list(Branin2D = results_rank_branin, Currin2D = results_rank_currin, Hartmann3D = results_rank_hartmann3d), function(x, nm) {
  init = switch(nm, Branin2D = 10, Currin2D = 10, Hartmann3D = 15, Hartmann6D = 30, Borehole8D = 40)
  plot_ranks(x, instance = nm, init = init, title = FALSE)
})

g1 = ggarrange(plotlist = c(plt1, plr1), nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")
ggsave("plots/tracesranks1.pdf", plot = g1, device = "pdf", width = 10, height = 6)

g2 = ggarrange(plotlist = c(plt2, plr2), nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom")
ggsave("plots/tracesranks2.pdf", plot = g2, device = "pdf", width = 15, height = 6)

