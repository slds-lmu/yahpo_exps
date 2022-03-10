library(mfsurrogates)
library(bbotk)
library(paradox)
library(mlr3)
library(mlr3misc)
library(mlr3oml)
library(mlr3learners)
library(mlr3mbo)
library(mlr3hyperband)
library(stringdist)
library(R6)
library(checkmate)
library(data.table)

workdir = "configs/"

# Branin
set.seed(123)
cfg_branin = cfgs("branin")
data_tabular_branin = readRDS(file.path(workdir, "branin_surrogate", "data.rds"))
cfg_branin_surrogate = cfgs("branin_surrogate", workdir = workdir)

# Currin
set.seed(123)
cfg_currin = cfgs("currin")
data_tabular_currin = readRDS(file.path(workdir, "currin_surrogate", "data.rds"))
cfg_currin_surrogate = cfgs("currin_surrogate", workdir = workdir)

# Hartmann3d
set.seed(123)
cfg_hartmann3d = cfgs("hartmann3d")
data_tabular_hartmann3d = readRDS(file.path(workdir, "hartmann3d_surrogate", "data.rds"))
cfg_hartmann3d_surrogate = cfgs("hartmann3d_surrogate", workdir = workdir)

# Hartmann6d
set.seed(123)
cfg_hartmann6d = cfgs("hartmann6d")
data_tabular_hartmann6d = readRDS(file.path(workdir, "hartmann6d_surrogate", "data.rds"))
cfg_hartmann6d_surrogate = cfgs("hartmann6d_surrogate", workdir = workdir)

# Borehole
set.seed(123)
cfg_borehole = cfgs("borehole")
data_tabular_borehole = readRDS(file.path(workdir, "borehole_surrogate", "data.rds"))
cfg_borehole_surrogate = cfgs("borehole_surrogate", workdir = workdir)



SamplerRandomTabular = R6Class("SamplerRandomTabular",
  inherit = Sampler,
  public = list(
    #' @field table [data.table::data.table].
    table = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param table ([data.table::data.table]).
    #' @param x_cols (`character()`).
    #' @param y_cols (`character(1)`).
    initialize = function(table, param_set) {
      self$table = assert_data_table(table)

      super$initialize(
        param_set = param_set
      )
    },

    #' @description
    #' Sample `n` values from the distribution.
    #'
    #' @param n (`integer(1)`).
    #' @return [Design].
    sample = function(n) {
      assert_count(n) # we do argcheck on toplevel
      ids = sample(seq_len(NROW(self$table)), size = min(n, NROW(self$table)), replace = FALSE)
      list(data = self$table[ids, ])
    }
  )
)



OptimizerRandomTabular = R6Class("OptimizerRandomTabular",
  inherit = Optimizer,
  public = list(
    #' @field table [data.table::data.table].
    table = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.

    #' @param table ([data.table::data.table]).
    initialize = function(table) {
      self$table = assert_data_table(table)
      param_set = ps(
        batch_size = p_int(default = 1L, tags = "required"),
        fb_acqo = p_lgl(default = FALSE, tags = "required")
      )
      param_set$values = list(batch_size = 1L, fb_acqo = FALSE)

      super$initialize(
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit")
      )
    }
  ),
  private = list(
    .optimize = function(inst) {
      batch_size = self$param_set$values$batch_size
      table = copy(self$table)
      # Full budget search is implemented below
      if (length(inst$objective$constants$ids())) {
        if (inst$objective$constants$ids() != "fidelity") {
          stop("Not implemented.")
        }
        table = table[fidelity == inst$objective$constants$values$fidelity]
      }
      # Full budget search as acquisition function optimizer
      if (self$param_set$values$fb_acqo) {
        table = table[fidelity == 1]
      }
      x_cols = inst$search_space$ids()

      # FIXME: assert column names
      repeat { # iterate until we have an exception from eval_batch
        if (batch_size > NROW(table)) batch_size = NROW(table)
        if (batch_size > 0) {
          ids = sample(seq_len(NROW(table)), size = min(batch_size, NROW(table)), replace = FALSE)
          design = table[ids, ]
          table = table[-ids, ]
          inst$eval_batch(design[, x_cols, with = FALSE])
        } else {
          inst$eval_batch(design[1, x_cols, with = FALSE])  # this is needed to trigger the exception for termination
        }
      }
    }
  )
)

mlr_optimizers$add("random_tabular", OptimizerRandomTabular)



OptimInstanceSingleCritTabular = R6Class("OptimInstanceSingleCritTabular",
  inherit = OptimInstance,
  public = list(
    #' @field table [data.table::data.table].
    table = NULL,

    #' @field x_cols `character()`.
    x_cols = NULL,

    #' @field y_col `character(1)`.
    y_col = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param max_fidelity (`logical(1)`).
    #' @param table ([data.table::data.table]).
    #' @param x_cols (`character()`).
    #' @param y_col (`character(1)`).
    #' @param search_space ([bbotk::ParamSet]).
    #' @param terminator ([Terminator]).
    #' @param check_values (`logical(1)`)\cr
    #' Should x-values that are added to the archive be checked for validity?
    #' Search space that is logged into archive.
    initialize = function(max_fidelity, table, x_cols, y_col, search_space, direction, terminator, keep_evals = "all", check_values = TRUE) {
      self$x_cols = assert_subset(x_cols, choices = colnames(table))
      self$y_col = assert_choice(y_col, choices = setdiff(colnames(table), x_cols))
      self$table = assert_data_table(table)

      assert_r6(search_space, "ParamSet")
      assert_choice(direction, choices = c("minimize", "maximize"))

      if (max_fidelity) {
        constants = ps(fidelity = p_dbl(lower = 1e-3, upper = 1, default = 1))
        constants$values$fidelity = 1

        objective = ObjectiveRFunDt$new(
          fun = function(xdt, fidelity) {
            on_log = names(which(map_lgl(self$search_space$params, function(param) "log" %in% param$tags)))
            stopifnot(length(on_log) %in% c(0, 1))
            xdt[, fidelity := 1]
            if (length(on_log)) {
              for (i in seq_len(nrow(xdt))) {
                val = xdt[i, ][[on_log]]
                val = self$table[[on_log]][which.min(abs(val - self$table[[on_log]]))]
                xdt[i, (on_log) := val]
              }
            }
            self$table[xdt, self$y_col, with = FALSE, on = c(self$x_cols, "fidelity")]
          },
          domain = search_space,
          constants = constants,
          codomain = ParamSet$new(list(ParamDbl$new(id = self$y_col, tags = direction))),
          check_values = FALSE
        )
      } else {
        objective = ObjectiveRFunDt$new(
          fun = function(xdt) {
            on_log = names(which(map_lgl(self$search_space$params, function(param) "log" %in% param$tags)))
            stopifnot(length(on_log) %in% c(0, 1))
            if (length(on_log)) {
              for (i in seq_len(nrow(xdt))) {
                val = xdt[i, ][[on_log]]
                val = self$table[[on_log]][which.min(abs(val - self$table[[on_log]]))]
                xdt[i, (on_log) := val]
              }
            }
            self$table[xdt, self$y_col, with = FALSE, on = c(self$x_cols, "fidelity")]
          },
          domain = search_space,
          codomain = ParamSet$new(list(ParamDbl$new(id = self$y_col, tags = direction))),
          check_values = FALSE
        )
      }
      super$initialize(objective, NULL, terminator, keep_evals, check_values)
    },

    #' @description
    #' The [Optimizer] object writes the best found point
    #' and estimated performance value here. For internal use.
    #'
    #' @param y (`numeric(1)`)\cr
    #' Optimal outcome.
    assign_result = function(xdt, y) {
      # FIXME: We could have one way that just lets us put a 1xn DT as result directly.
      assert_data_table(xdt)
      assert_names(names(xdt), must.include = self$search_space$ids())
      assert_number(y)
      assert_names(names(y), permutation.of = self$objective$codomain$ids())
      x_domain = unlist(transform_xdt_to_xss(xdt, self$search_space), recursive = FALSE)
      if (is.null(x_domain)) x_domain = list()
      private$.result = cbind(xdt, x_domain = list(x_domain), t(y)) # t(y) so the name of y stays
    }
  )
)



get_ins = function(id, method = c("real", "tabular", "surrogate"), full_budget = FALSE, budget = 100) {
  cfg = cfgs(id)
  cfg_surrogate = cfgs(paste0(id, "_surrogate"), workdir = workdir)
  if (full_budget) {
    budget_param = which(map_lgl(cfg$param_set$tags, function(x) "budget" %in% x))
    search_space = ParamSet$new(cfg$param_set$params[- budget_param])
    search_space$trafo = cfg$param_set$trafo
    search_space$deps = cfg$param_set$deps
    switch(method,
    "real" =
    OptimInstanceSingleCrit$new(
      objective = {
        tmp = cfg$get_objective(max_fidelity = TRUE)
        tmp$constants$values$fidelity = 1
        tmp
      },
      terminator = trm("evals", n_evals = budget),
      search_space = search_space,
      check_values = FALSE
    ),
    "tabular" =
    OptimInstanceSingleCritTabular$new(
      max_fidelity = TRUE,
      table = get(paste0("data_tabular_", id)),
      x_cols = search_space$ids(),
      y_col = cfg$codomain$ids(),
      direction = "minimize",
      terminator = trm("evals", n_evals = budget),
      search_space = search_space,
      check_values = FALSE
    ),
    "surrogate" =
    OptimInstanceSingleCrit$new(
      objective = {
        tmp = cfg_surrogate$get_objective(max_fidelity = TRUE)
        tmp$constants$values$fidelity = 1
        tmp
      },
      terminator = trm("evals", n_evals = budget),
      search_space = search_space,
      check_values = FALSE
    )
  )
  } else {
    search_space = cfg$param_set
    search_space$trafo = cfg$param_set$trafo
    search_space$deps = cfg$param_set$deps
    switch(method,
      "real" =
      OptimInstanceSingleCrit$new(
        objective = cfg$get_objective(),
        terminator = trm("budget", budget = budget),
        search_space = search_space,
        check_values = FALSE
      ),
      "tabular" =
      OptimInstanceSingleCritTabular$new(
        max_fidelity = FALSE,
        table = get(paste0("data_tabular_", id)),
        x_cols = search_space$ids(),
        y_col = cfg$codomain$ids(),
        direction = "minimize",
        terminator = trm("budget", budget = budget),
        search_space = search_space,
        check_values = FALSE

      ),
      "surrogate" =
      OptimInstanceSingleCrit$new(
        objective = cfg_surrogate$get_objective(),
        terminator = trm("budget", budget = budget),
        search_space = search_space,
        check_values = FALSE
      )
    )
  }
}

get_ins_surrogate_size = function(id, size, full_budget = FALSE, budget = 100) {
  cfg = cfgs(paste0(id, "_surrogate"), workdir = workdir)
  path = paste0(workdir, cfg$model_name, "_", gsub("\\.", "_", as.character(size / 1000)), "/")
  cfg$subdir = path
  cfg$workdir = NULL
  if (full_budget) {
    budget_param = which(map_lgl(cfg$param_set$tags, function(x) "budget" %in% x))
    search_space = ParamSet$new(cfg$param_set$params[- budget_param])
    search_space$trafo = cfg$param_set$trafo
    search_space$deps = cfg$param_set$deps
    OptimInstanceSingleCrit$new(
      objective = {
        tmp = cfg$get_objective(max_fidelity = TRUE)
        tmp$constants$values$fidelity = 1
        tmp
      },
      terminator = trm("evals", n_evals = budget),
      search_space = search_space,
      check_values = FALSE
    )
  } else {
    search_space = cfg$param_set
    search_space$trafo = cfg$param_set$trafo
    search_space$deps = cfg$param_set$deps
    OptimInstanceSingleCrit$new(
      objective = cfg$get_objective(),
      terminator = trm("budget", budget = budget),
      search_space = search_space,
      check_values = FALSE
    )
  }
}



get_trace = function(archive, m, o, full_budget = FALSE) {
  tmp = archive$data
  if (full_budget) {
    tmp$fidelity = 1
  }
  tmp[, cumbudget := cumsum(fidelity)]
  tmp[, iteration := seq_len(.N)]
  tmp$best = map_dbl(tmp$iteration, function(i) {
    min(tmp[iteration %in% 1:i][["y"]])
  })
  tmp[, method := m]
  tmp[, optimizer := o]
  tmp[, c("cumbudget", "iteration", "best", "method", "optimizer")]
}



do_real_rs_full = function(id) {
  ins_real = get_ins(id, method = "real", full_budget = TRUE, budget = 100)
  opt("random_search", batch_size = 1)$optimize(ins_real)
  real_rs_full = get_trace(ins_real$archive, "real", "rs_full", full_budget = TRUE)
  real_rs_full[, config := id]
  real_rs_full
}

do_tabular_rs_full = function(id) {
  ins_tabular = get_ins(id, method = "tabular", full_budget = TRUE, budget = 100)
  opt("random_tabular", table = ins_tabular$table, batch_size = 1)$optimize(ins_tabular)
  tabular_rs_full = get_trace(ins_tabular$archive, "tabular", "rs_full", full_budget = TRUE)
  tabular_rs_full[, config := id]
  tabular_rs_full
}

do_surrogate_rs_full = function(id) {
  ins_surrogate = get_ins(id, method = "surrogate", full_budget = TRUE, budget = 100)
  opt("random_search", batch_size = 1)$optimize(ins_surrogate)
  surrogate_rs_full = get_trace(ins_surrogate$archive, "surrogate", "rs_full", full_budget = TRUE)
  surrogate_rs_full[, config := id]
  surrogate_rs_full
}

do_surrogate_size_rs_full = function(id, size) {
  ins_surrogate = get_ins_surrogate_size(id, size = size, full_budget = TRUE, budget = 100)
  opt("random_search", batch_size = 1)$optimize(ins_surrogate)
  surrogate_rs_full = get_trace(ins_surrogate$archive, "surrogate", "rs_full", full_budget = TRUE)
  surrogate_rs_full[, config := id]
  surrogate_rs_full
}




do_real_bo_full = function(id, surrogate, acqopt) {
  ins_real = get_ins(id, method = "real", full_budget = TRUE, budget = 100)
  bayesopt_soo(
    instance = ins_real,
    acq_function = AcqFunctionEI$new(surrogate),
    acq_optimizer = acqopt,
    n_design = 5 * length(ins_real$search_space$ids())
  )
  real_bo_full = get_trace(ins_real$archive, "real", "bo_full", full_budget = TRUE)
  real_bo_full[, acqopt := class(acqopt$optimizer)[1L]]
  real_bo_full[, config := id]
  real_bo_full
}

do_tabular_bo_full = function(id, surrogate, acqopt) {
  ins_tabular = get_ins(id, method = "tabular", full_budget = TRUE, budget = 100)
  tabular_init = ins_tabular$table[fidelity == 1]
  ins_tabular$eval_batch(tabular_init[sample(seq_len(NROW(tabular_init)), size = 5L * length(ins_tabular$search_space$ids()), replace = FALSE), ins_tabular$x_cols, with = FALSE])
  if (acqopt == "default") {
    acqopt = AcqOptimizer$new(opt("random_tabular", table = ins_tabular$table, batch_size = 1000L, fb_acqo = TRUE), trm("evals", n_evals = NROW(tabular_init)))
  } else if (acqopt == "alt") {
    acqopt = AcqOptimizer$new(opt("random_tabular", table = ins_tabular$table, batch_size = 1000L, fb_acqo = TRUE), trm("evals", n_evals = 10000L))
  }    
  bayesopt_soo(
    instance = ins_tabular,
    acq_function = AcqFunctionEI$new(surrogate),
    acq_optimizer = acqopt
  )
  tabular_bo_full = get_trace(ins_tabular$archive, "tabular", "bo_full", full_budget = TRUE)
  tabular_bo_full[, acqopt:= class(acqopt$optimizer)[1L]]
  tabular_bo_full[, config := id]
  tabular_bo_full
}

do_surrogate_bo_full = function(id, surrogate, acqopt) {
  ins_surrogate = get_ins(id, method = "surrogate", full_budget = TRUE, budget = 100)
  bayesopt_soo(
    instance = ins_surrogate,
    acq_function = AcqFunctionEI$new(surrogate),
    acq_optimizer = acqopt,
    n_design = 5 * length(ins_surrogate$search_space$ids())
  )
  surrogate_bo_full = get_trace(ins_surrogate$archive, "surrogate", "bo_full", full_budget = TRUE)
  surrogate_bo_full[, acqopt := class(acqopt$optimizer)[1L]]
  surrogate_bo_full[, config := id]
  surrogate_bo_full
}

do_surrogate_size_bo_full = function(id, size, surrogate, acqopt) {
  ins_surrogate = get_ins_surrogate_size(id, size = size, full_budget = TRUE, budget = 100)
  bayesopt_soo(
    instance = ins_surrogate,
    acq_function = AcqFunctionEI$new(surrogate),
    acq_optimizer = acqopt,
    n_design = 5 * length(ins_surrogate$search_space$ids())
  )
  surrogate_bo_full = get_trace(ins_surrogate$archive, "surrogate", "bo_full", full_budget = TRUE)
  surrogate_bo_full[, acqopt := class(acqopt$optimizer)[1L]]
  surrogate_bo_full[, config := id]
  surrogate_bo_full
}




do_real_hb = function(id) {
  ins_real = get_ins(id, method = "real", full_budget = FALSE, budget = 100)
  opt("hyperband")$optimize(ins_real)
  real_hb = get_trace(ins_real$archive, "real", "hb")
  real_hb[, config := id]
  real_hb
}

do_tabular_hb = function(id) {
  ins_tabular = get_ins(id, method = "tabular", full_budget = FALSE, budget = 100)
  budget_param = which(map_lgl(ins_tabular$search_space$tags, function(x) "budget" %in% x))
  param_set_hb = ParamSet$new(ins_tabular$search_space$params[- budget_param])
  data_tabular = get(paste0("data_tabular_", id))
  opt("hyperband", sampler = SamplerRandomTabular$new(table = data_tabular[, ins_tabular$search_space$ids(), with = FALSE], param_set = param_set_hb))$optimize(ins_tabular)
  tabular_hb = get_trace(ins_tabular$archive, "tabular", "hb")
  tabular_hb[, config := id]
  tabular_hb
}

do_surrogate_hb = function(id) {
  ins_surrogate = get_ins(id, method = "surrogate", full_budget = FALSE, budget = 100)
  opt("hyperband")$optimize(ins_surrogate)
  surrogate_hb = get_trace(ins_surrogate$archive, "surrogate", "hb")
  surrogate_hb[, config := id]
  surrogate_hb
}

do_surrogate_size_hb = function(id, size) {
  ins_surrogate = get_ins_surrogate_size(id, size = size, full_budget = FALSE, budget = 100)
  opt("hyperband")$optimize(ins_surrogate)
  surrogate_hb = get_trace(ins_surrogate$archive, "surrogate", "hb")
  surrogate_hb[, config := id]
  surrogate_hb
}


do_optimize = function(id, method, optimizer, surrogate = NULL, acqopt = NULL) {
  if (method == "real") {
    switch(optimizer,
      "rs_full" = do_real_rs_full(id),
      "bo_full" = do_real_bo_full(id, surrogate = surrogate, acqopt = acqopt),
      "hb" = do_real_hb(id)
    )
  } else if (method == "tabular") {
    switch(optimizer,
      "rs_full" = do_tabular_rs_full(id),
      "bo_full" = do_tabular_bo_full(id, surrogate = surrogate, acqopt = acqopt),
      "hb" = do_tabular_hb(id)
    )
  } else if (method == "surrogate") {
    switch(optimizer,
      "rs_full" = do_surrogate_rs_full(id),
      "bo_full" = do_surrogate_bo_full(id, surrogate = surrogate, acqopt = acqopt),
      "hb" = do_surrogate_hb(id)
    )
  }
}

do_optimize_surrogate_size = function(id, size, optimizer, surrogate = NULL, acqopt = NULL) {
  switch(optimizer,
    "rs_full" = do_surrogate_size_rs_full(id, size = size),
    "bo_full" = do_surrogate_size_bo_full(id, size = size, surrogate = surrogate, acqopt = acqopt),
    "hb" = do_surrogate_size_hb(id, size = size)
  )
}


source("LearnerRegrBananasNN.R")
