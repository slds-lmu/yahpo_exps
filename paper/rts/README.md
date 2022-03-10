This directory contains all code to replicate all results, tables and figures of the real vs. tabular vs. surrogate section.

Note that experiments were conducted with a [pre-alpha version](https://github.com/slds-lmu/paper_2021_multi_fidelity_surrogates) of YAHPO GYM.

* `configs/` contains data needed for the surrogates
* `plots/` contains all plots presented in the paper
* `results/` contains raw benchmark results saved as `.rds` files
* `run_experiments.R` is the main file
* `analyze_experiments.R` contains code needed to analyze the results and create plots and tables
* `LearnerRegrBananasNN.R`, `nn_ensemble.py`, and `real_tabular_surrogate_helpers.R` are helper files that contain code needed by the main file
* `registry_rts_clean.zip` is the original batchtools registry as used on the cluster (stripped of the individual results which are collected in `results/` and stripped of the logs to save some space); see `run_experiments.R` for an example on how to inspect this registry
* `renv.lock` lists the exact R packages that were used on the cluster
* `requirements.txt` lists the exact python modules required for Naszilla and its NN ensemble surrogate

R 4.0.2 and python 3.7.10 (via reticulate) were used.

To replicate our results, install R and do the following steps (we assume that you work in a terminal and are in the current directory):

Then, within R (we assume that you managed to install R 4.0.2 or use a docker environment):

```r
install.packages("renv")  # 0.15.4
renv::restore(lockfile = "renv.lock")
```

Close the R session, followed by setting up python and a virtualenv (we use pyenv and pipenv here).
Naszilla is required for the NN ensemble surrogate (no explicit install is needed though but we still need the requirements):

```bash
CONFIGURE_OPTS=--enable-shared pyenv install 3.7.10  # enable-shared must be set due to reticulate
pipenv --python 3.7.10
pipenv shell
pip install -r requirements.txt  # up to date Naszilla requirements
git clone https://github.com/naszilla/naszilla.git
cd naszilla
git checkout 58ec28d864f8ac961b83cc7e617cf5d426e931b4
pip install -e.
```

Finally, if you want to rerun experiments, make sure to adapt some paths to point at your newly created virtualenv, i.e., in `LearnerRegrBananasNN.R`.
We indicate this via `"FIXME_python_path_of_your_virtualenv"` as a placeholder.

