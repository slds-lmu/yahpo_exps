from yahpo_gym import *
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import math


def plot_true_lc(ax, ccs, df, bench, metric, color):
    """
        Get all available values for config 'cc' from the data and plot them.
    """
    config = ccs[bench.config_space.get_hyperparameter_names()].copy()
    config.drop(bench.config.fidelity_params[0], axis = 1, inplace=True)
    pdf = df.merge(config, on=list(config.columns.values))
    pdf = pdf.sort_values(bench.config.fidelity_params[0])
    ax.plot(pdf[bench.config.fidelity_params[0]], pdf[metric], marker = ".", color=color, linestyle='dashed', alpha =.5)


def plot_pred_lc(ax, cc, df, bench, metric, color):
    fidelities = df[bench.config.fidelity_params[0]].unique()
    if len(fidelities) < 30:
        fmin = fidelities.min()
        fmax = fidelities.max()
        fidelities = np.linspace(fmin, fmax, 50)

    fidelities = np.sort(fidelities)
    cc = {k:v for k,v in cc.items() if v is not np.nan}

    # List of configurations to evaluate
    out = []
    for fid in fidelities:
        val = cc.copy()
        val[bench.config.fidelity_params[0]] = fid
        out += [val]

    # Evaluate the objective function
    res = bench.objective_function(out, seed=0)
    res = [x[metric[0]] for x in res]
    ax.plot(fidelities, res, color=color)

def check_nan(item):
    item = list(item.values())[0]
    if type(item) is float:
        return math.isnan(item)
    return False


def plot_figure(axes, df, x):
    colors = ['red', 'blue', 'orange', 'purple', 'green', 'gold', 'magenta', 'darkviolet', 'cyan', 'olive']
    bench = x.bset
    if bench.config.instance_names is not None:
        df2 = df[df[bench.config.instance_names] == x.instance]
    else:
        df2 = df

    for i in range(4):
        if x.scenario[0:4] == 'rbv2':
            ccs = df2[(df2.trainsize < 1.0) & (df2[x.target[0]] > 0.5)].sample(1) # Only those with superevals, we also get rid of very extreme configuration
        else:
            ccs = df2.sample(1)
        cc = ccs.to_dict()
        cc = {k:list(cc[k].values())[0] for k in bench.config_space.get_hyperparameter_names() if not check_nan(cc[k])}
        print(cc)
        j = int(x.name)
        ax = axes.ravel()[j]
        plot_true_lc(ax, ccs, df2, bench, x.target, colors[i])
        plot_pred_lc(ax, cc, df2, bench, x.target, colors[i])
    
    
    #axes.ravel()[j].xlabel(bench.config.fidelity_params[0])
    #axes.ravel()[j].ylabel(x.target[0])
    axes.ravel()[j].set_title(f'{x.scenario}:{x.instance}')
    # axes.ravel()[j].set_axis_off()

    # plt.savefig(f"learning_curves_{x.scenario}_{x.instance}.pdf")
    # plt.show(block=False)
    # plt.close()



if __name__ == '__main__':

    suite = get_suite('single').sample(frac = 1.0)
    # suite = suite[suite.scenario.isin(['rbv2_ranger', 'rbv2_glmnet'])]
    suite['bset'] = [BenchmarkSet(x.scenario, x.instance, check = False) for v, x in suite.iterrows()]
    fig, axes = plt.subplots(ncols=2, nrows=10, figsize=(10, 15))
    for j, scenario in enumerate(suite.scenario.unique()):
        print('scenario:', scenario)
        x0 = suite[suite.scenario == scenario].iloc[0,]
        df = pd.read_csv(x0.bset.config.get_path("dataset"), low_memory=False)
        if x0.bset.config.instance_names is not None:
            df[x0.bset.config.instance_names] = df[x0.bset.config.instance_names].astype('str')

        for i, x in suite[suite.scenario == scenario].iterrows():  
            plot_figure(axes, df, x)
            plt.show(block=False)

    plt.tight_layout()
    plt.savefig(f"learning_curves_all.pdf") 






