import yahpo_gym
import matplotlib.pyplot as plt
import numpy as np
import ConfigSpace as CS
import ConfigSpace.hyperparameters as CSH


def get_surrogate_scores(b, rw):
    y_surr = []
    for _ in range(10):
        # 100 batches of 1000
        xs = b.get_opt_space(seed = 1).sample_configuration(1000)
        y = [x[rw.target[0]] for x in b.objective_function(xs)]
        y_surr += y
    return y_surr

def filter_configspace(data, b, target):
    optspace = b.get_opt_space(seed = 1, drop_fidelity_params = True)

    fidn = b.config.fidelity_params[0]
    fid = b.get_fidelity_space().get_hyperparameter(fidn)
    
    hnames = optspace.get_hyperparameter_names()
    
    dct = data[hnames+[fidn]+target].copy()
    # Subset maximum fidelitiy
    dct = dct[dct[fidn] == fid.upper]
    # Subset only valid configs
    for hpn in hnames:
        hp = optspace.get_hyperparameter(hpn)
        if isinstance(hp, CSH.FloatHyperparameter) or isinstance(hp, CSH.IntegerHyperparameter):
            dct = dct[(dct[hpn] >= hp.lower) & (dct[hpn] <= hp.upper)]
        elif isinstance(hp, CSH.CategoricalHyperparameter):
            dct = dct[dct[hpn].isin(hp.choices)]
        elif isinstance(hp, CSH.Constant):
            dct = dct[dct[hpn] == int(hp.value)]
    
    return dct
    
def get_tabular_scores(b, rw, data):
    if "iaml" in rw.scenario or "rbv2" in rw.scenario:
        data = filter_configspace(data, b, rw.target)
    ys = data.sample(n = min(int(1e4), len(data)))[rw.target[0]].values
    return ys

def get_ecdf(x):
    x = np.sort(x)
    y = np.arange(len(x))/float(len(x))
    return x, y
    

if __name__ == '__main__':
    single = yahpo_gym.get_suite('single', version=1.0)
    print(len(single))
    
    fig, axes = plt.subplots(ncols=4, nrows=5, figsize=(10, 15))
    
    for j in range(20): 
        rw = single.iloc[j,:]
        b = yahpo_gym.BenchmarkSet(rw.scenario, rw.instance)
        scenario = ""

        # Load data
        if not scenario == rw.scenario:
            scenario = rw.scenario
            data = b.config.data
        

        ax = axes.ravel()[j]   
        ytb = get_tabular_scores(b, rw, data)
        x1,y1 = get_ecdf(ytb)
        ysr = get_surrogate_scores(b, rw)
        x2,y2 = get_ecdf(ysr)
        ax.plot(x1, y1, "orange")
        ax.plot(x2, y2, "blue")
        axes.ravel()[j].set_title(f'{rw.scenario}:{rw.instance}')

    plt.tight_layout()
    plt.show()
    plt.savefig(f"ecdf_tabular_surrogates.png") 
    



