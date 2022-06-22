import yahpo_gym

import matplotlib.pyplot as plt
import numpy as np
import ConfigSpace as CS
import ConfigSpace.hyperparameters as CSH
from diskcache import Cache

cache = Cache("cachedir")

def get_surrogate_scores_random(b, rw):
    y_surr = []
    for _ in range(20):
        # import pdb; pdb.set_trace()
        # 100 batches of 1000
        xs = b.get_opt_space(seed = 1).sample_configuration(1000)
        xs = [cf.get_dictionary() for cf in xs]

        # Add fidelity parameter
        fidpar = {}
        for fidn in b.config.fidelity_params:
            fid = b.get_fidelity_space().get_hyperparameter(fidn)
            fidpar.update({fidn:fid.upper})

        for x in xs: x.update(fidpar)
        # Evaluate objective
        y = [x[rw.target[0]] for x in b.objective_function(xs)]
        y_surr += y
    return y_surr

def to_type(x, dtype):
    return np.array(x).astype(dtype)

def filter_configspace(dct, b, target):
    optspace = b.get_opt_space(seed = 1, drop_fidelity_params = True)
    fidn = b.config.fidelity_params
    hnames = optspace.get_hyperparameter_names()
    # Subset only valid configs
    for hpn in hnames:
        hp = optspace.get_hyperparameter(hpn)
        if isinstance(hp, CSH.FloatHyperparameter) or isinstance(hp, CSH.IntegerHyperparameter):
            dct = dct[(dct[hpn] >= hp.lower) & (dct[hpn] <= hp.upper) | dct[hpn].isnull()]
        elif isinstance(hp, CSH.CategoricalHyperparameter):
            dct = dct[dct[hpn].isin(hp.choices) | dct[hpn].isnull()]
        elif isinstance(hp, CSH.Constant):
            dct = dct[dct[hpn].isin(to_type([hp.value], dct[hpn].dtypes).tolist()) | dct[hpn].isnull()]
        print(f"{hpn} {len(dct)}")
    
    return dct

def filter_max_fidelity(data,b,target):
    fidn = b.config.fidelity_params[0]
    fid = b.get_fidelity_space().get_hyperparameter(fidn)
    # Subset maximum fidelitiy
    data = data[data[fidn] == fid.upper]
    return data

@cache.memoize()
def get_tabular_scores(rw):
    yahpo_gym.BenchmarkSet(rw.scenario, rw.instance)
    data = b.config.data
    optspace = b.get_opt_space(seed = 1, drop_fidelity_params = True)
    data = data[b.config.fidelity_params+optspace.get_hyperparameter_names()+rw.target].sample(n = min(len(data), int(5*1e6))).copy()
    if "iaml" in rw.scenario or "rbv2" in rw.scenario or  "lcbench" in rw.scenario:
        data = filter_configspace(data, b, rw.target)
    data = filter_max_fidelity(data, b, rw.target)

    data = data.sample(n = min(int(1e5), len(data)))
    ys = data[rw.target[0]].values

    # # From surrogate
    # optspace = b.get_opt_space(seed = 1, drop_fidelity_params = True)
    # fidn = b.config.fidelity_params
    # hnames = optspace.get_hyperparameter_names()
    # # Task to str
    # if "nb301" not in rw.scenario:
    #     data[b.config.instance_names] = data[b.config.instance_names].astype('str')
    # # Minor fixes to data types
    # if "rbv2_ranger" in rw.scenario or "rbv2_super" in rw.scenario: 
    #     data["num.random.splits"] = data["num.random.splits"].astype('int')

    # y = [x[rw.target[0]] for x in b.objective_function(data[hnames+fidn].to_dict("records"))]
    return ys, 0

def get_ecdf(x):
    x = np.sort(x)
    y = np.arange(len(x))/float(len(x))
    return x, y
    

if __name__ == '__main__':

    single = yahpo_gym.get_suite('single', version=1.0)
    print(single)
    
    fig, axes = plt.subplots(ncols=4, nrows=5, figsize=(10, 15))
    
    for j in range(20): 

        rw = single.iloc[j,:]
        b = yahpo_gym.BenchmarkSet(rw.scenario, rw.instance)
        scenario = ""

        # Load data
        if not scenario == rw.scenario:
            scenario = rw.scenario
        
        ax = axes.ravel()[j]
        ytb, ytbs = get_tabular_scores(rw)
        ysr = get_surrogate_scores_random(b, rw)
        x1,y1 = get_ecdf(ytb)
        x2,y2 = get_ecdf(ysr)
        # x3,y3 = get_ecdf(ytbs)
        ax.plot(x1, y1, "orange")
        ax.plot(x2, y2, "blue")
        # ax.plot(x3, y3, "green") 
        axes.ravel()[j].set_title(f'{rw.scenario}:{rw.instance}')

    plt.tight_layout()
    plt.savefig(f"ecdf_tabular_surrogates.pdf") 
    # plt.show(block=False)
