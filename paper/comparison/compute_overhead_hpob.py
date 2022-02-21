# Run with python -m memory_profiler compute_overhead_hpob.py
from HPOB.hpob_handler import HPOBHandler
from memory_profiler import profile

import numpy as np
import time
import os, psutil
def eval_time(objfun, repls = 5):
    t = [None for _ in range(repls)]
    for i in range(repls):
        t0 = time.time(); 
        objfun()
        t1 = time.time()
        t[i] = t1 - t0
    
    t = np.array([t]).mean()
    print(f'Time taken: {np.round(t, 3)} ms')

import random
import numpy as np

class RandomSearch:
    def __init__(self):
        print("Using random search method...")

    def observe_and_suggest(self, X_obs, y_obs, X_pen=None):
        if X_pen is not None:
            size_pending_eval = len(X_pen)
            idx = random.randint(0, size_pending_eval-1)
            return idx

        else:
            dim = len(X_obs[0])
            bounds = tuple([(0,1) for i in range(dim)])
            x_new = np.array([random.uniform(lower, upper) for upper, lower in bounds]).reshape(-1, dim)

            return x_new



@profile
def objfun():
    n_trials = 300
    hpob_hdlr = HPOBHandler(root_dir="HPOB/hpob-data/", mode="v2")
    search_space_id =  '5527' # SVM
    dataset_id = hpob_hdlr.get_datasets(search_space_id)[1] # [0] is broken for some reason
    acc = hpob_hdlr.evaluate(RandomSearch(), search_space_id = search_space_id, dataset_id = dataset_id, n_trials = n_trials, seed = 'test0')
    acc

if __name__ == '__main__':
    eval_time(objfun)
