from hpob_handler import HPOBHandler
from methods.random_search import RandomSearch

import numpy as np
import time
import os, psutil
def eval_time(objfun, repls = 1):
    t = [None for _ in range(repls)]
    for i in range(repls):
        t0 = time.time(); 
        objfun()
        t1 = time.time()
        t[i] = t1 - t0
    
    t = np.array([t]).mean()
    print(f'Time taken: {np.round(t, 3)} ms')
    print(f'Memory: {psutil.Process(os.getpid()).memory_info().rss / 1024 ** 2} MB')

def objfun():
    n_trials = 50
    hpob_hdlr = HPOBHandler(root_dir="hpob-data/", mode="v2")
    search_space_id =  '5527' # SVM
    dataset_id = hpob_hdlr.get_datasets(search_space_id)[1] # [0] is broken for some reason
    acc = hpob_hdlr.evaluate(RandomSearch(), search_space_id = search_space_id, dataset_id = dataset_id, n_trials = n_trials, seed = 'test0')
    acc

if __name__ == '__main__':
    eval_time(objfun)