#!/usr/bin/env python3
import random 
from math import log1p

rx = [
    # gene          # dependencies    # signals     # modifications                       # rate 
    ("gene1",       ["g1"],           ["s1"],       [("g1", -1), ("ga1", 1)],             0.1),
    ("gene1",       ["ga1"],          [],           [("g1", 1), ("ga1", -1)],             0.05),
    ("gene1",       ["ga1"],          [],           [("m1", 1)],                          0.1),
    ("gene1",       ["m1"],           [],           [("m1", -1)],                         0.01),
    ("gene1",       ["m1"],           [],           [("p1", 1)],                          0.2),
    ("gene1",       ["p1"],           [],           [("p1", -1)],                         0.01),

    ("gene2",       ["g2", "p1"],     [],           [("g2", -1), ("ga2", 1), ("p1", -1)], 0.1),
    ("gene2",       ["ga2"],          [],           [("g2", 1), ("ga2", -1), ("p1", 1)],  0.05),
    ("gene2",       ["ga2"],          [],           [("m2", 1)],                          0.1),
    ("gene2",       ["m2"],           [],           [("m2", -1)],                         0.01),
    ("gene2",       ["m2"],           [],           [("p2", 1)],                          0.2),
    ("gene2",       ["p2"],           [],           [("p2", -1)],                         0.01),
]

signals_1 = {"s1" : 0.1}
signals_2 = {"s1" : 1.0}

initial_state = {
    "g1"  : 1, 
    "ga1" : 0, 
    "m1"  : 0, 
    "p1"  : 0, 

    "g2"  : 1, 
    "ga2" : 0, 
    "m2"  : 0, 
    "p2"  : 0, 
}


def propensity(rx, state, signals):
    a = []
    for gene, depends, sig, mods, rate in rx:
        if len(depends) > 1 and depends[0] == depends[1]:
            a.append(state[depends[0]] * (state[depends[1]] - 1) * rate/2)
        else:
            p = 1
            for m in depends:
                p = p * state[m] 

            for s in sig:
                p = p * signals[s]
            a.append(p * rate)
    return a, sum(a)

def update_time(t, b):
    t = t + log1p(1 / random.uniform(0,1)) / b
    return t

def reaction(rx, state, signals, t):
    a, b = propensity(rx, state, signals)
    a_sorted = sorted(list(enumerate(a)), key=lambda x: x[1], reverse=True)
    rb = random.uniform(0, 1) * b
	
    m, MU = 0, 0 	
    for i, p in a_sorted:
        m = m + p 
        if m > rb:
            MU = i
            break
    
    _, _, _, mods, _ = rx[MU]
    for mol, count in mods:
        state[mol] += count
    
    t = update_time(t, b)
    return t


def simulate(rx, initial_state, signals, t_stop, t_check, n):
    t = 0
    state = initial_state.copy()
    results = []

    if t_stop == None:
        n_ss = 2 
        t_stop = float(n_ss)/min(rate for _, _, _, _, rate in rx)
     
    for i in range(n):
        run_results = []
        while t < t_stop:
            t = reaction(rx, state, signals, t)
            if t > t_check:
                run_results.append((t, state.copy()))
                print(t, state)

        results.append(run_results)
    return results

def main():
    t_check = 0.1
    t_stop = None 
    n = 10
    
    results_1, results_2 = simulate(rx, initial_state, signals_1, t_stop, t_check, n), \
                           simulate(rx, initial_state, signals_2, t_stop, t_check, n)
    
    for results in (results_1, results_2):
        print(results)
        p2_dist = [run_results[-1][1]['p2'] for run_results in results]
        p2_mean = sum(p2_dist)/float(len(p2_dist))
        print(p2_mean)


if __name__ == '__main__':
    main()
