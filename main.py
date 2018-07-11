#!/usr/bin/env python3
import random
from random import randint
from math import log1p
from math import exp
import numpy as np

rx = [
    # gene          # dependencies    # signals     # modifications                       # rate
    ["gene1",       ["g1"],           ["s1"],       [("g1", -1), ("ga1", 1)],             0.1],
    ["gene1",       ["ga1"],          [],           [("g1", 1), ("ga1", -1)],             0.05],
    ["gene1",       ["ga1"],          [],           [("m1", 1)],                          0.1],
    ["gene1",       ["m1"],           [],           [("m1", -1)],                         0.01],
    ["gene1",       ["m1"],           [],           [("p1", 1)],                          0.2],
    ["gene1",       ["p1"],           [],           [("p1", -1)],                         0.01],

    ["gene2",       ["g2", "p1"],     [],           [("g2", -1), ("ga2", 1), ("p1", -1)], 0.1],
    ["gene2",       ["ga2"],          [],           [("g2", 1), ("ga2", -1), ("p1", 1)],  0.05],
    ["gene2",       ["ga2"],          [],           [("m2", 1)],                          0.1],
    ["gene2",       ["m2"],           [],           [("m2", -1)],                         0.01],
    ["gene2",       ["m2"],           [],           [("p2", 1)],                          0.2],
    ["gene2",       ["p2"],           [],           [("p2", -1)],                         0.01],
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
            dep = 1
            for m in depends:
                dep = dep * state[m]

            for s in sig:
                dep = dep * signals[s]
            a.append(dep * rate)
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

def stoch_sim(rx, initial_state, signals, t_stop, t_check, n):
    results = []

    if t_stop == None:
        n_ss = 2
        ode_result = ode_sim(rx, initial_state, signals_1, t_stop, t_check)
        x = []
        for gene, depends, sig, mods, rate in rx:
            dep = 1.0
            for s in sig:
                dep = dep * signals[s]
            for d in depends:
                dep = dep * ode_result[-1][-1][1][d]
            x.append(rate * dep)
        t_stop = float(n_ss)/min(x)

    for i in range(n):
        t = 0
        run_results = []
        state = initial_state.copy()
        while t < t_stop:
            t = reaction(rx, state, signals, t)
            if t > t_check:
                run_results.append((t, state.copy()))

        results.append(run_results)
    return results

def ode_sim(rx, initial_state, signals, t_stop, t_check):
    state = initial_state.copy()
    state_temp = state.copy()
    results = []

    if t_stop == None:
        n_ss = 2
        t_stop = float(n_ss)/min(rate for _, _, _, _, rate in rx)

    t = 0
    if 0.001/max(rate for _, _, _, _, rate in rx) <= 0.1:
        dt = 0.001/max(rate for _, _, _, _, rate in rx)
    else:
        dt = 0.1

    run_results = []
    while t < t_stop:
        for gene, depends, sig, mods, rate in rx:
            dep = 1
            for mol in depends:
                dep = dep * state[mol]
            for s in sig:
                dep = dep * signals[s]

            for mol, sign in mods:
                state_temp[mol] += rate * dep * sign * dt

        state = state_temp.copy()

        t = t + dt
        if t > t_check:
            run_results.append((t, state.copy()))

        results.append(run_results)
    return results

def gradient(rx, initial_state, signals_1, signals_2, t_stop, t_check):
    results_1, results_2 = ode_sim(rx, initial_state, signals_1, t_stop, t_check), ode_sim(rx, initial_state, signals_2, t_stop, t_check)
    bo = abs(results_1[-1][-1][1]['p2'] - results_2[-1][-1][1]['p2'])
    rmax = 1.0
    fmin = 2.0
    alpha = True
    while alpha == True:
        beta = True
        while beta == True:
            m, r, p = randint(0,len(rx)-1), random.uniform(0.5,0.7), random.choice([-1,1])
            rate = rx[m][4]
            rate = rate * (1 + p * r)
            if rate < rmax:
                beta = False
                rx[m][4] = rate
            else:
                rate = rate / (1 + p * r)
        results_1n, results_2n = ode_sim(rx, initial_state, signals_1, t_stop, t_check), ode_sim(rx, initial_state, signals_2, t_stop, t_check)
        bn = abs(results_1n[-1][-1][1]['p2'] - results_2n[-1][-1][1]['p2'])
        if bo > bn:
            rate = rate / (1 + p * r)
            rx[m][4] = rate
        if bo * fmin < bn:
            alpha = False
    return rx

def accept_prob(bo, bn, T):
    if bn < bo:
        prob = 1.0
    else:
        prob = exp(-abs(bo - bn) / T)
    return prob

def calc_overlap(results_1, results_2):
    p2_values_1 = [run_results[-1][1]['p2'] for run_results in results_1]
    p2_values_2 = [run_results[-1][1]['p2'] for run_results in results_2]
    p2_max, overlap_1, overlap_2, overlap = max(p2_values_2), 0, 0, 0
    bin_size = int(p2_max / 100)
    if bin_size == 0:
        bin_size = 1
    for i in range(0,p2_max,bin_size):
        p2_bin_1, p2_bin_2 = 0, 0
        for j in range(i,i+bin_size):
            p2_bin_1 += p2_values_1.count(j)
            p2_bin_2 += p2_values_2.count(j)
        if p2_bin_2 > 0:
            overlap_1 += p2_bin_1 / float(len(p2_values_1))
        if p2_bin_1 > 0:
            overlap_2 += p2_bin_2 / float(len(p2_values_2))
    overlap = max(overlap_1,overlap_2)
    return overlap

def param_opt_gradient(rx, initial_state, signals_1, signals_2, t_stop, t_check, n):
    overlap = 1.0
    while overlap >= 0.05:
        gradient(rx, initial_state, signals_1, signals_2, t_stop, t_check)
        results_1, results_2 = stoch_sim(rx, initial_state, signals_1, t_stop, t_check, n), \
                           stoch_sim(rx, initial_state, signals_2, t_stop, t_check, n)
        overlap = calc_overlap(results_1, results_2)
        print(overlap)
    return results_1, results_2

def param_opt_anneal(rx, initial_state, signals_1, signals_2, t_stop, t_check, n):
    rmax = 1.0
    T = 10.0
    cooling_rate = 0.3
    results_1, results_2 = stoch_sim(rx, initial_state, signals_1, t_stop, t_check, n), stoch_sim(rx, initial_state, signals_2, t_stop, t_check, n)
    bo = calc_overlap(results_1, results_2)
    results_1n, results_2n = None, None
    while bo > 0.5:
        beta = True
        while beta == True:
            m, r, p = randint(0,len(rx)-1), random.uniform(0.5,0.7), random.choice([-1,1])
            rate = rx[m][4]
            rate = rate * (1 + p * r)
            if rate > 0 and rate < rmax:
                beta = False
                rx[m][4] = rate
            else:
                rate = rate / (1 + p * r)
        results_1n, results_2n = stoch_sim(rx, initial_state, signals_1, t_stop, t_check, n), stoch_sim(rx, initial_state, signals_2, t_stop, t_check, n)
        bn = calc_overlap(results_1n, results_2n)
        if T > 0.001:
            prob = accept_prob(bo, bn, T)
        else:
            prob = 0
        ran = random.uniform(0,1)
        if ran > prob:
            rate = rate / (1 + p * r)
            rx[m][4] = rate
        else:
            bo = calc_overlap(results_1n, results_2n)
        T = T * (1 - cooling_rate)
        print(T,bo,ran,prob)
    return results_1n, results_2n

def main():
    t_check = 0.1
    t_stop = None
    n = 100

    results_1, results_2 = param_opt_anneal(rx, initial_state, signals_1,signals_2,t_stop,t_check, n)

    for results in (results_1, results_2):
        p2_dist = [run_results[-1][1]['p2'] for run_results in results]
        p2_mean = sum(p2_dist)/float(len(p2_dist))
        print(p2_mean)


if __name__ == '__main__':
    main()
