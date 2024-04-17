#!/bin/bash

ssh_options="-o StrictHostKeyChecking=no"
simnumber="4-crossfitting-HD"

for in_input in "not" ; do
    for model_input in "CF" "RF"; do
        for dense_input in "sparse_forest" ; do
            for e_method_input in "TMLE" "Onestep"; do
                for d_method_input in "densratio" "bayes" "dnorm"; do
                    scp $ssh_options aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/ADMGtmle/sim${simnumber}/Y${in_input}L/${dense_input}/${model_input}/${e_method_input}-${d_method_input}/result.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/primal-fixability/code/Simulations/sim${simnumber}/Y${in_input}L/${dense_input}/${model_input}/${e_method_input}-${d_method_input}/result.Rdata"
                done
            done
        done
    done
done
