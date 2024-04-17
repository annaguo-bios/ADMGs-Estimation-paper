#!/bin/bash

#  "bayes" "dnorm"
ssh_options="-o StrictHostKeyChecking=no"
simnumber="1-consistency-univariate"

for in_input in "in" "not"; do
    for e_method_input in "TMLE" "Onestep"; do
        for d_method_input in "densratio" "bayes" "dnorm"; do
            scp $ssh_options aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/ADMGtmle/sim${simnumber}/Y${in_input}L/${e_method_input}-${d_method_input}/result.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/primal-fixability/code/Simulations/sim${simnumber}/Y${in_input}L/${e_method_input}-${d_method_input}/result.Rdata"
        done
    done
done
