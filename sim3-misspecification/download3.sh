ssh_options="-o StrictHostKeyChecking=no"

simnumber_input="3-misspecification"

for in_input in "YnotL"; do
    for model_input in "CF" "Linear" "SL"; do
        for e_method_input in "TMLE" "Onestep"; do
            for d_method_input in "densratio" "bayes" "dnorm"; do
                scp $ssh_options aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/ADMGtmle/sim${simnumber_input}/${in_input}/${model_input}/${e_method_input}-${d_method_input}/result.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/primal-fixability/code/Simulations/sim${simnumber_input}/${in_input}/${model_input}/${e_method_input}-${d_method_input}/result.Rdata"
            done
        done
    done
done
