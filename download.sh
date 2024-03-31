# ssh_options="-o StrictHostKeyChecking=no"

# echo "TMLE or Onestep. 1:TMLE, 2: Onestep"
# read e_method_input

# case $e_method_input in
#     1) e_method="TMLE" ;;
#     2) e_method="Onestep" ;;
#     *) echo "Invalid input for e_method"; exit 1 ;;
# esac

# echo "What is the simulation number. 1: 1-consistency, 2: 2-weak-overlap, 3:3-misspecification, 4-crossfitting"
# read simnumber_input

# case $simnumber_input in
#     1) simnumber="1-consistency" ;;
#     2) simnumber="2-weak-overlap" ;;
#     3) simnumber="3-misspecification" ;;
#     4) simnumber="4-crossfitting" ;;
#     *) echo "Invalid input for simnumber"; exit 1 ;;
# esac

# echo "Y in L or Y not in L. 1:in, 2: not in"
# read in_input

# case $in_input in
#     1) in="in" ;;
#     2) in="not" ;;
#     *) echo "Invalid input for in"; exit 1 ;;
# esac

# echo "What is the density ratio estimation method. 1: bayes, 2:dnorm, 3:densratio."
# read d_method_input

# case $d_method_input in
#     1) d_method="bayes" ;;
#     2) d_method="dnorm" ;;
#     3) d_method="densratio" ;;
#     *) echo "Invalid input for d_method"; exit 1 ;;
# esac

# scp aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/ADMGtmle/sim${simnumber}/Y${in}L/${e_method}-${d_method}/result.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/primal-fixability/code/Simulations/sim${simnumber}/Y${in}L/${e_method}-${d_method}/result.Rdata"
ssh_options="-o StrictHostKeyChecking=no"

echo "TMLE or Onestep. 1:TMLE, 2: Onestep"
read e_method_input

case $e_method_input in
    1) e_method="TMLE" ;;
    2) e_method="Onestep" ;;
    *) echo "Invalid input for e_method"; exit 1 ;;
esac

echo "What is the simulation number. 1: 1-consistency, 2: 2-weak-overlap, 3:3-misspecification, 4-crossfitting"
read simnumber_input

case $simnumber_input in
    1) simnumber="1-consistency" ;;
    2) simnumber="2-weak-overlap" ;;
    3) simnumber="3-misspecification" 
       echo "Model method. 1: CF, 2: linear, 3: SL"
       read model_input
       case $model_input in
           1) model="CF" ;;
           2) model="linear" ;;
           3) model="SL" ;;
           *) echo "Invalid input for model"; exit 1 ;;
       esac
       ;;
    4) simnumber="4-crossfitting"
       echo "Model method. 1: CF, 2: RF"
       read model_input
       case $model_input in
           1) model="CF" ;;
           2) model="RF" ;;
           *) echo "Invalid input for model"; exit 1 ;;
       esac
       echo "Dense method. 1: dense_forest, 2: sparse_forest"
       read dense_input
       case $dense_input in
           1) dense="dense_forest" ;;
           2) dense="sparse_forest" ;;
           *) echo "Invalid input for dense"; exit 1 ;;
       esac
       ;;
    *) echo "Invalid input for simnumber"; exit 1 ;;
esac

echo "Y in L or Y not in L. 1:in, 2: not in"
read in_input

case $in_input in
    1) in="in" ;;
    2) in="not" ;;
    *) echo "Invalid input for in"; exit 1 ;;
esac

echo "What is the density ratio estimation method. 1: bayes, 2: dnorm, 3: densratio."
read d_method_input

case $d_method_input in
    1) d_method="bayes" ;;
    2) d_method="dnorm" ;;
    3) d_method="densratio" ;;
    *) echo "Invalid input for d_method"; exit 1 ;;
esac

# Modify SCP command based on simnumber
if [ "$simnumber" = "3-misspecification" ]; then
    scp aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/ADMGtmle/sim${simnumber}/Y${in}L/${model}/${e_method}-${d_method}/result.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/primal-fixability/code/Simulations/sim${simnumber}/Y${in}L/${model}/${e_method}-${d_method}/result.Rdata"
elif [ "$simnumber" = "4-crossfitting" ]; then
    scp aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/ADMGtmle/sim${simnumber}/Y${in}L/${dense}/${model}/${e_method}-${d_method}/result.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/primal-fixability/code/Simulations/sim${simnumber}/Y${in}L/${dense}/${model}/${e_method}-${d_method}/result.Rdata"
else
    scp aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/ADMGtmle/sim${simnumber}/Y${in}L/${e_method}-${d_method}/result.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/primal-fixability/code/Simulations/sim${simnumber}/Y${in}L/${e_method}-${d_method}/result.Rdata"
fi
