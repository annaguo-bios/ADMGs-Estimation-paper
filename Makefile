sim1-consistency/DGPs/Y${input}L-truth.Rdata: sim1-consistency/DGPs/compute_truth.R sim1-consistency/DGPs/Y${input}L-truth.R sim1-consistency/DGPs/Y${input}L-dgp.R
	Rscript sim1-consistency/DGPs/compute_truth.R sim1-consistency/DGPs/Y${input}L-truth.R sim1-consistency/DGPs/Y${input}L-dgp.R sim1-consistency/DGPs/Y${input}L-truth.Rdata 10000

sim1-consistency/Y${input}L/joblist_n1.txt: sim1-consistency/Y${input}L/write_job.R sim1-consistency/Y${input}L/main.R sim1-consistency/DGPs/Y${input}L-truth.Rdata
	Rscript sim1-consistency/Y${input}L/write_job.R
	
.PHONY:clean
clean:
	rm -f sim*/DGPs/*.Rdata
