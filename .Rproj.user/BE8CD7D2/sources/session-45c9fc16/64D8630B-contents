sim1-consistency/DGPs/Y${input}L-truth.Rdata: sim1-consistency/DGPs/compute_truth.R sim1-consistency/DGPs/Y${input}L-truth.R sim1-consistency/DGPs/Y${input}L-dgp.R
	Rscript sim1-consistency/DGPs/compute_truth.R sim1-consistency/DGPs/Y${input}L-truth.R sim1-consistency/DGPs/Y${input}L-dgp.R sim1-consistency/DGPs/Y${input}L-truth.Rdata 10000

	
.PHONY:clean
clean:
	rm -f sim*/DGPs/*.Rdata
