sim${sim}/DGPs/Y${input}L-truth.Rdata: sim${sim}/DGPs/compute_truth.R sim${sim}/DGPs/Y${input}L-truth.R sim${sim}/DGPs/Y${input}L-dgp.R
	Rscript sim${sim}/DGPs/compute_truth.R sim${sim}/DGPs/Y${input}L-truth.R sim${sim}/DGPs/Y${input}L-dgp.R sim${sim}/DGPs/Y${input}L-truth.Rdata 500000

	
.PHONY:clean
clean:
	rm -f sim*/DGPs/*.Rdata
