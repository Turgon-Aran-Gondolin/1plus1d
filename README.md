# `1plus1d`, a Mathematica package for 1+1 dimensional QCD calculation
keywords: 1+1 dimensional; 't Hooft equation


## Usage: 

  Use `SolveScript.wls` first to generate 't Hooft wavefunctions, i.e. `./SolveScript.wls 16` with 16 kernels. 
  
  Then use `Script.wls` to generate scattering amplitude, i.e. `./Script.wls 16` with 16 kernels.
  
  Once established the flavour number, use run.sh to quickly change excited states, i.e. `./run.sh 0 0 0 0`.
  
  Finally use `Display.m` to generate pictures. Use `Msumdat=<<"Path to amp file"` and execute `Row[displayfunction1....]` cell in the Display section to do it. 
