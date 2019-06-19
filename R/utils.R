most_frequent = function(x){
  ux = unique(x)
  freqs = tabulate(match(x, ux))
  ux[which.max(freqs)]
}
