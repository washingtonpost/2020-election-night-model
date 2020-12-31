
boot_sd = function(d, i) {
  d2 = d[i]
  return(sd(d2))
}

boot_mean = function(d, i, wt) {
  d2 = d[i]
  wt2 = wt[i]
  
  return(sum(d2 * wt2) / sum(wt2))
}

boot_sigma = function(d, conf, R=1000) {
  sigma_ci = boot.ci(boot(d, boot_sd, R=R), conf = conf, type=c("basic"))
  sigma = sigma_ci$basic[5]
  return(sigma)
}