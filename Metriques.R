
abd = function(dat, cov, weight, a) {
  b= dat[,a];
  w = dat[, weight];
  cov = dat[,cov];
  abs(weighted.mean(cov[b == 1], w = w[b== 1])-
     weighted.mean(cov[b == 0], w = w[b== 0]));
}

std = function(dat, cov, weight, a) {
  b= dat[,a];
  w = dat[, weight];
  cov = dat[,cov];
  s = sqrt((sum(w*(b == 1))*cov.wt(as.matrix(cov[b ==1]), 
    wt = w[b==1])$cov+sum(w*(b == 0))*cov.wt(as.matrix(cov[b==0]),
    wt = w[b==0])$cov)/sum(w));
  abs(weighted.mean(cov[b == 1], w = w[b == 1])-
      weighted.mean(cov[b == 0], w = w[b == 0]))/s;
}

ld = function(dat, cov, weight, a) {
  b= dat[,a];
  cov = dat[,cov];
  w = dat[, weight];
  F1 = ewcdf(cov[b == 1],w = w[b == 1]);
  F0 = ewcdf(cov[b == 0],w = w[b == 0]);
  e = max(abs(F1(cov)-F0(cov)));
  if(length(unique(cov))<= 10) return(e);
  x = seq(min(cov), max(cov),length.out = 1000);
  check = all(F0(x-e)-e<=F1(x) & F1(x)<=F0(x+e)+e);
  while(check) {
    e = e-.01;
    check = all(F0(x-e)-e<=F1(x)&F1(x)<=F0(x+e)+e);
  }
  e;
}

ovl = function(dat, cov, weight, a) {
  b= dat[,a];
  cov = dat[,cov];
  w = dat[, weight];
  if(length(unique(cov)) <= 10) {
    pt = apply(prop.table(table(cov,b), 2), 1, min);
    return(1-sum(pt)); # reversed to measure imbalance
  }
  mn = weighted.quantile(cov,w,0.05);
  mx = weighted.quantile(cov,w,0.95);
  m1=w[b == 1]
  m0=w[b == 0]
  f1 = approxfun(density(cov[b == 1], 
    weights = m1/sum(m1),from = mn, to = mx, bw = "nrd0")); 
  f0 = approxfun(density(cov[b == 0], 
    weights = m0/sum(m0),from = mn, to = mx, bw = "nrd0")); 
  fn = function(x) pmin(f1(x), f0(x));
  s = tryCatch({1 - integrate(fn, lower = mn, upper = mx,
subdivisions = 500)$value}, error = function(e){s = NA});
}

ksd = function(dat, cov , weight, a) {
  b= dat[,a];
  cov = dat[,cov];
  w = dat[, weight];
  F1 = ewcdf(cov[b == 1],w = w[b == 1]);
  F0 = ewcdf(cov[b == 0],w = w[b == 0]); 
  max(abs(F1(cov)-F0(cov)));
}

mhb = function(covs, trt, weight) { 
  a1=covs[trt==1,]
  a0=covs[trt==0,]
  weight1=weight[trt==1]
  weight0=weight[trt==0]
  S1 = cov.wt(a1, wt = weight1)$cov;
  S0 = cov.wt(a0, wt = weight0)$cov;
  S = ((sum(weight1) - 1)*S1 + (sum(weight0) - 1)*S0)/
    (sum(weight1) + sum(weight0)-2);
  Sinv = solve(S,tol=1e-100);
  x1 = apply(a1, 2, weighted.mean, weight1);
  x0 = apply(a0, 2, weighted.mean, weight0);
  sum((t(x1-x0) %*% Sinv)*(x1-x0));
}

gwd = function(dat, nc, trt, weight){
  C = dat[,1:nc];
  weight1=weight[trt==1];
  weight0=weight[trt==0];
  for(i in 1:nc){
    for(j in i:nc) C=cbind(C, dat[,i]*dat[,j]);
  }
  m1 = apply(C[trt==1,], 2, weighted.mean, weight1);
  m0 = apply(C[trt==0,], 2, weighted.mean, weight0);
  s = apply(C, 2, weighted.var, weight);
  weighted.sd = sqrt(s);
  b = c(rep(1, nc), rep(.5, nc*(nc+1)/2));
  mean(b*abs(m1-m0)/weighted.sd);
}


