/* Includes FORTRAN linear algebra routines in Lapack.h and BLAS.h */
/* Requires a Makevars file with PKG_LIBS+=$(BLAS_LIBS) $(LAPACK_LIBS) $(FLIBS) */

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Lapack.h>

#define RANDIN GetRNGstate()
#define RANDOUT PutRNGstate()
    
/* Main mcmc function */
void fgevspatial(int *iters, int *ns, int *nf, int *nt, double *y, 
  double *Xmu, int *npmu, double *Xsig, int *npsig, double *Xxi, int *npxi, 
  double *mnbmu, double *itaumu,
  double *mnbsig, double *itausig,
  double *mnbxi, double *itauxi,
  double *ialpha, double *ilogbw, double *beta, double *logs, double *u, 
  double *Qb, double *dw2, 
  double *MHbeta, double *MHalpha, double *MHlogbw, double *MHs, double *MHu,
  double *pribeta, double *prialpha, double *prilogbw, double *pritau, int *trace,  
  double *betasamp, double *paramsamp, double *psrvsamp, double *urvsamp);
  
/* Main latent mcmc function */
void fgevspatial_latent(int *iters, int *ns, int *nt, double *y, 
  double *Xmu, int *npmu, double *Xsig, int *npsig, double *Xxi, int *npxi, 
  double *mnbmu, double *itaumu,
  double *mnbsig, double *itausig,
  double *mnbxi, double *itauxi,
  double *beta, double *Qb, double *MHbeta,
  double *pribeta, double *pritau, int *trace,  
  double *betasamp, double *paramsamp);

/* Conjugate simulation functions */
double *rgevlin(double *beta, double tau, double *X, int nxp, double *Qb, 
    int ns, int nt, double pribetaval);
double rtau(double *beta, double *mnb, double *X, int nxp, double *Qb, 
    int ns, int nt, double pritauval);

/* Log posterior density functions */
double loglike(double data, double mu, double logsig, double xi, 
  double theta, double alpha);
double loglike_latent(double data, double mu, double logsig, double xi);
double h1s(double psrv, double urv, double alpha);
double h1u(double psrv, double urv, double alpha);
double h1a(double psrv, double urv, double alpha);
double h1(double psrv, double urv, double alpha);
double dmvnorm(double *beta, double *mnb, double *X, double tau, int nxp, 
  double *Qb, int ns);

/* Helper functions */
void make_omega(double *omega, double alpha, double logbw, double *d2, int ns, int nf, 
  int nt);
void make_theta(double *theta, double *logs, double *omega, int ns, int nf, 
  int nt);

/* Linear algebra functions: column-major order */
void R_smult(double *A, double *B, int *m, int *n, int *p, double *C);
void R_tmult(double *A, double *B, int *m, int *n, int *p, double *C);
void R_xmult(double *A, double *B, int *m, int *n, int *p, double *C);
void R_zmult(double *A, double *B, int *m, int *n, int *p, double *C);
void R_chol(double *A, int *n);
void R_chol2inv(double *A, int *n);

    
void fgevspatial(int *iters, int *ns, int *nf, int *nt, double *y, 
  double *Xmu, int *npmu, double *Xsig, int *npsig, double *Xxi, int *npxi, 
  double *mnbmu, double *itaumu, 
  double *mnbsig, double *itausig,
  double *mnbxi, double *itauxi,
  double *ialpha, double *ilogbw, double *beta, double *logs, double *u, 
  double *Qb, double *dw2, 
  double *MHbeta, double *MHalpha, double *MHlogbw, double *MHs, double *MHu,
  double *pribeta, double *prialpha, double *prilogbw, double *pritau, int *trace,
  double *betasamp, double *paramsamp, double *psrvsamp, double *urvsamp)
{
  
  /* integer values */
  int i,j,k,t,l;
  int npar = *npxi + *npsig + *npmu + 6;
  
  /* other double values */
  double lik_init, acc_prob, VVV, MMM;
  double alpha = *ialpha, logbw = *ilogbw;
  double taumu = *itaumu, tausig = *itausig, tauxi = *itauxi;
  double canu, canalpha, canbeta, canlogbw, candiff, canlogs;
  
  /* double matrices: theta, omega and likelihood */
  double *cantheta, *theta, *canomega, *omega, *curll, *canll;
  cantheta = (double *)R_alloc(*ns * *nt, sizeof(double));
  theta = (double *)R_alloc(*ns * *nt, sizeof(double));
  canomega = (double *)R_alloc(*ns * *nf, sizeof(double));
  omega = (double *)R_alloc(*ns * *nf, sizeof(double));
  curll = (double *)R_alloc(*ns * *nt, sizeof(double));
  canll = (double *)R_alloc(*ns * *nt, sizeof(double));
  
  /* double ns vectors used in psrv updates */
  double *canccc, *ccc, *v;
  ccc = (double *)R_alloc(*ns, sizeof(double));
  canccc = (double *)R_alloc(*ns, sizeof(double));
  v = (double *)R_alloc(*ns, sizeof(double));
  
  /* double ns vectors used in other updates */
  double *Xb, *Qbv, *betamu, *betasig, *betaxi;
  Xb = (double *)R_alloc(*ns, sizeof(double));
  Qbv = (double *)R_alloc(*ns, sizeof(double));
  betamu = (double *)R_alloc(*ns, sizeof(double));
  betasig = (double *)R_alloc(*ns, sizeof(double));
  betaxi = (double *)R_alloc(*ns, sizeof(double));
  
  /* double vectors used in conjugate simulation */
  double *outmu, *outsig, *outxi;
  outmu = (double *)R_alloc(*npmu, sizeof(double));
  outsig = (double *)R_alloc(*npsig, sizeof(double));
  outxi = (double *)R_alloc(*npxi, sizeof(double));
  
  /* construct intial omega and theta matrices */
  make_omega(canomega, alpha, logbw, dw2, *ns, *nf, *nt);
  make_theta(cantheta, logs, canomega, *ns, *nf, *nt);
  for(i=0;i<*ns**nt;i++) theta[i] = cantheta[i];
  for(i=0;i<*ns**nf;i++) omega[i] = canomega[i];

  /* calculate and print initial likelihood */
  lik_init = 0;
  for(t=0;t<*nt;t++) {
    for(j=0;j<*ns;j++) {
      if(ISNAN(y[j**nt+t])) continue;
      lik_init += loglike(y[j**nt+t],beta[j*3+0],beta[j*3+1],beta[j*3+2],theta[j**nt+t],alpha);
    }
  }
  if(R_IsNaN(lik_init)) error("starting values give NaN likelihood");
  if(!R_FINITE(lik_init)) error("starting values give zero likelihood"); 
  /* Rprintf("init likelihood %f\n", lik_init); */
  
  /* calculate bandwidth lower bound */
  double bwlower, dw2smin;
  bwlower = 0;
  for(j=0;j<*ns;j++) {
    dw2smin = R_PosInf;
    for(l=0;l<*nf;l++) dw2smin = fmin2(dw2[j**nf + l], dw2smin);
    bwlower = fmax2(dw2smin, bwlower);
  }

  RANDIN;
  /* main loop */
  for(i=0;i<*iters;i++) {
        
    /* #### psrv propsals #### */ 
    
    for(t=0;t<*nt;t++) { 
      for(j=0;j<*ns;j++) {
        if(ISNAN(y[j**nt+t])) continue;
        v[j] =  1 + beta[j*3 + 2]*exp(-beta[j*3 + 1])*(y[j**nt + t] - beta[j*3 + 0]);
        v[j] = R_pow(v[j], -1/(alpha * beta[j*3 + 2]));      
        ccc[j] = log(theta[j**nt + t]) - theta[j**nt + t] * v[j];
        if(R_IsNaN(ccc[j])) error("zero likelihood in psrv update");
      }
      for(l=0;l<*nf;l++) {
        canlogs = rnorm(logs[l**nt + t], MHs[l**nt + t]);
        candiff = exp(canlogs) - exp(logs[l**nt + t]);
        acc_prob = 0;
        for(j=0;j<*ns;j++) {
          if(omega[j**nf + l] > 0) {
            cantheta[j**nt + t] = theta[j**nt + t] + omega[j**nf + l] * candiff;
            if(ISNAN(y[j**nt+t])) continue;
            canccc[j] = log(cantheta[j**nt + t]) - cantheta[j**nt + t] * v[j];
            acc_prob = acc_prob + canccc[j] - ccc[j];
          }
        }
        acc_prob = acc_prob + h1s(canlogs, u[l**nt+t], alpha) - h1s(logs[l**nt+t], u[l**nt+t], alpha);
        if(ISNAN(acc_prob)) error("NA value for logs update");
        if(runif(0, 1) < exp(acc_prob)) {
          logs[l**nt + t] = canlogs;
          for(j=0;j<*ns;j++) {
            if(omega[j**nf + l] > 0) {    
              theta[j**nt + t] = cantheta[j**nt + t];
              if(ISNAN(y[j**nt+t])) continue;
              ccc[j] = canccc[j];
            }
          }
        }
      }
    }
    
    /* #### u propsals #### */ 
  
    for(t=0;t<*nt;t++) {
      for(l=0;l<*nf;l++) {  
        canu =  rnorm(log(u[l**nt + t]/(1-u[l**nt + t])), MHu[l**nt + t]);
        canu = exp(canu) / (1 + exp(canu)); 
        acc_prob = h1u(logs[l**nt+t], canu, alpha) - h1u(logs[l**nt+t], u[l**nt+t], alpha) +
          log(canu * (1 - canu)) - log(u[l**nt+t] * (1 - u[l**nt+t]));
        if(ISNAN(acc_prob)) error("NA value for u update");
        if(runif(0, 1) < exp(acc_prob)) u[l**nt + t] = canu;
      }
    }
    
    /* ### current likelihood ### */
    
    for(t=0;t<*nt;t++) {
      for(j=0;j<*ns;j++) {
        if(ISNAN(y[j**nt+t])) continue;
        curll[j**nt + t] = loglike(y[j**nt+t],beta[j*3+0],beta[j*3+1],beta[j*3+2],theta[j**nt+t],alpha);
      }
    }
     
    /* #### alpha propsal #### */
  
    canalpha = rnorm(log(alpha/(1-alpha)), MHalpha[0]);  
    canalpha = exp(canalpha) / (1 + exp(canalpha));  
    make_omega(canomega, canalpha, logbw, dw2, *ns, *nf, *nt);
    make_theta(cantheta,logs,canomega,*ns,*nf,*nt);
    for(j=0;j<*ns**nt;j++) canll[j] = curll[j];
    acc_prob = 0;
    for(t=0;t<*nt;t++) {
      for(j=0;j<*ns;j++) {
        if(ISNAN(y[j**nt+t])) continue;
        canll[j**nt + t] = loglike(y[j**nt+t],beta[j*3+0],beta[j*3+1],beta[j*3+2],cantheta[j**nt+t],canalpha);
        acc_prob = acc_prob + canll[j**nt + t] - curll[j**nt + t];
      }
    }
    for(t=0;t<*nt;t++) {
      for(l=0;l<*nf;l++) {
        acc_prob = acc_prob + h1a(logs[l**nt+t], u[l**nt+t], canalpha) - h1a(logs[l**nt+t], u[l**nt+t], alpha);
      }
    }    
    acc_prob = acc_prob + log(canalpha * (1 - canalpha)) - log(alpha * (1 - alpha)) +
        dbeta(canalpha, prialpha[0], prialpha[1], 1) -
        dbeta(alpha, prialpha[0], prialpha[1], 1);
    if(ISNAN(acc_prob)) error("NA value for alpha update");
    if(runif(0, 1) < exp(acc_prob)) {
      alpha = canalpha;
      for(j=0;j<*ns**nt;j++) theta[j] = cantheta[j];
      for(j=0;j<*ns**nf;j++) omega[j] = canomega[j];
      for(j=0;j<*ns**nt;j++) curll[j] = canll[j]; 
    } 
    
    /* #### logbandwidth propsal #### */
  
    canlogbw = rnorm(logbw, MHlogbw[0]);
    if(exp(canlogbw) <= bwlower) canlogbw = logbw; /* lower bound */
    make_omega(canomega,alpha,canlogbw,dw2,*ns,*nf,*nt);
    for(j=0;j<*ns**nt;j++) canll[j] = curll[j];
    make_theta(cantheta,logs,canomega,*ns,*nf,*nt);
    acc_prob = 0;
    for(t=0;t<*nt;t++) {
      for(j=0;j<*ns;j++) {
        if(ISNAN(y[j**nt+t])) continue;
        canll[j**nt + t] = loglike(y[j**nt+t],beta[j*3+0],beta[j*3+1],beta[j*3+2],cantheta[j**nt+t],alpha);
        acc_prob = acc_prob + canll[j**nt + t] - curll[j**nt + t];
      }
    }
    acc_prob = acc_prob + dnorm(canlogbw, prilogbw[0], prilogbw[1], 1) -
      dnorm(logbw, prilogbw[0], prilogbw[1], 1);
    if(ISNAN(acc_prob)) error("NA value for bandwidth update");
    if(runif(0, 1) < exp(acc_prob)) {
      logbw = canlogbw;
      for(j=0;j<*ns**nf;j++) omega[j] = canomega[j];
      for(j=0;j<*ns**nt;j++) theta[j] = cantheta[j];
      for(j=0;j<*ns**nt;j++) curll[j] = canll[j];
    }   
            
    /* #### gev parameter mu propsals #### */ 
   
    for(j=0;j<*ns;j++) {
      Xb[j] = 0;
      for(k=0;k<*npmu;k++) Xb[j] += Xmu[j**npmu + k] * mnbmu[k];
    } 
    for(j=0;j<*ns;j++) {
      for(k=0;k<*ns;k++) Qbv[k] = Qb[j**ns + k]; 
      VVV = taumu * Qbv[j];
      MMM = taumu * Qbv[j] * Xb[j];
      for(k=0;k<*ns;k++) MMM = MMM - taumu * Qbv[k] * (beta[k*3+0] - Xb[k]);
      MMM = MMM + taumu * Qbv[j] * (beta[j*3+0] - Xb[j]);
      canbeta = rnorm(beta[j*3+0], MHbeta[j*3+0]);
      acc_prob = 0;
      for(t=0;t<*nt;t++) {
        if(ISNAN(y[j**nt+t])) continue;
        canll[j**nt + t] = loglike(y[j**nt+t],canbeta,beta[j*3+1],beta[j*3+2],theta[j**nt+t],alpha);
        acc_prob = acc_prob + canll[j**nt + t] - curll[j**nt + t];
      }
      acc_prob = acc_prob + dnorm(canbeta, MMM/VVV, 1/sqrt(VVV), 1) -
        dnorm(beta[j*3+0], MMM/VVV, 1/sqrt(VVV), 1);  
      if(ISNAN(acc_prob)) error("NA value for mu gev parameter update");
  
      if(runif(0, 1) < exp(acc_prob)) {
        beta[j*3+0] = canbeta;
        for(t=0;t<*nt;t++) curll[j**nt + t] = canll[j**nt + t];
      }
    }
    
    /* #### gev parameter sig propsals #### */ 
   
    for(j=0;j<*ns;j++) {
      Xb[j] = 0;
     for(k=0;k<*npsig;k++) Xb[j] += Xsig[j**npsig + k] * mnbsig[k];
    }   
    for(j=0;j<*ns;j++) {
      for(k=0;k<*ns;k++) Qbv[k] = Qb[j**ns + k]; 
      VVV = tausig * Qbv[j];
      MMM = tausig * Qbv[j] * Xb[j];
      for(k=0;k<*ns;k++) MMM = MMM - tausig * Qbv[k] * (beta[k*3+1] - Xb[k]);
      MMM = MMM + tausig * Qbv[j] * (beta[j*3+1] - Xb[j]);
      canbeta = rnorm(beta[j*3+1], MHbeta[j*3+1]);
      acc_prob = 0;
      for(t=0;t<*nt;t++) {
        if(ISNAN(y[j**nt+t])) continue;
        canll[j**nt + t] = loglike(y[j**nt+t],beta[j*3+0],canbeta,beta[j*3+2],theta[j**nt+t],alpha);
        acc_prob = acc_prob + canll[j**nt + t] - curll[j**nt + t];
      }
      acc_prob = acc_prob + dnorm(canbeta, MMM/VVV, 1/sqrt(VVV), 1) -
        dnorm(beta[j*3+1], MMM/VVV, 1/sqrt(VVV), 1); 
      if(ISNAN(acc_prob)) error("NA value for sig gev parameter update");
  
      if(runif(0, 1) < exp(acc_prob)) {
        beta[j*3+1] = canbeta;
        for(t=0;t<*nt;t++) curll[j**nt + t] = canll[j**nt + t];
      }
    }
    
    /* #### gev parameter xi propsals #### */
 
    for(j=0;j<*ns;j++) {
      Xb[j] = 0;
     for(k=0;k<*npxi;k++) Xb[j] += Xxi[j**npxi + k] * mnbxi[k];
    }  
    for(j=0;j<*ns;j++) {
      for(k=0;k<*ns;k++) Qbv[k] = Qb[j**ns + k]; 
      VVV = tauxi * Qbv[j];
      MMM = tauxi * Qbv[j] * Xb[j];
      for(k=0;k<*ns;k++) MMM = MMM - tauxi * Qbv[k] * (beta[k*3+2] - Xb[k]);
      MMM = MMM + tauxi * Qbv[j] * (beta[j*3+2] - Xb[j]);
      canbeta = rnorm(beta[j*3+2], MHbeta[j*3+2]);
      acc_prob = 0;
      for(t=0;t<*nt;t++) {
        if(ISNAN(y[j**nt+t])) continue;
        canll[j**nt + t] = loglike(y[j**nt+t],beta[j*3+0],beta[j*3+1],canbeta,theta[j**nt+t],alpha);
        acc_prob = acc_prob + canll[j**nt + t] - curll[j**nt + t];
      }
      acc_prob = acc_prob + dnorm(canbeta, MMM/VVV, 1/sqrt(VVV), 1) -
        dnorm(beta[j*3+2], MMM/VVV, 1/sqrt(VVV), 1); 
      if(ISNAN(acc_prob)) error("NA value for xi gev parameter update");
  
      if(runif(0, 1) < exp(acc_prob)) {
        beta[j*3+2] = canbeta;
        for(t=0;t<*nt;t++) curll[j**nt + t] = canll[j**nt + t];
      }
    }
    
    /* #### linear model and tau simulations for mu #### */
   
    for(j=0;j<*ns;j++) betamu[j] = beta[j*3+0];
    outmu = rgevlin(betamu, taumu, Xmu, *npmu, Qb, *ns, *nt, pribeta[0]);
    for(j=0;j<*npmu;j++) {
      mnbmu[j] = outmu[j];
      if(ISNAN(mnbmu[j])) error("NA value for mnbmu parameter");
    }
    taumu = rtau(betamu, mnbmu, Xmu, *npmu, Qb, *ns, *nt, pritau[0]);
    if(ISNAN(taumu)) error("NA value for taumu parameter");
    
    /* #### linear model and tau simulations for sig #### */
    
    for(j=0;j<*ns;j++) betasig[j] = beta[j*3+1];
    outsig = rgevlin(betasig, tausig, Xsig, *npsig, Qb, *ns, *nt, pribeta[1]);
    for(j=0;j<*npsig;j++) {
      mnbsig[j] = outsig[j];
      if(ISNAN(mnbsig[j])) error("NA value for mnbsig parameter");
    }
    tausig = rtau(betasig, mnbsig, Xsig, *npsig, Qb, *ns, *nt, pritau[1]);
    if(ISNAN(tausig)) error("NA value for tausig parameter");
    
    /* #### linear model and tau simulations for xi #### */
    
    for(j=0;j<*ns;j++) betaxi[j] = beta[j*3+2];
    outxi = rgevlin(betaxi, tauxi, Xxi, *npxi, Qb, *ns, *nt, pribeta[2]);
    for(j=0;j<*npxi;j++) {
      mnbxi[j] = outxi[j];
      if(ISNAN(mnbxi[j])) error("NA value for mnbxi parameter");
    }
    tauxi = rtau(betaxi, mnbxi, Xxi, *npxi, Qb, *ns, *nt, pritau[2]);
    if(ISNAN(tauxi)) error("NA value for taumu parameter");
    
    /* #### calculate the posterior #### */
 
    lik_init = 0;
    for(t=0;t<*nt;t++) {
      for(j=0;j<*ns;j++) {
        if(ISNAN(y[j**nt+t])) continue;
        lik_init += loglike(y[j**nt+t],beta[j*3+0],beta[j*3+1],beta[j*3+2],theta[j**nt+t],alpha);
      }
    }
   /*  Rprintf("%f  ", lik_init); */
    
    for(t=0;t<*nt;t++) {
      for(l=0;l<*nf;l++) {  
        lik_init += h1(logs[l**nt+t], u[l**nt+t], alpha);
      }
    }
    /* Rprintf("%f  ", lik_init); */
    
    lik_init += dmvnorm(betamu, mnbmu, Xmu, taumu, *npmu, Qb, *ns);
    lik_init += dmvnorm(betasig, mnbsig, Xsig, tausig, *npsig, Qb, *ns);
    lik_init += dmvnorm(betaxi, mnbxi, Xxi, tauxi, *npxi, Qb, *ns);
    /* Rprintf("%f  ", lik_init); */
    
    lik_init += dbeta(alpha, prialpha[0], prialpha[1], 1);
    lik_init += dnorm(logbw, prilogbw[0], prilogbw[1], 1);
    lik_init += dgamma(taumu,0.1,1/0.1,1) + dgamma(tausig,0.1,1/0.1,1) + dgamma(tauxi,0.1,1/0.1,1);
    for(j=0;j<*npmu;j++) lik_init += dnorm(mnbmu[j], 0, 10, 1);
    for(j=0;j<*npsig;j++) lik_init += dnorm(mnbsig[j], 0, 10, 1);
    for(j=0;j<*npxi;j++) lik_init += dnorm(mnbxi[j], 0, 10, 1);
    
    /* #### store the samples and posterior #### */
    
    for(j=0;j<*npmu;j++) paramsamp[i*npar + j] = mnbmu[j];
    paramsamp[i*npar + *npmu] = taumu;
    for(j=0;j<*npsig;j++) paramsamp[i*npar + j + *npmu + 1] = mnbsig[j];
    paramsamp[i*npar + *npsig + *npmu + 1] = tausig;
    for(j=0;j<*npxi;j++) paramsamp[i*npar + j + *npsig + *npmu + 2] = mnbxi[j];
    paramsamp[i*npar + *npxi + *npsig + *npmu + 2] = tauxi;
    paramsamp[i*npar + *npxi + *npsig + *npmu + 3] = alpha;
    paramsamp[i*npar + *npxi + *npsig + *npmu + 4] = logbw;
    paramsamp[i*npar + *npxi + *npsig + *npmu + 5] = lik_init;
    
    for(j=0;j<(*ns * 3);j++) betasamp[i*(*ns * 3) + j] = beta[j];
    for(j=0;j<(*nf * *nt);j++) psrvsamp[i*(*nf * *nt) + j] = logs[j]; 
    for(j=0;j<(*nf * *nt);j++) urvsamp[i*(*nf * *nt) + j] = u[j];
    
    /* #### print log posterior #### */
    
    if((*trace) && !((i+1) % *trace)) Rprintf("%i: %.4f\n", i+1, lik_init);
  }
  RANDOUT;
}

void fgevspatial_latent(int *iters, int *ns, int *nt, double *y, 
  double *Xmu, int *npmu, double *Xsig, int *npsig, double *Xxi, int *npxi, 
  double *mnbmu, double *itaumu,
  double *mnbsig, double *itausig,
  double *mnbxi, double *itauxi,
  double *beta, double *Qb, double *MHbeta,
  double *pribeta, double *pritau, int *trace,  
  double *betasamp, double *paramsamp)
{
  
  /* integer values */
  int i,j,k,t;
  int npar = *npxi + *npsig + *npmu + 4;
  
  /* other double values */
  double lik_init, acc_prob, VVV, MMM;
  double taumu = *itaumu, tausig = *itausig, tauxi = *itauxi;
  double  canbeta;
  
  /* double matrices: likelihood */
  double *curll, *canll;
  curll = (double *)R_alloc(*ns * *nt, sizeof(double));
  canll = (double *)R_alloc(*ns * *nt, sizeof(double));
  
  /* double ns vectors used in other updates */
  double *Xb, *Qbv, *betamu, *betasig, *betaxi;
  Xb = (double *)R_alloc(*ns, sizeof(double));
  Qbv = (double *)R_alloc(*ns, sizeof(double));
  betamu = (double *)R_alloc(*ns, sizeof(double));
  betasig = (double *)R_alloc(*ns, sizeof(double));
  betaxi = (double *)R_alloc(*ns, sizeof(double));
  
  /* double vectors used in conjugate simulation */
  double *outmu, *outsig, *outxi;
  outmu = (double *)R_alloc(*npmu, sizeof(double));
  outsig = (double *)R_alloc(*npsig, sizeof(double));
  outxi = (double *)R_alloc(*npxi, sizeof(double));

  /* calculate and print initial likelihood */
  lik_init = 0;
  for(t=0;t<*nt;t++) {
    for(j=0;j<*ns;j++) {
      if(ISNAN(y[j**nt+t])) continue;
      lik_init += loglike_latent(y[j**nt+t],beta[j*3+0],beta[j*3+1],beta[j*3+2]);
    }
  }
  if(R_IsNaN(lik_init)) error("starting values give NaN likelihood");
  if(!R_FINITE(lik_init)) error("starting values give zero likelihood"); 
  /* Rprintf("init likelihood %f\n", lik_init); */

  RANDIN;
  /* main loop */
  for(i=0;i<*iters;i++) {
     
    /* ### current likelihood ### */
    
    for(t=0;t<*nt;t++) {
      for(j=0;j<*ns;j++) {
        if(ISNAN(y[j**nt+t])) continue;
        curll[j**nt + t] = loglike_latent(y[j**nt+t],beta[j*3+0],beta[j*3+1],beta[j*3+2]);
      }
    }
    
    /* #### gev parameter mu propsals #### */ 
   
    for(j=0;j<*ns;j++) {
      Xb[j] = 0;
      for(k=0;k<*npmu;k++) Xb[j] += Xmu[j**npmu + k] * mnbmu[k];
    } 
    for(j=0;j<*ns;j++) {
      for(k=0;k<*ns;k++) Qbv[k] = Qb[j**ns + k]; 
      VVV = taumu * Qbv[j];
      MMM = taumu * Qbv[j] * Xb[j];
      for(k=0;k<*ns;k++) MMM = MMM - taumu * Qbv[k] * (beta[k*3+0] - Xb[k]);
      MMM = MMM + taumu * Qbv[j] * (beta[j*3+0] - Xb[j]);
      canbeta = rnorm(beta[j*3+0], MHbeta[j*3+0]);
      acc_prob = 0;
      for(t=0;t<*nt;t++) {
        if(ISNAN(y[j**nt+t])) continue;
        canll[j**nt + t] = loglike_latent(y[j**nt+t],canbeta,beta[j*3+1],beta[j*3+2]);
        acc_prob = acc_prob + canll[j**nt + t] - curll[j**nt + t];
      }
      acc_prob = acc_prob + dnorm(canbeta, MMM/VVV, 1/sqrt(VVV), 1) -
        dnorm(beta[j*3+0], MMM/VVV, 1/sqrt(VVV), 1);  
      if(ISNAN(acc_prob)) error("NA value for mu gev parameter update");
  
      if(runif(0, 1) < exp(acc_prob)) {
        beta[j*3+0] = canbeta;
        for(t=0;t<*nt;t++) curll[j**nt + t] = canll[j**nt + t];
      }
    }
    
    /* #### gev parameter sig propsals #### */ 
   
    for(j=0;j<*ns;j++) {
      Xb[j] = 0;
     for(k=0;k<*npsig;k++) Xb[j] += Xsig[j**npsig + k] * mnbsig[k];
    }   
    for(j=0;j<*ns;j++) {
      for(k=0;k<*ns;k++) Qbv[k] = Qb[j**ns + k]; 
      VVV = tausig * Qbv[j];
      MMM = tausig * Qbv[j] * Xb[j];
      for(k=0;k<*ns;k++) MMM = MMM - tausig * Qbv[k] * (beta[k*3+1] - Xb[k]);
      MMM = MMM + tausig * Qbv[j] * (beta[j*3+1] - Xb[j]);
      canbeta = rnorm(beta[j*3+1], MHbeta[j*3+1]);
      acc_prob = 0;
      for(t=0;t<*nt;t++) {
        if(ISNAN(y[j**nt+t])) continue;
        canll[j**nt + t] = loglike_latent(y[j**nt+t],beta[j*3+0],canbeta,beta[j*3+2]);
        acc_prob = acc_prob + canll[j**nt + t] - curll[j**nt + t];
      }
      acc_prob = acc_prob + dnorm(canbeta, MMM/VVV, 1/sqrt(VVV), 1) -
        dnorm(beta[j*3+1], MMM/VVV, 1/sqrt(VVV), 1); 
      if(ISNAN(acc_prob)) error("NA value for sig gev parameter update");
  
      if(runif(0, 1) < exp(acc_prob)) {
        beta[j*3+1] = canbeta;
        for(t=0;t<*nt;t++) curll[j**nt + t] = canll[j**nt + t];
      }
    }
    
    /* #### gev parameter xi propsals #### */
 
    for(j=0;j<*ns;j++) {
      Xb[j] = 0;
     for(k=0;k<*npxi;k++) Xb[j] += Xxi[j**npxi + k] * mnbxi[k];
    }  
    for(j=0;j<*ns;j++) {
      for(k=0;k<*ns;k++) Qbv[k] = Qb[j**ns + k]; 
      VVV = tauxi * Qbv[j];
      MMM = tauxi * Qbv[j] * Xb[j];
      for(k=0;k<*ns;k++) MMM = MMM - tauxi * Qbv[k] * (beta[k*3+2] - Xb[k]);
      MMM = MMM + tauxi * Qbv[j] * (beta[j*3+2] - Xb[j]);
      canbeta = rnorm(beta[j*3+2], MHbeta[j*3+2]);
      acc_prob = 0;
      for(t=0;t<*nt;t++) {
        if(ISNAN(y[j**nt+t])) continue;
        canll[j**nt + t] = loglike_latent(y[j**nt+t],beta[j*3+0],beta[j*3+1],canbeta);
        acc_prob = acc_prob + canll[j**nt + t] - curll[j**nt + t];
      }
      acc_prob = acc_prob + dnorm(canbeta, MMM/VVV, 1/sqrt(VVV), 1) -
        dnorm(beta[j*3+2], MMM/VVV, 1/sqrt(VVV), 1); 
      if(ISNAN(acc_prob)) error("NA value for xi gev parameter update");
  
      if(runif(0, 1) < exp(acc_prob)) {
        beta[j*3+2] = canbeta;
        for(t=0;t<*nt;t++) curll[j**nt + t] = canll[j**nt + t];
      }
    }
    
    /* #### linear model and tau simulations for mu #### */
   
    for(j=0;j<*ns;j++) betamu[j] = beta[j*3+0];
    outmu = rgevlin(betamu, taumu, Xmu, *npmu, Qb, *ns, *nt, pribeta[0]);
    for(j=0;j<*npmu;j++) {
      mnbmu[j] = outmu[j];
      if(ISNAN(mnbmu[j])) error("NA value for mnbmu parameter");
    }
    taumu = rtau(betamu, mnbmu, Xmu, *npmu, Qb, *ns, *nt, pritau[0]);
    if(ISNAN(taumu)) error("NA value for taumu parameter");
    
    /* #### linear model and tau simulations for sig #### */
    
    for(j=0;j<*ns;j++) betasig[j] = beta[j*3+1];
    outsig = rgevlin(betasig, tausig, Xsig, *npsig, Qb, *ns, *nt, pribeta[1]);
    for(j=0;j<*npsig;j++) {
      mnbsig[j] = outsig[j];
      if(ISNAN(mnbsig[j])) error("NA value for mnbsig parameter");
    }
    tausig = rtau(betasig, mnbsig, Xsig, *npsig, Qb, *ns, *nt, pritau[1]);
    if(ISNAN(tausig)) error("NA value for tausig parameter");
    
    /* #### linear model and tau simulations for xi #### */
    
    for(j=0;j<*ns;j++) betaxi[j] = beta[j*3+2];
    outxi = rgevlin(betaxi, tauxi, Xxi, *npxi, Qb, *ns, *nt, pribeta[2]);
    for(j=0;j<*npxi;j++) {
      mnbxi[j] = outxi[j];
      if(ISNAN(mnbxi[j])) error("NA value for mnbxi parameter");
    }
    tauxi = rtau(betaxi, mnbxi, Xxi, *npxi, Qb, *ns, *nt, pritau[2]);
    if(ISNAN(tauxi)) error("NA value for taumu parameter");
    
    /* #### calculate the posterior #### */
 
    lik_init = 0;
    for(t=0;t<*nt;t++) {
      for(j=0;j<*ns;j++) {
        if(ISNAN(y[j**nt+t])) continue;
        lik_init += loglike_latent(y[j**nt+t],beta[j*3+0],beta[j*3+1],beta[j*3+2]);
      }
    }
    /*  Rprintf("%f  ", lik_init); */
    
    lik_init += dmvnorm(betamu, mnbmu, Xmu, taumu, *npmu, Qb, *ns);
    lik_init += dmvnorm(betasig, mnbsig, Xsig, tausig, *npsig, Qb, *ns);
    lik_init += dmvnorm(betaxi, mnbxi, Xxi, tauxi, *npxi, Qb, *ns);
    /* Rprintf("%f  ", lik_init); */
    
    lik_init += dgamma(taumu,0.1,1/0.1,1) + dgamma(tausig,0.1,1/0.1,1) + dgamma(tauxi,0.1,1/0.1,1);
    for(j=0;j<*npmu;j++) lik_init += dnorm(mnbmu[j], 0, 10, 1);
    for(j=0;j<*npsig;j++) lik_init += dnorm(mnbsig[j], 0, 10, 1);
    for(j=0;j<*npxi;j++) lik_init += dnorm(mnbxi[j], 0, 10, 1);
    
    /* #### store the samples and posterior #### */
    
    for(j=0;j<*npmu;j++) paramsamp[i*npar + j] = mnbmu[j];
    paramsamp[i*npar + *npmu] = taumu;
    for(j=0;j<*npsig;j++) paramsamp[i*npar + j + *npmu + 1] = mnbsig[j];
    paramsamp[i*npar + *npsig + *npmu + 1] = tausig;
    for(j=0;j<*npxi;j++) paramsamp[i*npar + j + *npsig + *npmu + 2] = mnbxi[j];
    paramsamp[i*npar + *npxi + *npsig + *npmu + 2] = tauxi;
    paramsamp[i*npar + *npxi + *npsig + *npmu + 3] = lik_init;
    
    for(j=0;j<(*ns * 3);j++) betasamp[i*(*ns * 3) + j] = beta[j];
    
    
    /* #### print log posterior #### */
    
    if((*trace) && !((i+1) % *trace)) Rprintf("%i: %.4f\n", i+1, lik_init);
  }
  RANDOUT;
}


/* HELPER FUNCTIONS */

void make_omega(double *omega, double alpha, double logbw, double *d2, int ns, int nf, 
  int nt)
{
  int i,j;
  double *omegars;
  double bw2;
  
  omegars = (double *)Calloc(ns * sizeof(double), double);
  for(i=0;i<ns;i++) omegars[i] = 0;
  
  /* create w_l(s)^(1/alpha) from normal kernel */
  bw2 = exp(logbw)*exp(logbw);
  for(i=0;i<ns;i++) {
    for(j=0;j<nf;j++) {
      omega[i*nf + j] = fmax2(1 - d2[i*nf + j]/bw2, 0);
      omega[i*nf + j] = omega[i*nf + j] * omega[i*nf + j] * omega[i*nf + j];
      omegars[i] += omega[i*nf + j];
    }
    if(omegars[i] == 0) error("site has no close enough knots");
  }
  for(i=0;i<ns;i++) {
    for(j=0;j<nf;j++) {
      omega[i*nf + j] = R_pow(omega[i*nf + j]/omegars[i], 1/alpha);
      if(ISNAN(omega[i*nf + j])) error("NA value for omega");
      if(omega[i*nf + j] < 0) error("negative value for omega");
    }
  }
  
  Free(omegars);
}

void make_theta(double *theta, double *logs, double *omega, int ns, int nf, 
  int nt)
{
  int i,j;
  double *epsrvs;

  epsrvs = (double *)Calloc(nf * nt * sizeof(double), double);
  
  /* calculate theta matrix, excluding ^alpha */
  for(i=0;i<nf*nt;i++) epsrvs[i] = exp(logs[i]);
  R_smult(epsrvs, omega, &nt, &ns, &nf, theta);
  
  for(i=0;i<ns;i++) {
    for(j=0;j<nt;j++) {
      if(ISNAN(theta[i*nt + j])) error("NA value for theta");
      if(theta[i*nt + j] < 0) error("negative value for theta");
    }
  }
  
  Free(epsrvs);
}

/* CONJUGATE SIMULATION FUNCTIONS */

double *rgevlin(double *beta, double tau, double *X, int nxp, double *Qb, 
    int ns, int nt, double pribetaval)
{
  int i,j;
  double *rgtXQ, *rgtXQX;
  double *rgmnb1, *rgmnb2, *rgmnb3, *rgMMM, *rgout;
  int one = 1;
          
  rgtXQ = (double *)Calloc(nxp * ns * sizeof(double), double);
  rgtXQX = (double *)Calloc(nxp * nxp * sizeof(double), double);
  
  rgmnb1 = (double *)Calloc(nxp * sizeof(double), double);
  rgmnb2 = (double *)Calloc(nxp * sizeof(double), double);
  rgmnb3 = (double *)Calloc(nxp * sizeof(double), double);
  rgMMM = (double *)Calloc(nxp * sizeof(double), double);
  rgout = (double *)Calloc(nxp * sizeof(double), double); 
    
  R_xmult(Qb, X, &ns, &nxp, &ns, rgtXQ);
  R_smult(X, rgtXQ, &nxp, &nxp, &ns, rgtXQX);
  
  for(i=0;i<nxp;i++) {
    for(j=0;j<nxp;j++) {
      rgtXQX[nxp * i + j] = tau * rgtXQX[nxp * i + j];
      if(i == j) rgtXQX[nxp * i + j] = rgtXQX[nxp * i + j] + 1/(pribetaval * pribetaval);
    }
  }
  R_chol(rgtXQX, &nxp); R_chol2inv(rgtXQX, &nxp); 

  R_smult(beta, rgtXQ, &one, &nxp, &ns, rgMMM);
  for(i=0;i<nxp;i++) rgMMM[i] = tau * rgMMM[i];
  R_smult(rgMMM, rgtXQX, &one, &nxp, &nxp, rgmnb1);
  for(i=0;i<nxp;i++) rgmnb2[i] = rnorm(0, 1);
  R_chol(rgtXQX, &nxp);
  R_xmult(rgmnb2, rgtXQX, &one, &nxp, &nxp, rgmnb3);
  for(i=0;i<nxp;i++) rgout[i] = rgmnb1[i] + rgmnb3[i];
  
  Free(rgtXQ); Free(rgtXQX);
  Free(rgmnb1); Free(rgmnb2); Free(rgmnb3); Free(rgMMM);
  
  return rgout;
}

double rtau(double *beta, double *mnb, double *X, int nxp, double *Qb, 
    int ns, int nt, double pritauval)
{
  int i;
  double *rtbeta2, *rtbeta3;
  int one = 1;
  double SS;
  
  rtbeta2 = (double *)Calloc(ns * sizeof(double), double);
  rtbeta3 = (double *)Calloc(ns * sizeof(double), double);
      
  R_smult(mnb, X, &one, &ns, &nxp, rtbeta2);
  for(i=0;i<ns;i++) {
    rtbeta2[i] = beta[i] - rtbeta2[i]; 
  }
  R_xmult(Qb, rtbeta2, &ns, &one, &ns, rtbeta3);
  R_smult(rtbeta2, rtbeta3, &one, &one, &ns, &SS);
  /* unlike R, the second argument of rgamma is the scale */
  SS = rgamma(ns/2 + pritauval, 1/(SS/2 + pritauval));
  
  Free(rtbeta2); Free(rtbeta3); 
  
  return SS;
}

/* LOG POSTERIOR DENSITY FUNCTIONS */

double loglike(double data, double mu, double logsig, double xi, 
  double theta, double alpha)
{
  double out, ttt, logttt, xistar;
  
  if(fabs(xi) <= 0.0001) {
    xistar = 0;
    logttt = -(data - mu) / exp(logsig) / alpha;
    ttt = exp(logttt);
  } else {
    xistar = alpha * xi;
    ttt = 1 + xi * (data - mu) / exp(logsig);
    ttt = R_pow(ttt, -1/xistar);
    if(ttt <= 0) return R_NegInf;
    logttt = log(ttt);
  }
  out = -log(alpha) - logsig + log(theta) + 
    (xistar + 1)*logttt - theta * ttt;
      
  if(R_IsNaN(out)) return R_NegInf;
  if(!R_FINITE(out)) error("numerical issues in loglike");
  
  return out;  
}

double loglike_latent(double data, double mu, double logsig, double xi)
{
  double out, ttt, logttt;
  
  if(fabs(xi) <= 0.0001) {
    xi = 0;
    logttt = -(data - mu) / exp(logsig);
    ttt = exp(logttt);
  } else {
    ttt = 1 + xi * (data - mu) / exp(logsig);
    ttt = R_pow(ttt, -1/xi);
    if(ttt <= 0) return R_NegInf;
    logttt = log(ttt);
  }
  out = -logsig + (xi + 1)*logttt - ttt;
      
  if(R_IsNaN(out)) return R_NegInf;
  if(!R_FINITE(out)) error("numerical issues in loglike");
  
  return out;  
}

double h1s(double lpsrv, double urv, double alpha) 
{
  
  double out, psrv;

  urv = M_PI * urv;
  psrv = exp(lpsrv);
  urv = (log(sin(alpha * urv)) - log(sin(urv)))/(1 - alpha) + 
    log(sin((1 - alpha) * urv)) - log(sin(alpha * urv));
  out = -lpsrv/(1-alpha) - R_pow(psrv, -alpha/(1-alpha)) * exp(urv)
    + lpsrv; /*jacobian term */
  
  if(!R_FINITE(out) || R_IsNaN(out)) error("NaN/Inf value in h1s");
  return out;  
}

double h1u(double lpsrv, double urv, double alpha) 
{
  
  double out, psrv;

  urv = M_PI * urv;
  psrv = exp(lpsrv);
  urv = (log(sin(alpha * urv)) - log(sin(urv)))/(1 - alpha) + 
    log(sin((1 - alpha) * urv)) - log(sin(alpha * urv));
  out = urv - R_pow(psrv, -alpha/(1-alpha)) * exp(urv);
  
  if(!R_FINITE(out) || R_IsNaN(out)) error("NaN/Inf value in h1u");
  return out;  
}

double h1a(double lpsrv, double urv, double alpha) 
{
  
  double out, psrv;

  urv = M_PI * urv;
  psrv = exp(lpsrv);
  urv = (log(sin(alpha * urv)) - log(sin(urv)))/(1 - alpha) + 
    log(sin((1 - alpha) * urv)) - log(sin(alpha * urv));
  out = log(alpha) - log(1-alpha) - lpsrv/(1-alpha) + urv - 
    R_pow(psrv, -alpha/(1-alpha)) * exp(urv); 
  
  if(!R_FINITE(out) || R_IsNaN(out)) error("NaN/Inf value in h1a");
  return out;  
}

double h1(double lpsrv, double urv, double alpha) 
{
  
  double out, psrv;

  urv = M_PI * urv;
  psrv = exp(lpsrv);
  urv = (log(sin(alpha * urv)) - log(sin(urv)))/(1 - alpha) + 
    log(sin((1 - alpha) * urv)) - log(sin(alpha * urv));
  out = log(alpha) - log(1-alpha) - lpsrv/(1-alpha) + urv - 
    R_pow(psrv, -alpha/(1-alpha)) * exp(urv)
    + lpsrv; /*jacobian term */
  
  if(!R_FINITE(out) || R_IsNaN(out)) error("NaN/Inf value in h1");
  return out;  
}

double dmvnorm(double *beta, double *mnb, double *X, double tau, int nxp, 
  double *Qb, int ns) {
      
  int i, one = 1;
  double out, quadform;
  double *beta2, *beta3;
  
  beta2 = (double *)Calloc(ns * sizeof(double), double);
  beta3 = (double *)Calloc(ns * sizeof(double), double);
      
  R_smult(mnb, X, &one, &ns, &nxp, beta2);
  for(i=0;i<ns;i++) {
    beta2[i] = beta[i] - beta2[i];
  }
  R_xmult(Qb, beta2, &ns, &one, &ns, beta3);
  R_smult(beta2, beta3, &one, &one, &ns, &quadform);
  
  out = -0.5 * (ns * log(M_2PI / tau) + tau * quadform);
  
  Free(beta2); Free(beta3);
  return out;
}
          
/* LINEAR ALGEBRA ROUTINES: COLUMN-MAJOR ORDER */

/* Calculates A %*% B where A is m by p and B is p by n */
/* Stores result in m by n vector C */
/* Uses dgemm from BLAS */
void R_smult(double *A, double *B, int *m, int *n, int *p, double *C)
{

  double one = 1.0;
  double zero = 0.0;
	
	F77_CALL(dgemm)("N","N",m,n,p,&one,A,m,B,p,&zero,C,m);
}

/* Calculates t(A) %*% B where A is p by m and B is p by n */
/* Stores result in m by n vector C */
/* Uses dgemm from BLAS */
void R_tmult(double *A, double *B, int *m, int *n, int *p, double *C)
{

  double one = 1.0;
  double zero = 0.0;
	
	F77_CALL(dgemm)("T","N",m,n,p,&one,A,p,B,p,&zero,C,m);
}

/* Calculates A %*% t(B) where A is m by p and B is n by p */
/* Stores result in m by n vector C */
/* Uses dgemm from BLAS */
void R_xmult(double *A, double *B, int *m, int *n, int *p, double *C)
{

  double one = 1.0;
  double zero = 0.0;
	
	F77_CALL(dgemm)("N","T",m,n,p,&one,A,m,B,n,&zero,C,m);
}

/* Calculates t(A) %*% t(B) where A is p by m and B is n by p */
/* Stores result in m by n vector C */
/* Uses dgemm from BLAS */
void R_zmult(double *A, double *B, int *m, int *n, int *p, double *C)
{

  double one = 1.0;
  double zero = 0.0;
	
	F77_CALL(dgemm)("T","T",m,n,p,&one,A,p,B,n,&zero,C,m);
}

/* The C equivalent to the R function chol */ 
/* A is n by n square symmetric positive definite */
/* Only the upper triangle of A is used */
/* Stores result into upper triangle of A with strict lower triangle zeroed */
/* Uses dpotrf from Lapack */
void R_chol(double *A, int *n)
{

  int zero = 0;
  int i,j;
	
	F77_CALL(dpotrf)("U",n,A,n,&zero);
  
  /* fill strict lower triangle */
  for(i=0;i<*n;i++){
    for(j=i+1;j<*n;j++){
      A[*n * i + j] = 0;
    }
  }
}

/* The C equivalent to the R function chol2inv */ 
/* A is n by n square matrix with cholesky decomposition in upper triangle */
/* Stores result into A giving (symmetric) inverse of the original  */
/* Uses dpotri from Lapack */
void R_chol2inv(double *A, int *n)
{

  int zero = 0;
  int i,j;
	
	F77_CALL(dpotri)("U",n,A,n,&zero);
  
  /* fill strict lower triangle */
  for(i=0;i<*n;i++){
    for(j=i+1;j<*n;j++){
      A[*n * i + j] = A[*n * j + i];
    }
  }
}
