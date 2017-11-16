// http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/Vol1.pdf
// Page 3: Rats liveweight example modified for cows daily milk yield 
// Fitting wilmink curves, with coefficient priors plus attempting top add priors on herd milk yield
// Not stable if -0.05 exponential factor is made a parameter
// Attempting to estimate day for peak production fails (when coefficient values take on the wrong sign?)

data {
  int<lower=0> N; //number of cows
  int<lower=0> T; //observation per cow
  int<lower=0> H; //added, herds
  real x[T];   //day of lactation for each observation      
  real y[N,T]; //cow by date milk yield observations
  real z[H];   //added, herd milk summary
  real w[H,N];  //matrix that allocates cows to herd
//  real v[N];   //added cow milk summary
  vector[N] v;   //added cow milk summary
}
parameters {
  real alpha[N]; //parameters of wilmink 
  real beta[N];
  real chi[N];  //added

  real mu_alpha;
  real mu_beta;          // beta.c in original bugs model
  real mu_chi;  //added

  real<lower=0> sigmasq_y;
  real<lower=0> sigmasq_alpha;
  real<lower=0> sigmasq_beta;
  real<lower=0> sigmasq_chi;   //added
  real<lower=0> sigmasq_cow_error;
}
transformed parameters {
  real<lower=0> sigma_y;       // sigma in original bugs model
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_chi;      //added
  real<lower=0> sigma_cow_error;

  sigma_y = sqrt(sigmasq_y);
  sigma_alpha = sqrt(sigmasq_alpha);
  sigma_beta = sqrt(sigmasq_beta);
  sigma_chi = sqrt(sigmasq_chi);
  sigma_cow_error = sqrt(sigmasq_cow_error);
}
model {
  mu_alpha ~ normal(35, 5);
  mu_beta ~ normal(-10, 8);
  mu_chi ~ normal(-0.07, 0.04);   //added
  sigmasq_y ~ inv_gamma(0.001, 0.001);
  sigmasq_alpha ~ inv_gamma(0.001, 0.001);
  sigmasq_beta ~ inv_gamma(0.001, 0.001);
  sigmasq_chi ~ inv_gamma(0.001, 0.001);   //added
  sigmasq_cow_error ~ inv_gamma(0.001, 0.001);   //added
  alpha ~ normal(mu_alpha, sigma_alpha); // vectorized
  beta ~ normal(mu_beta, sigma_beta);  // vectorized
  chi ~ normal(mu_chi, sigma_chi);    //added
  for (n in 1:N)
//    v[n] ~ normal( alpha * 245 - 0.05 * beta * exp(-0.05 * 245) + 0.5 * chi * 245 * 245, sigma_cow_error);
//    v[n] ~ normal( 245 * alpha - 0.000000239  * beta + 30012.5 * chi, sigma_cow_error);
//    v[n] ~ normal(to_vector(alpha) * 245 - 0.05 * to_vector(beta) * exp(-0.05 * 245) + 0.5 * to_vector(chi) * 245 * 245, sigma_cow_error);
    for (t in 1:T) 
      y[n,t] ~ normal(alpha[n] + beta[n] * exp(-0.05 * x[t]) + chi[n] * x[t], sigma_y); //edited, now wilmink
      
}
generated quantities {
  real alpha0_mu;      // mean intercept for y at t=0
  vector[N] alpha0_n;  // per cow intercept for y at t=0
  real cumul_y_est;    // cumulative y (to t=245 days) for curve with coefficient mean values
  vector[N] cumul_y;   // cumulative y per cow
  vector[N] cow_error; // difference in cow total vs fit
  vector[H] herd_error; // difference in herd total vs fit
  alpha0_mu = mu_alpha + mu_beta; //edited 
  alpha0_n = to_vector(alpha) + to_vector(beta);
  cumul_y_est = mu_alpha * 245 - 0.05 * mu_beta * exp(-0.05 * 245) + 0.5 * mu_chi * 245 * 245; //integral of wilmink 
  cumul_y = to_vector(alpha) * 245 - 0.05 * to_vector(beta) * exp(-0.05 * 245) + 0.5 * to_vector(chi) * 245 * 245;
  cow_error = to_vector(v) - (to_vector(alpha) * 245 - 0.05 * to_vector(beta) * exp(-0.05 * 245) + 0.5 * to_vector(chi) * 245 * 245);
  herd_error =  to_vector(z) - to_matrix(w) * (to_vector(alpha) * 245 - 0.05 * to_vector(beta) * exp(-0.05 * 245) + 0.5 * to_vector(chi) * 245 * 245);    //i.e. difference between actual (z) and to_matrix(w) * cumul_y;

}

