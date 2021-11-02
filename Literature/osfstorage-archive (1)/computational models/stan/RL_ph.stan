data {
  int<lower=1> S; //subject
  int<lower=1> T; // num trials 
  int<lower=-1, upper=1> reward[S,T]; // reward set to -1 and 1
  int<lower=1, upper=2> choice[S,T]; // originally 0=left, 1=right 
}

transformed data {
  vector[2] initQ;  // initial values for Q
  initQ = rep_vector(0.0, 2);
}

parameters {
  // add group level parameter names:
  real eta_mu_pr;    
  real k_mu_pr;    
  real alpha0_mu_pr;    
  real beta_mu_pr;
  real<lower=0> eta_sd;
  real<lower=0> k_sd;
  real<lower=0> alpha0_sd;
  real<lower=0> beta_sd;
  
  vector[S] eta_pr;
  vector[S] k_pr;  
  vector[S] alpha0_pr; 
  vector[S] beta_pr; 
}

transformed parameters{
  // subject level parameters
  vector<lower=0,upper=1>[S] eta; // weight between prediction error and associability
  vector<lower=0,upper=1>[S] k;   // weight of associability
  vector<lower=0,upper=1>[S] alpha0;   // initial associability
  vector <lower=0,upper=20>[S] beta; // inverse temp.
  
  eta = Phi_approx(eta_mu_pr+eta_sd*eta_pr);
  k = Phi_approx(k_mu_pr+k_sd*k_pr);
  alpha0=Phi_approx(alpha0_mu_pr+alpha0_sd*alpha0_pr);
  beta=Phi_approx(beta_mu_pr+beta_sd*beta_pr)*20;
}

model {  
  eta_mu_pr~normal(0,1);
  k_mu_pr~normal(0,1);
  alpha0_mu_pr~normal(0,1);
  beta_mu_pr~normal(0,1);
  eta_sd~cauchy(0,3);
  k_sd~cauchy(0,3);
  alpha0_sd~cauchy(0,3);
  beta_sd~cauchy(0,3);
  k_pr~normal(0,1);
  eta_pr~normal(0,1);
  alpha0_pr~normal(0,1);
  beta_pr~normal(0,1);
  
  {
    for (s in 1:S){
      vector[2] Q;  // Q value for all actions
      real alpha; 
      real pe;
      Q = initQ;
      alpha = alpha0[s];
      
      for (t in 1:T)  {
        choice[s,t] ~ categorical_logit( beta[s]*Q ); // "sampling statement""
        pe = reward[s,t]-Q[choice[s,t]]; // pe
        // value update
        Q[choice[s,t]] += k[s] * alpha * pe;
        // associability update
        alpha = eta[s] * fabs(pe) + (1-eta[s]) * alpha;
      } // end of t loop (T trials)
    } // end of s loop (S subjects)
  }
}

generated quantities {
  real<lower=0,upper=1> eta_mu; 
  real<lower=0,upper=1> k_mu; 
  real<lower=0,upper=1> alpha0_mu; 
  real <lower=0,upper=20> beta_mu;
  real log_lik[S];
  real pe[S,T];
  real alpha[S,T+1];
  real chosenQ[S,T];
  real unchosenQ[S,T];
  int  y_pred[S, T];
  
  eta_mu=Phi_approx(eta_mu_pr);
  k_mu=Phi_approx(k_mu_pr);
  alpha0_mu=Phi_approx(alpha0_mu_pr);
  beta_mu=Phi_approx(beta_mu_pr)*20;
  y_pred = rep_array(-999,S ,T);
  
  {
    for (s in 1:S){
      vector[2] Q;  // Q value for all actions
      Q = initQ;
      log_lik[s]=0;
      alpha[s,1] = alpha0[s];
      
      for (t in 1:T)  {
        log_lik[s] += categorical_logit_lpmf(choice[s,t] | beta[s]*Q );
        y_pred[s,t] = categorical_logit_rng( beta[s] * Q ); 
        
        chosenQ[s,t] = Q[choice[s,t]];
        unchosenQ[s,t] = Q[3-choice[s,t]];
        
        pe[s,t]=(reward[s,t]-Q[choice[s,t]]);
        Q[choice[s,t]] += k[s] * alpha[s,t] * pe[s,t];
        alpha[s,t+1] = eta[s] * fabs(pe[s,t]) + (1-eta[s]) * alpha[s,t];
      } // end of t loop (T trials)
    } // end of t loop (S subjects)
  }
}  
