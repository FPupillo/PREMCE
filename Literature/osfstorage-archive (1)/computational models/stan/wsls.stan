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
  real beta_mu_pr;
  real<lower=0> beta_sd;
  vector[S] beta_pr; 
}

transformed parameters{
  // subject level parameters
  vector <lower=0,upper=20>[S] beta; // inverse temp.
  beta=Phi_approx(beta_mu_pr+beta_sd*beta_pr)*20;
}

model {  
  beta_mu_pr~normal(0,1);
  beta_sd~cauchy(0,3);
  beta_pr~normal(0,1);
  
  {
    for (s in 1:S){
      vector[2] Q;  // Q value for all actions
      Q = initQ;
      
      for (t in 1:T)  {
        choice[s,t] ~ categorical_logit( beta[s]*Q ); // "sampling statement""
        
        Q[choice[s,t]]   = (reward[s,t] == 1)?1:(-1);
        Q[3-choice[s,t]] = (reward[s,t] == 1)?(-1):1;
      } // end of t loop (T trials)
    } // end of s loop (S subjects)
  }
}

generated quantities {
  real <lower=0,upper=20> beta_mu;
  real log_lik[S];
  int  y_pred[S, T];
  
  beta_mu=Phi_approx(beta_mu_pr)*20;
  y_pred = rep_array(-999,S ,T);
  
  {
    for (s in 1:S){
      vector[2] Q;  // Q value for all actions
      Q = initQ;
      log_lik[s]=0;
      
      for (t in 1:T)  {
        log_lik[s] += categorical_logit_lpmf(choice[s,t] | beta[s]*Q );
        y_pred[s,t] = categorical_logit_rng( beta[s] * Q ); 
        
        Q[choice[s,t]]   = (reward[s,t] == 1)?1:(-1);
        Q[3-choice[s,t]] = (reward[s,t] == 1)?(-1):1;
      } // end of t loop (T trials)
    } // end of t loop (S subjects)
  }
}  
