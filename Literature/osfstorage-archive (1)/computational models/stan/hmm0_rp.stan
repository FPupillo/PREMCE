data {
  int<lower=1> S; //subject
  int<lower=1> T; // num trials 
  int<lower=-1, upper=1> reward[S,T]; // reward set to -1 and 1
  int<lower=1, upper=2> choice[S,T]; // originally 0=left, 1=right 
}

transformed data{
  vector[2] init_Ps; // initial prob/belief of the two states
  init_Ps = rep_vector(0.5, 2);
}

parameters {
  // group-level parameters
  real gamma_mu_pr; // transition probability, gamma means reversal, 1 - gamma means stay [1-gamma, gamma; gamma, 1 - gamma]
  real c_mu_pr;     // emission for reward
  real d_mu_pr;     // emission for punishment
  real<lower=0> gamma_sd;
  real<lower=0> c_sd;
  real<lower=0> d_sd;
  
  // individual-level parameters
  vector[S] gamma_pr; 
  vector[S] c_pr; 
  vector[S] d_pr; 
}

transformed parameters{
  vector<lower=0, upper=1>[S] gamma;
  vector<lower=0.5, upper=1>[S] c;
  vector<lower=0.5, upper=1>[S] d;
  
  gamma = Phi_approx(gamma_mu_pr + gamma_sd * gamma_pr);
  c = Phi_approx(c_mu_pr + c_sd * c_pr) * 0.5 + 0.5;
  d = Phi_approx(d_mu_pr + d_sd * d_pr) * 0.5 + 0.5;
}

model {  
  // prior of group-level parameters
  gamma_mu_pr ~ normal(0,1);
  c_mu_pr ~ normal(0,1);
  d_mu_pr ~ normal(0,1);
  
  gamma_sd ~ cauchy(0,2);
  c_sd ~ cauchy(0,2);
  d_sd ~ cauchy(0,2);
  
  // prior of individual-level parameters
  gamma_pr ~ normal(0,1);
  c_pr ~ normal(0,1);
  d_pr ~ normal(0,1);  
  
  {
    for (s in 1:S){
      vector[2] Ps;  // prob of the states, 1 - left, 2 - right
      real P_O_S1;   // p(O|S1) - O = {A,R} given left
      real P_O_S2;   // p(O|S2) - O = {A,R} given right
      
      for (t in 1:T)  {
        // State update using the transition matrix 
        // from S[t-1] to S[t], BEFORE observing the outcome
        if (t == 1) {
          Ps = init_Ps;
        } else {
           Ps[1] = Ps[1] * (1-gamma[s]) + Ps[2] * gamma[s];
           Ps[2] = 1 - Ps[1];
        }

        // action selection based on the State probability 
        choice[s,t] ~ categorical( Ps); 

        // renew of emission prob: p(O|S1) p(O|S2); O is a pair between A(ction) and R(eward)
        // --> the probability of actually observing this outcome
        // Conditional Operator (a ? b : c) --> b if a is true, c if a is false
        if (reward[s,t] == 1) {
          P_O_S1 = 0.5 * ( (choice[s,t] == 1)?c[s]:(1-c[s]) );
          P_O_S2 = 0.5 * ( (choice[s,t] == 2)?c[s]:(1-c[s]) );
        } else if (reward[s,t] == -1) {
          P_O_S1 = 0.5 * ( (choice[s,t] == 1)?(1-d[s]):d[s] );
          P_O_S2 = 0.5 * ( (choice[s,t] == 2)?(1-d[s]):d[s] );
        }
        
        // State belief update using Bayesian rule, after observing the outcome
        if (( P_O_S1 * Ps[1] + P_O_S2 * Ps[2]) != 0) {
          Ps[1] = (P_O_S1 * Ps[1]) / ( P_O_S1 * Ps[1] + P_O_S2 * Ps[2]);
          Ps[2] = 1 - Ps[1];
        }
      } // trial loop
    } // subj loop
  } // local block
}

generated quantities {
  real <lower=0,upper=1> gamma_mu;
  real <lower=0.5,upper=1> c_mu;
  real <lower=0.5,upper=1> d_mu;    
  real p[S,T];
  real entropy[S,T];
  real surprise[S,T];
  real log_lik[S];
  int  y_pred[S, T];

  gamma_mu = Phi_approx(gamma_mu_pr);
  c_mu = Phi_approx(c_mu_pr) * 0.5 + 0.5;
  d_mu = Phi_approx(d_mu_pr) * 0.5 + 0.5;
  y_pred = rep_array(-999,S ,T);
  
  {
    for (s in 1:S){
      vector[2] Ps_pre;  // before observations
      vector[2] Ps_post; // after observations
      real P_O_S1;   
      real P_O_S2;   
      log_lik[s]=0;
      
      for (t in 1:T)  {
        if (t == 1) {
          Ps_pre = init_Ps;
        } else {
          Ps_pre[1] = Ps_post[1] * (1-gamma[s]) + Ps_post[2] * gamma[s];
          Ps_pre[2] = 1 - Ps_pre[1];
        }
        
        p[s,t] = Ps_pre[1];

        log_lik[s] +=  categorical_lpmf( choice[s,t] | Ps_pre ); 
        y_pred[s,t] = categorical_rng( Ps_pre ); 

        if (reward[s,t] == 1) {
          P_O_S1 = 0.5 * ( (choice[s,t] == 1)?c[s]:(1-c[s]) );
          P_O_S2 = 0.5 * ( (choice[s,t] == 2)?c[s]:(1-c[s]) );
        } else if (reward[s,t] == -1) {
          P_O_S1 = 0.5 * ( (choice[s,t] == 1)?(1-d[s]):d[s] );
          P_O_S2 = 0.5 * ( (choice[s,t] == 2)?(1-d[s]):d[s] );
        }

        Ps_post[1] = (P_O_S1 * Ps_pre[1]) / ( P_O_S1 * Ps_pre[1] + P_O_S2 * Ps_pre[2]);
        Ps_post[2] = 1 - Ps_post[1];
        
        // compute entropy
        if (Ps_post[1] == 0 || Ps_post[1] == 1) 
          entropy[s,t] = 0;
        else
          entropy[s,t] = -(Ps_post[1]*log(Ps_post[1]) + Ps_post[2]*log(Ps_post[2]));
          
        // compute bayesian surprise
        if (Ps_post[1] == 0)
          surprise[s,t] = Ps_post[2] * log(Ps_post[2]/Ps_pre[2]);
        else if ( Ps_post[1] == 1)
          surprise[s,t] = Ps_post[1] * log(Ps_post[1]/Ps_pre[1]); 
        else  
          surprise[s,t] = Ps_post[1] * log(Ps_post[1]/Ps_pre[1]) + Ps_post[2] * log(Ps_post[2]/Ps_pre[2]);
        
      } // trial loop
    } // subj loop
  }
}  
