pred_run = function(modelStr, test = TRUE, fitObj = NA, saveFit = FALSE, suffix=NULL,
                       study = NULL, block = NULL, grp = NULL, seed = NULL, nopar = NULL,
                       nIter = 2000, nwarmup = NULL, nCore = NULL,
                       adapt = 0.8, treedepth = 10, nThinn = 1 ) {
    
    library(rstan); library(parallel); library(loo); library(hBayesDM)
    L = list()
    
    # block 1 - volatile
    # block 2 - stable
    
    #### prepare data #### ===========================================================================
    
    if (study == 2 ) {
        load(sprintf("data/study_%s/stanb%s.rdata", study, block))
    } else if (study == 3 ) {
        load(sprintf("data/study_%s/stanb%s_%s.rdata", study, block, grp))
    } 
    
    if (block == 1) {
        dataList = stanb1
        rm(stanb1)
    } else if (block == 2) {
        dataList = stanb2
        rm(stanb2)
        reset = rep(0, 160)
        reset[c(1,21,45,65,81,97,121,137)] = 1
        dataList$reset = reset
    }
    dataList$reward[dataList$reward==0] = -1
    dataList$choice = dataList$choice + 1
    
    #### preparation for running stan #### ============================================================
    # model string in a separate .stan file
    modelFile = paste0("scripts/stan/",modelStr,".stan")
    
    # setup up Stan configuration
    if (test == TRUE) {
        options(mc.cores = 1)
        nSamples = 4
        nChains  = 1 
        nBurnin  = 0
        nThin    = 1
        
        est_seed = sample.int(.Machine$integer.max, 1)
        
    } else {
        if (is.null(nCore)) {
            options(mc.cores = 4) 
        } else {
            options(mc.cores = nCore)  
        }
        
        if (is.null(seed)) {
            est_seed = sample.int(.Machine$integer.max, 1) 
        } else {
            est_seed = seed  
        }
        
        nSamples = nIter
        nChains  = 4
        if (is.null(nwarmup)) {
            nBurnin = floor(nSamples/2)
        } else {
            nBurnin = nwarmup
        }
        nThin    = nThinn
    }
    if (is.null(nopar)) {
        algrth   = 'NUTS' 
    } else {
        algrth   = 'Fixed_param'
    }
    
    # parameter of interest (this could save both memory and space)
    poi = create_pois(modelStr)
    
    #### run stan ####  ==============================================================================
    cat("Estimating", modelStr, "model, study", study, ", block", block, " ... \n")
    cat("Variant: ", modelStr, suffix, " ... \n", sep = "")
    startTime = Sys.time(); print(startTime)
    cat("Calling", nChains, "simulations in Stan... \n")
    cat("Running with", dataList$S, "participants... \n")
    
    
    rstan_options(auto_write = TRUE)
    
    stanfit = stan(modelFile,
                    fit     = fitObj,
                    data    = dataList,
                    pars    = poi$pars,
                    chains  = nChains,
                    iter    = nSamples,
                    warmup  = nBurnin,
                    thin    = nThin,
                    algorithm = algrth,
                    init    = "random",
                    seed    = est_seed,
                    control = list(adapt_delta = adapt, max_treedepth = treedepth),
                    verbose = FALSE)
    
    cat("Finishing", modelStr, "model simulation ... \n")
    endTime = Sys.time(); print(endTime)  
    cat("It took",as.character.Date(endTime - startTime), "\n")
    
    L$data = dataList
    L$fit  = stanfit
    
    class(L) = "hBayesDM"
    
    if (saveFit == TRUE) {
        if (study == 2) {
            saveRDS(L, file = paste0('stanfits/', 'study', study, '_block', block, '_', modelStr, '_ppc', suffix, '.RData'))
        } else if (study == 3) {
            saveRDS(L, file = paste0('stanfits/', 'study', study, '_block', block, '_group', toupper(grp),
                                     '_', modelStr, '_ppc', suffix, '.RData'))
        }
    }
    
    
    # model diag information
    rh = brms::rhat(L$fit, pars = poi$pars_chk)
    cat(' # --- rhat range:', round(range(rh, na.rm = TRUE), 4), '\n')
    cat(' # --- rhat above 1.10: N =', sum(rh > 1.1, na.rm = TRUE), '\n')
    
    cat(' # --- LOOIC: \n')
    L_mat = loo::extract_log_lik(L$fit, 'log_lik')
    rel_n_eff = loo::relative_eff(exp(L_mat), chain_id = rep(1:nChains, each = nSamples - nBurnin))
    print(suppressWarnings(loo::loo(L_mat, r_eff = rel_n_eff)))
    
    # returning outputs
    return(L)
}  # function run_model()


#### nested functions #### ===========================================================================

# ------------------------------------------------------------------------------------------------------
create_pois = function(model){
    pois = list()
    
    
    if (model == "wsls0") {
        pars = c("y_pred", "log_lik")
        pars_chk = c("log_lik")
        
    } else if (model == "wsls") {
        pars = c("beta_mu", "beta_sd", "beta",
                 "y_pred", "log_lik",
                 "lp__")
        pars_chk = c("beta_mu", "beta_sd", "beta",
                     "log_lik", "lp__")
        
    } else if (model == "RL_rw" || model == "RL_rw_reset") {
        pars = c("alpha_mu", "beta_mu", 
                 "alpha_sd", "beta_sd", 
                 "alpha", "beta",
                 "y_pred", "log_lik",
                 "chosenQ", "unchosenQ", "pe",
                 "lp__")
        pars_chk = c("alpha_mu", "beta_mu", "alpha_sd", "beta_sd", 
                 "alpha", "beta", "log_lik", "lp__")
        
    } else if (model == "RL_rp" || model == "RL_rp_reset") {
        pars = c("alpha_pos_mu", "alpha_neg_mu", "beta_mu", 
                 "alpha_pos_sd", "alpha_neg_sd", "beta_sd", 
                 "alpha_pos", "alpha_neg", "beta",
                 "y_pred", "log_lik",
                 "chosenQ", "unchosenQ", "pe",
                 "lp__")
        pars_chk = c("alpha_pos_mu", "alpha_neg_mu", "beta_mu", 
                     "alpha_pos_sd", "alpha_neg_sd", "beta_sd", 
                     "alpha_pos", "alpha_neg", "beta",
                     "log_lik", "lp__")
        
    } else if (model == "RL_fict" || model == "RL_fict_reset") {
        pars = c("alpha_mu", "beta_mu", 
                 "alpha_sd", "beta_sd", 
                 "alpha", "beta",
                 "y_pred", "log_lik",
                 "chosenQ", "unchosenQ", "pe", "pe_nc",
                 "lp__")
        pars_chk = c("alpha_mu", "beta_mu", "alpha_sd", "beta_sd", 
                     "alpha", "beta", "log_lik", "lp__")
        
    } else if (model == "RL_ph" || model == "RL_ph_reset") {
        pars = c("eta_mu", "k_mu", "alpha0_mu", "beta_mu", 
                 "eta_sd", "eta_sd", "alpha0_sd", "beta_sd", 
                 "eta", "k","alpha0", "beta",
                 "y_pred", "log_lik",
                 "chosenQ", "unchosenQ", "alpha",
                 "lp__")
        pars_chk = c("eta_mu", "k_mu", "alpha0_mu", "beta_mu", 
                     "eta_sd", "eta_sd", "alpha0_sd", "beta_sd", 
                     "eta", "k","alpha0", "beta", "log_lik", "lp__")
        
    } else if ( model == "hmm" || model == "hmm_reset") {
        pars = c("gamma_mu", "c_mu", "beta_mu",
                 "gamma_sd", "c_sd",  "beta_sd", 
                 "gamma", "c", "beta",
                 "y_pred", "log_lik",
                 "entropy", "surprise", "p",
                 "lp__")
        pars_chk = c("gamma_mu", "c_mu", "beta_mu",
                     "gamma_sd", "c_sd",  "beta_sd", 
                     "gamma", "c", "beta",
                     "log_lik", "lp__")
        
    } else if ( model == "hmm0" || model == "hmm0_reset") {
        pars = c("gamma_mu", "c_mu", 
                 "gamma_sd", "c_sd",  
                 "gamma", "c",
                 "y_pred", "log_lik",
                 "entropy", "surprise", "p",
                 "lp__")
        pars_chk = c("gamma_mu", "c_mu",
                     "gamma_sd", "c_sd", 
                     "gamma", "c",
                     "log_lik", "lp__")
        
    }else if ( model == "hmm_rp" || model == "hmm_rp_reset") {
        pars = c("gamma_mu", "c_mu", "d_mu", "beta_mu",
                 "gamma_sd", "c_sd", "d_sd", "beta_sd", 
                 "gamma", "c", "d", "beta",
                 "y_pred", "log_lik",
                 "entropy", "surprise", "p",
                 "lp__")
        pars_chk = c("gamma_mu", "c_mu", "d_mu", "beta_mu",
                 "gamma_sd", "c_sd", "d_sd", "beta_sd", 
                 "gamma", "c", "d", "beta",
                 "log_lik", "lp__")
        
    } else if ( model == "hmm0_rp" || model == "hmm0_rp_reset") {
        pars = c("gamma_mu", "c_mu", "d_mu",
                 "gamma_sd", "c_sd", "d_sd", 
                 "gamma", "c", "d",
                 "y_pred", "log_lik",
                 "entropy", "surprise", "p",
                 "lp__")
        pars_chk = c("gamma_mu", "c_mu", "d_mu",
                     "gamma_sd", "c_sd", "d_sd", 
                     "gamma", "c", "d",
                     "log_lik", "lp__")
        
    } else if ( model == "bl" || model == "bl0") {
        pars = c("v0_mu", "k_mu", 
                 "v0_sd", "k_sd",
                 "v0", "k",
                 "y_pred", "log_lik",
                 "lp__")
        pars_chk = c("v0_mu", "k_mu", 
                     "v0_sd", "k_sd",
                     "v0", "k",
                     "log_lik",
                     "lp__")
        
    }
    
    pois$pars = pars
    pois$pars_chk = pars_chk
    
    return(pois)
} # function

#### end of function ####
