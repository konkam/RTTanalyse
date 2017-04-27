#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4TKTD_one_species_mod) {


    class_<rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> >("model_TKTD_one_species")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_TKTD_one_species_namespace::model_TKTD_one_species, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4hierarchical_no_correlation_mod) {


    class_<rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> >("model_hierarchical_no_correlation")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4hierarchical_no_correlation_old_mod) {


    class_<rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> >("model_hierarchical_no_correlation_old")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_hierarchical_no_correlation_old_namespace::model_hierarchical_no_correlation_old, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4hierarchical_no_correlation_uniform_priors_mod) {


    class_<rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> >("model_hierarchical_no_correlation_uniform_priors")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_hierarchical_no_correlation_uniform_priors_namespace::model_hierarchical_no_correlation_uniform_priors, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4hierarchical_only_NEC_varies_mod) {


    class_<rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> >("model_hierarchical_only_NEC_varies")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_hierarchical_only_NEC_varies_namespace::model_hierarchical_only_NEC_varies, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
