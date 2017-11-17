#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4censored_correlations_mod) {


    class_<rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> >("model_censored_correlations")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_censored_correlations_namespace::model_censored_correlations, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4censored_correlations_interval_mod) {


    class_<rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> >("model_censored_correlations_interval")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_censored_correlations_interval_namespace::model_censored_correlations_interval, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4censored_correlations_unkown_l_mod) {


    class_<rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> >("model_censored_correlations_unkown_l")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_censored_correlations_unkown_l_namespace::model_censored_correlations_unkown_l, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4censored_correlations_z_mod) {


    class_<rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> >("model_censored_correlations_z")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_censored_correlations_z_namespace::model_censored_correlations_z, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4kendalls_tau_mod) {


    class_<rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> >("model_kendalls_tau")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_kendalls_tau_namespace::model_kendalls_tau, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
