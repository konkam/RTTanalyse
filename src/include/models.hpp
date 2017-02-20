
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.14

#include <stan/model/model_header.hpp>

namespace model_hierarchical_no_correlation_namespace {

using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;

typedef Eigen::Matrix<double,Eigen::Dynamic,1> vector_d;
typedef Eigen::Matrix<double,1,Eigen::Dynamic> row_vector_d;
typedef Eigen::Matrix<double,Eigen::Dynamic,Eigen::Dynamic> matrix_d;

static int current_statement_begin__;

class model_hierarchical_no_correlation : public prob_grad {
private:
    int ndat;
    int nspecies;
    double minc;
    double maxc;
    vector<double> x;
    vector<double> t;
    vector<double> tprec;
    vector<int> y;
    vector<int> Nprec;
    vector<int> species;
public:
    model_hierarchical_no_correlation(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        typedef boost::ecuyer1988 rng_t;
        rng_t base_rng(0);  // 0 seed default
        ctor_body(context__, base_rng, pstream__);
    }

    template <class RNG>
    model_hierarchical_no_correlation(stan::io::var_context& context__,
        RNG& base_rng__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, base_rng__, pstream__);
    }

    template <class RNG>
    void ctor_body(stan::io::var_context& context__,
                   RNG& base_rng__,
                   std::ostream* pstream__) {
        current_statement_begin__ = -1;

        static const char* function__ = "model_hierarchical_no_correlation_namespace::model_hierarchical_no_correlation";
        (void) function__; // dummy call to supress warning
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        double DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        // initialize member variables
        context__.validate_dims("data initialization", "ndat", "int", context__.to_vec());
        ndat = int(0);
        vals_i__ = context__.vals_i("ndat");
        pos__ = 0;
        ndat = vals_i__[pos__++];
        context__.validate_dims("data initialization", "nspecies", "int", context__.to_vec());
        nspecies = int(0);
        vals_i__ = context__.vals_i("nspecies");
        pos__ = 0;
        nspecies = vals_i__[pos__++];
        context__.validate_dims("data initialization", "minc", "double", context__.to_vec());
        minc = double(0);
        vals_r__ = context__.vals_r("minc");
        pos__ = 0;
        minc = vals_r__[pos__++];
        context__.validate_dims("data initialization", "maxc", "double", context__.to_vec());
        maxc = double(0);
        vals_r__ = context__.vals_r("maxc");
        pos__ = 0;
        maxc = vals_r__[pos__++];
        context__.validate_dims("data initialization", "x", "double", context__.to_vec(ndat));
        validate_non_negative_index("x", "ndat", ndat);
        x = std::vector<double>(ndat,double(0));
        vals_r__ = context__.vals_r("x");
        pos__ = 0;
        size_t x_limit_0__ = ndat;
        for (size_t i_0__ = 0; i_0__ < x_limit_0__; ++i_0__) {
            x[i_0__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "t", "double", context__.to_vec(ndat));
        validate_non_negative_index("t", "ndat", ndat);
        t = std::vector<double>(ndat,double(0));
        vals_r__ = context__.vals_r("t");
        pos__ = 0;
        size_t t_limit_0__ = ndat;
        for (size_t i_0__ = 0; i_0__ < t_limit_0__; ++i_0__) {
            t[i_0__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "tprec", "double", context__.to_vec(ndat));
        validate_non_negative_index("tprec", "ndat", ndat);
        tprec = std::vector<double>(ndat,double(0));
        vals_r__ = context__.vals_r("tprec");
        pos__ = 0;
        size_t tprec_limit_0__ = ndat;
        for (size_t i_0__ = 0; i_0__ < tprec_limit_0__; ++i_0__) {
            tprec[i_0__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "y", "int", context__.to_vec(ndat));
        validate_non_negative_index("y", "ndat", ndat);
        y = std::vector<int>(ndat,int(0));
        vals_i__ = context__.vals_i("y");
        pos__ = 0;
        size_t y_limit_0__ = ndat;
        for (size_t i_0__ = 0; i_0__ < y_limit_0__; ++i_0__) {
            y[i_0__] = vals_i__[pos__++];
        }
        context__.validate_dims("data initialization", "Nprec", "int", context__.to_vec(ndat));
        validate_non_negative_index("Nprec", "ndat", ndat);
        Nprec = std::vector<int>(ndat,int(0));
        vals_i__ = context__.vals_i("Nprec");
        pos__ = 0;
        size_t Nprec_limit_0__ = ndat;
        for (size_t i_0__ = 0; i_0__ < Nprec_limit_0__; ++i_0__) {
            Nprec[i_0__] = vals_i__[pos__++];
        }
        context__.validate_dims("data initialization", "species", "int", context__.to_vec(ndat));
        validate_non_negative_index("species", "ndat", ndat);
        species = std::vector<int>(ndat,int(0));
        vals_i__ = context__.vals_i("species");
        pos__ = 0;
        size_t species_limit_0__ = ndat;
        for (size_t i_0__ = 0; i_0__ < species_limit_0__; ++i_0__) {
            species[i_0__] = vals_i__[pos__++];
        }

        // validate, data variables
        check_greater_or_equal(function__,"ndat",ndat,0);
        check_greater_or_equal(function__,"nspecies",nspecies,0);
        check_greater_or_equal(function__,"minc",minc,0);
        check_greater_or_equal(function__,"maxc",maxc,0);
        for (int k0__ = 0; k0__ < ndat; ++k0__) {
            check_greater_or_equal(function__,"x[k0__]",x[k0__],0);
        }
        for (int k0__ = 0; k0__ < ndat; ++k0__) {
            check_greater_or_equal(function__,"t[k0__]",t[k0__],0);
        }
        for (int k0__ = 0; k0__ < ndat; ++k0__) {
            check_greater_or_equal(function__,"tprec[k0__]",tprec[k0__],0);
        }
        for (int k0__ = 0; k0__ < ndat; ++k0__) {
            check_greater_or_equal(function__,"y[k0__]",y[k0__],0);
        }
        for (int k0__ = 0; k0__ < ndat; ++k0__) {
            check_greater_or_equal(function__,"Nprec[k0__]",Nprec[k0__],0);
        }
        for (int k0__ = 0; k0__ < ndat; ++k0__) {
            check_greater_or_equal(function__,"species[k0__]",species[k0__],0);
        }
        // initialize data variables

        try {
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e,current_statement_begin__);
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate transformed data

        // set parameter ranges
        num_params_r__ = 0U;
        param_ranges_i__.clear();
        ++num_params_r__;
        ++num_params_r__;
        ++num_params_r__;
        ++num_params_r__;
        ++num_params_r__;
        ++num_params_r__;
        ++num_params_r__;
        ++num_params_r__;
        num_params_r__ += nspecies;
        num_params_r__ += nspecies;
        num_params_r__ += nspecies;
        num_params_r__ += nspecies;
    }

    ~model_hierarchical_no_correlation() { }


    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        stan::io::writer<double> writer__(params_r__,params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;

        if (!(context__.contains_r("lks_mu")))
            throw std::runtime_error("variable lks_mu missing");
        vals_r__ = context__.vals_r("lks_mu");
        pos__ = 0U;
        context__.validate_dims("initialization", "lks_mu", "double", context__.to_vec());
        // generate_declaration lks_mu
        double lks_mu(0);
        lks_mu = vals_r__[pos__++];
        try {
            writer__.scalar_lub_unconstrain(-(7),2,lks_mu);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lks_mu: ") + e.what());
        }

        if (!(context__.contains_r("lNEC_mu")))
            throw std::runtime_error("variable lNEC_mu missing");
        vals_r__ = context__.vals_r("lNEC_mu");
        pos__ = 0U;
        context__.validate_dims("initialization", "lNEC_mu", "double", context__.to_vec());
        // generate_declaration lNEC_mu
        double lNEC_mu(0);
        lNEC_mu = vals_r__[pos__++];
        try {
            writer__.scalar_lub_unconstrain(((log(minc) / log(10)) - 1),((log(maxc) / log(10)) + 1),lNEC_mu);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lNEC_mu: ") + e.what());
        }

        if (!(context__.contains_r("lke_mu")))
            throw std::runtime_error("variable lke_mu missing");
        vals_r__ = context__.vals_r("lke_mu");
        pos__ = 0U;
        context__.validate_dims("initialization", "lke_mu", "double", context__.to_vec());
        // generate_declaration lke_mu
        double lke_mu(0);
        lke_mu = vals_r__[pos__++];
        try {
            writer__.scalar_lub_unconstrain(-(7),2,lke_mu);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lke_mu: ") + e.what());
        }

        if (!(context__.contains_r("lm0_mu")))
            throw std::runtime_error("variable lm0_mu missing");
        vals_r__ = context__.vals_r("lm0_mu");
        pos__ = 0U;
        context__.validate_dims("initialization", "lm0_mu", "double", context__.to_vec());
        // generate_declaration lm0_mu
        double lm0_mu(0);
        lm0_mu = vals_r__[pos__++];
        try {
            writer__.scalar_lub_unconstrain(-(7),2,lm0_mu);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lm0_mu: ") + e.what());
        }

        if (!(context__.contains_r("lks_sigma")))
            throw std::runtime_error("variable lks_sigma missing");
        vals_r__ = context__.vals_r("lks_sigma");
        pos__ = 0U;
        context__.validate_dims("initialization", "lks_sigma", "double", context__.to_vec());
        // generate_declaration lks_sigma
        double lks_sigma(0);
        lks_sigma = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0,lks_sigma);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lks_sigma: ") + e.what());
        }

        if (!(context__.contains_r("lNEC_sigma")))
            throw std::runtime_error("variable lNEC_sigma missing");
        vals_r__ = context__.vals_r("lNEC_sigma");
        pos__ = 0U;
        context__.validate_dims("initialization", "lNEC_sigma", "double", context__.to_vec());
        // generate_declaration lNEC_sigma
        double lNEC_sigma(0);
        lNEC_sigma = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0,lNEC_sigma);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lNEC_sigma: ") + e.what());
        }

        if (!(context__.contains_r("lke_sigma")))
            throw std::runtime_error("variable lke_sigma missing");
        vals_r__ = context__.vals_r("lke_sigma");
        pos__ = 0U;
        context__.validate_dims("initialization", "lke_sigma", "double", context__.to_vec());
        // generate_declaration lke_sigma
        double lke_sigma(0);
        lke_sigma = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0,lke_sigma);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lke_sigma: ") + e.what());
        }

        if (!(context__.contains_r("lm0_sigma")))
            throw std::runtime_error("variable lm0_sigma missing");
        vals_r__ = context__.vals_r("lm0_sigma");
        pos__ = 0U;
        context__.validate_dims("initialization", "lm0_sigma", "double", context__.to_vec());
        // generate_declaration lm0_sigma
        double lm0_sigma(0);
        lm0_sigma = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0,lm0_sigma);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lm0_sigma: ") + e.what());
        }

        if (!(context__.contains_r("lm0")))
            throw std::runtime_error("variable lm0 missing");
        vals_r__ = context__.vals_r("lm0");
        pos__ = 0U;
        context__.validate_dims("initialization", "lm0", "vector_d", context__.to_vec(nspecies));
        // generate_declaration lm0
        vector_d lm0(static_cast<Eigen::VectorXd::Index>(nspecies));
        for (int j1__ = 0U; j1__ < nspecies; ++j1__)
            lm0(j1__) = vals_r__[pos__++];
        try {
            writer__.vector_unconstrain(lm0);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lm0: ") + e.what());
        }

        if (!(context__.contains_r("lks")))
            throw std::runtime_error("variable lks missing");
        vals_r__ = context__.vals_r("lks");
        pos__ = 0U;
        context__.validate_dims("initialization", "lks", "vector_d", context__.to_vec(nspecies));
        // generate_declaration lks
        vector_d lks(static_cast<Eigen::VectorXd::Index>(nspecies));
        for (int j1__ = 0U; j1__ < nspecies; ++j1__)
            lks(j1__) = vals_r__[pos__++];
        try {
            writer__.vector_unconstrain(lks);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lks: ") + e.what());
        }

        if (!(context__.contains_r("lNEC")))
            throw std::runtime_error("variable lNEC missing");
        vals_r__ = context__.vals_r("lNEC");
        pos__ = 0U;
        context__.validate_dims("initialization", "lNEC", "vector_d", context__.to_vec(nspecies));
        // generate_declaration lNEC
        vector_d lNEC(static_cast<Eigen::VectorXd::Index>(nspecies));
        for (int j1__ = 0U; j1__ < nspecies; ++j1__)
            lNEC(j1__) = vals_r__[pos__++];
        try {
            writer__.vector_unconstrain(lNEC);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lNEC: ") + e.what());
        }

        if (!(context__.contains_r("lke")))
            throw std::runtime_error("variable lke missing");
        vals_r__ = context__.vals_r("lke");
        pos__ = 0U;
        context__.validate_dims("initialization", "lke", "vector_d", context__.to_vec(nspecies));
        // generate_declaration lke
        vector_d lke(static_cast<Eigen::VectorXd::Index>(nspecies));
        for (int j1__ = 0U; j1__ < nspecies; ++j1__)
            lke(j1__) = vals_r__[pos__++];
        try {
            writer__.vector_unconstrain(lke);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable lke: ") + e.what());
        }

        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }

    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }


    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(vector<T__>& params_r__,
                 vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {

        T__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;

        // model parameters
        stan::io::reader<T__> in__(params_r__,params_i__);

        T__ lks_mu;
        (void) lks_mu;  // dummy to suppress unused var warning
        if (jacobian__)
            lks_mu = in__.scalar_lub_constrain(-(7),2,lp__);
        else
            lks_mu = in__.scalar_lub_constrain(-(7),2);

        T__ lNEC_mu;
        (void) lNEC_mu;  // dummy to suppress unused var warning
        if (jacobian__)
            lNEC_mu = in__.scalar_lub_constrain(((log(minc) / log(10)) - 1),((log(maxc) / log(10)) + 1),lp__);
        else
            lNEC_mu = in__.scalar_lub_constrain(((log(minc) / log(10)) - 1),((log(maxc) / log(10)) + 1));

        T__ lke_mu;
        (void) lke_mu;  // dummy to suppress unused var warning
        if (jacobian__)
            lke_mu = in__.scalar_lub_constrain(-(7),2,lp__);
        else
            lke_mu = in__.scalar_lub_constrain(-(7),2);

        T__ lm0_mu;
        (void) lm0_mu;  // dummy to suppress unused var warning
        if (jacobian__)
            lm0_mu = in__.scalar_lub_constrain(-(7),2,lp__);
        else
            lm0_mu = in__.scalar_lub_constrain(-(7),2);

        T__ lks_sigma;
        (void) lks_sigma;  // dummy to suppress unused var warning
        if (jacobian__)
            lks_sigma = in__.scalar_lb_constrain(0,lp__);
        else
            lks_sigma = in__.scalar_lb_constrain(0);

        T__ lNEC_sigma;
        (void) lNEC_sigma;  // dummy to suppress unused var warning
        if (jacobian__)
            lNEC_sigma = in__.scalar_lb_constrain(0,lp__);
        else
            lNEC_sigma = in__.scalar_lb_constrain(0);

        T__ lke_sigma;
        (void) lke_sigma;  // dummy to suppress unused var warning
        if (jacobian__)
            lke_sigma = in__.scalar_lb_constrain(0,lp__);
        else
            lke_sigma = in__.scalar_lb_constrain(0);

        T__ lm0_sigma;
        (void) lm0_sigma;  // dummy to suppress unused var warning
        if (jacobian__)
            lm0_sigma = in__.scalar_lb_constrain(0,lp__);
        else
            lm0_sigma = in__.scalar_lb_constrain(0);

        Eigen::Matrix<T__,Eigen::Dynamic,1>  lm0;
        (void) lm0;  // dummy to suppress unused var warning
        if (jacobian__)
            lm0 = in__.vector_constrain(nspecies,lp__);
        else
            lm0 = in__.vector_constrain(nspecies);

        Eigen::Matrix<T__,Eigen::Dynamic,1>  lks;
        (void) lks;  // dummy to suppress unused var warning
        if (jacobian__)
            lks = in__.vector_constrain(nspecies,lp__);
        else
            lks = in__.vector_constrain(nspecies);

        Eigen::Matrix<T__,Eigen::Dynamic,1>  lNEC;
        (void) lNEC;  // dummy to suppress unused var warning
        if (jacobian__)
            lNEC = in__.vector_constrain(nspecies,lp__);
        else
            lNEC = in__.vector_constrain(nspecies);

        Eigen::Matrix<T__,Eigen::Dynamic,1>  lke;
        (void) lke;  // dummy to suppress unused var warning
        if (jacobian__)
            lke = in__.vector_constrain(nspecies,lp__);
        else
            lke = in__.vector_constrain(nspecies);


        // transformed parameters


        try {
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e,current_statement_begin__);
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate transformed parameters

        const char* function__ = "validate transformed params";
        (void) function__;  // dummy to suppress unused var warning

        // model body
        try {
            {
                Eigen::Matrix<T__,Eigen::Dynamic,1>  ks(static_cast<Eigen::VectorXd::Index>(nspecies));
                (void) ks;  // dummy to suppress unused var warning
                stan::math::initialize(ks, DUMMY_VAR__);
                stan::math::fill(ks,DUMMY_VAR__);
                Eigen::Matrix<T__,Eigen::Dynamic,1>  NEC(static_cast<Eigen::VectorXd::Index>(nspecies));
                (void) NEC;  // dummy to suppress unused var warning
                stan::math::initialize(NEC, DUMMY_VAR__);
                stan::math::fill(NEC,DUMMY_VAR__);
                Eigen::Matrix<T__,Eigen::Dynamic,1>  ke(static_cast<Eigen::VectorXd::Index>(nspecies));
                (void) ke;  // dummy to suppress unused var warning
                stan::math::initialize(ke, DUMMY_VAR__);
                stan::math::fill(ke,DUMMY_VAR__);
                Eigen::Matrix<T__,Eigen::Dynamic,1>  m0(static_cast<Eigen::VectorXd::Index>(nspecies));
                (void) m0;  // dummy to suppress unused var warning
                stan::math::initialize(m0, DUMMY_VAR__);
                stan::math::fill(m0,DUMMY_VAR__);
                Eigen::Matrix<T__,Eigen::Dynamic,1>  psurv(static_cast<Eigen::VectorXd::Index>(ndat));
                (void) psurv;  // dummy to suppress unused var warning
                stan::math::initialize(psurv, DUMMY_VAR__);
                stan::math::fill(psurv,DUMMY_VAR__);
                Eigen::Matrix<T__,Eigen::Dynamic,1>  tNEC(static_cast<Eigen::VectorXd::Index>(ndat));
                (void) tNEC;  // dummy to suppress unused var warning
                stan::math::initialize(tNEC, DUMMY_VAR__);
                stan::math::fill(tNEC,DUMMY_VAR__);
                Eigen::Matrix<T__,Eigen::Dynamic,1>  tref(static_cast<Eigen::VectorXd::Index>(ndat));
                (void) tref;  // dummy to suppress unused var warning
                stan::math::initialize(tref, DUMMY_VAR__);
                stan::math::fill(tref,DUMMY_VAR__);


                lp_accum__.add(cauchy_log<propto__>(lke_sigma, 0, 2.5));
                lp_accum__.add(cauchy_log<propto__>(lks_sigma, 0, 2.5));
                lp_accum__.add(cauchy_log<propto__>(lm0_sigma, 0, 2.5));
                lp_accum__.add(cauchy_log<propto__>(lNEC_sigma, 0, 2.5));
                for (int i = 1; i <= nspecies; ++i) {

                    stan::math::assign(get_base1_lhs(m0,i,"m0",1), pow(10.0,get_base1(lm0,i,"lm0",1)));
                    stan::math::assign(get_base1_lhs(ks,i,"ks",1), pow(10.0,get_base1(lks,i,"lks",1)));
                    stan::math::assign(get_base1_lhs(NEC,i,"NEC",1), pow(10.0,get_base1(lNEC,i,"lNEC",1)));
                    stan::math::assign(get_base1_lhs(ke,i,"ke",1), pow(10.0,get_base1(lke,i,"lke",1)));
                }
                for (int i = 1; i <= ndat; ++i) {

                    stan::math::assign(get_base1_lhs(psurv,i,"psurv",1), exp((-(get_base1(m0,get_base1(species,i,"species",1),"m0",1)) * (get_base1(t,i,"t",1) - get_base1(tprec,i,"tprec",1)))));
                    if (as_bool(logical_gt(get_base1(x,i,"x",1),get_base1(NEC,get_base1(species,i,"species",1),"NEC",1)))) {

                        stan::math::assign(get_base1_lhs(tNEC,i,"tNEC",1), ((-(1) / get_base1(ke,get_base1(species,i,"species",1),"ke",1)) * log((1 - (get_base1(NEC,get_base1(species,i,"species",1),"NEC",1) / get_base1(x,i,"x",1))))));
                        if (as_bool(logical_gt(get_base1(t,i,"t",1),get_base1(tNEC,i,"tNEC",1)))) {

                            stan::math::assign(get_base1_lhs(tref,i,"tref",1), stan::math::fmax(get_base1(tprec,i,"tprec",1),get_base1(tNEC,i,"tNEC",1)));
                            stan::math::assign(get_base1_lhs(psurv,i,"psurv",1), (get_base1(psurv,i,"psurv",1) * exp((-(get_base1(ks,get_base1(species,i,"species",1),"ks",1)) * (((get_base1(x,i,"x",1) - get_base1(NEC,get_base1(species,i,"species",1),"NEC",1)) * (get_base1(t,i,"t",1) - get_base1(tref,i,"tref",1))) + (((1 / get_base1(ke,get_base1(species,i,"species",1),"ke",1)) * get_base1(x,i,"x",1)) * (exp((-(get_base1(ke,get_base1(species,i,"species",1),"ke",1)) * get_base1(t,i,"t",1))) - exp((-(get_base1(ke,get_base1(species,i,"species",1),"ke",1)) * get_base1(tref,i,"tref",1))))))))));
                        }
                    }
                    stan::math::assign(get_base1_lhs(psurv,i,"psurv",1), stan::math::fmax(get_base1(psurv,i,"psurv",1),pow(10.0,-(6.0))));
                }
                lp_accum__.add(normal_log<propto__>(lks, lks_mu, lks_sigma));
                lp_accum__.add(normal_log<propto__>(lNEC, lNEC_mu, lNEC_sigma));
                lp_accum__.add(normal_log<propto__>(lke, lke_mu, lke_sigma));
                lp_accum__.add(normal_log<propto__>(lm0, lm0_mu, lm0_sigma));
                lp_accum__.add(binomial_log<propto__>(y, Nprec, psurv));
            }
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e,current_statement_begin__);
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        lp_accum__.add(lp__);
        return lp_accum__.sum();

    } // log_prob()

    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }


    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("lks_mu");
        names__.push_back("lNEC_mu");
        names__.push_back("lke_mu");
        names__.push_back("lm0_mu");
        names__.push_back("lks_sigma");
        names__.push_back("lNEC_sigma");
        names__.push_back("lke_sigma");
        names__.push_back("lm0_sigma");
        names__.push_back("lm0");
        names__.push_back("lks");
        names__.push_back("lNEC");
        names__.push_back("lke");
    }


    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(nspecies);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(nspecies);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(nspecies);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(nspecies);
        dimss__.push_back(dims__);
    }

    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        vars__.resize(0);
        stan::io::reader<double> in__(params_r__,params_i__);
        static const char* function__ = "model_hierarchical_no_correlation_namespace::write_array";
        (void) function__; // dummy call to supress warning
        // read-transform, write parameters
        double lks_mu = in__.scalar_lub_constrain(-(7),2);
        double lNEC_mu = in__.scalar_lub_constrain(((log(minc) / log(10)) - 1),((log(maxc) / log(10)) + 1));
        double lke_mu = in__.scalar_lub_constrain(-(7),2);
        double lm0_mu = in__.scalar_lub_constrain(-(7),2);
        double lks_sigma = in__.scalar_lb_constrain(0);
        double lNEC_sigma = in__.scalar_lb_constrain(0);
        double lke_sigma = in__.scalar_lb_constrain(0);
        double lm0_sigma = in__.scalar_lb_constrain(0);
        vector_d lm0 = in__.vector_constrain(nspecies);
        vector_d lks = in__.vector_constrain(nspecies);
        vector_d lNEC = in__.vector_constrain(nspecies);
        vector_d lke = in__.vector_constrain(nspecies);
        vars__.push_back(lks_mu);
        vars__.push_back(lNEC_mu);
        vars__.push_back(lke_mu);
        vars__.push_back(lm0_mu);
        vars__.push_back(lks_sigma);
        vars__.push_back(lNEC_sigma);
        vars__.push_back(lke_sigma);
        vars__.push_back(lm0_sigma);
        for (int k_0__ = 0; k_0__ < nspecies; ++k_0__) {
            vars__.push_back(lm0[k_0__]);
        }
        for (int k_0__ = 0; k_0__ < nspecies; ++k_0__) {
            vars__.push_back(lks[k_0__]);
        }
        for (int k_0__ = 0; k_0__ < nspecies; ++k_0__) {
            vars__.push_back(lNEC[k_0__]);
        }
        for (int k_0__ = 0; k_0__ < nspecies; ++k_0__) {
            vars__.push_back(lke[k_0__]);
        }

        if (!include_tparams__) return;
        // declare and define transformed parameters
        double lp__ = 0.0;
        (void) lp__; // dummy call to supress warning
        stan::math::accumulator<double> lp_accum__;

        double DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning



        try {
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e,current_statement_begin__);
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate transformed parameters

        // write transformed parameters

        if (!include_gqs__) return;
        // declare and define generated quantities


        try {
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e,current_statement_begin__);
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate generated quantities

        // write generated quantities
    }

    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng,params_r_vec,params_i_vec,vars_vec,include_tparams,include_gqs,pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    static std::string model_name() {
        return "model_hierarchical_no_correlation";
    }


    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "lks_mu";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lNEC_mu";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lke_mu";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lm0_mu";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lks_sigma";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lNEC_sigma";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lke_sigma";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lm0_sigma";
        param_names__.push_back(param_name_stream__.str());
        for (int k_0__ = 1; k_0__ <= nspecies; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "lm0" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= nspecies; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "lks" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= nspecies; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "lNEC" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= nspecies; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "lke" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (!include_gqs__) return;
    }


    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "lks_mu";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lNEC_mu";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lke_mu";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lm0_mu";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lks_sigma";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lNEC_sigma";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lke_sigma";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "lm0_sigma";
        param_names__.push_back(param_name_stream__.str());
        for (int k_0__ = 1; k_0__ <= nspecies; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "lm0" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= nspecies; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "lks" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= nspecies; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "lNEC" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= nspecies; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "lke" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (!include_gqs__) return;
    }

}; // model

} // namespace




#endif
