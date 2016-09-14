// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef __Countr_RcppExports_h__
#define __Countr_RcppExports_h__

#include "Countr_types.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>

namespace Countr {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("Countr", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("Countr", "Countr_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in Countr");
            }
        }
    }

    inline arma::vec dWeibullgammaCount_mat(arma::Col<unsigned> x, double shape, double r, double alpha, double time = 1.0, bool logFlag = false, unsigned jmax = 100) {
        typedef SEXP(*Ptr_dWeibullgammaCount_mat)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullgammaCount_mat p_dWeibullgammaCount_mat = NULL;
        if (p_dWeibullgammaCount_mat == NULL) {
            validateSignature("arma::vec(*dWeibullgammaCount_mat)(arma::Col<unsigned>,double,double,double,double,bool,unsigned)");
            p_dWeibullgammaCount_mat = (Ptr_dWeibullgammaCount_mat)R_GetCCallable("Countr", "Countr_dWeibullgammaCount_mat");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullgammaCount_mat(Rcpp::wrap(x), Rcpp::wrap(shape), Rcpp::wrap(r), Rcpp::wrap(alpha), Rcpp::wrap(time), Rcpp::wrap(logFlag), Rcpp::wrap(jmax));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

    inline arma::vec dWeibullgammaCount_mat_vec(arma::Col<unsigned> x, arma::vec shape, double r, double alpha, double time = 1.0, bool logFlag = false, unsigned jmax = 100) {
        typedef SEXP(*Ptr_dWeibullgammaCount_mat_vec)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullgammaCount_mat_vec p_dWeibullgammaCount_mat_vec = NULL;
        if (p_dWeibullgammaCount_mat_vec == NULL) {
            validateSignature("arma::vec(*dWeibullgammaCount_mat_vec)(arma::Col<unsigned>,arma::vec,double,double,double,bool,unsigned)");
            p_dWeibullgammaCount_mat_vec = (Ptr_dWeibullgammaCount_mat_vec)R_GetCCallable("Countr", "Countr_dWeibullgammaCount_mat_vec");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullgammaCount_mat_vec(Rcpp::wrap(x), Rcpp::wrap(shape), Rcpp::wrap(r), Rcpp::wrap(alpha), Rcpp::wrap(time), Rcpp::wrap(logFlag), Rcpp::wrap(jmax));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

    inline arma::vec dWeibullgammaCount_mat_Covariates(arma::Col<unsigned> x, double cc, double r, double alpha, arma::mat Xcovar, arma::vec beta, double t = 1.0, bool logFlag = false, unsigned jmax = 100) {
        typedef SEXP(*Ptr_dWeibullgammaCount_mat_Covariates)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullgammaCount_mat_Covariates p_dWeibullgammaCount_mat_Covariates = NULL;
        if (p_dWeibullgammaCount_mat_Covariates == NULL) {
            validateSignature("arma::vec(*dWeibullgammaCount_mat_Covariates)(arma::Col<unsigned>,double,double,double,arma::mat,arma::vec,double,bool,unsigned)");
            p_dWeibullgammaCount_mat_Covariates = (Ptr_dWeibullgammaCount_mat_Covariates)R_GetCCallable("Countr", "Countr_dWeibullgammaCount_mat_Covariates");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullgammaCount_mat_Covariates(Rcpp::wrap(x), Rcpp::wrap(cc), Rcpp::wrap(r), Rcpp::wrap(alpha), Rcpp::wrap(Xcovar), Rcpp::wrap(beta), Rcpp::wrap(t), Rcpp::wrap(logFlag), Rcpp::wrap(jmax));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

    inline arma::vec dWeibullgammaCount_mat_Covariates_vec(arma::Col<unsigned> x, arma::vec cc, double r, double alpha, arma::mat Xcovar, arma::vec beta, double t = 1.0, bool logFlag = false, unsigned jmax = 100) {
        typedef SEXP(*Ptr_dWeibullgammaCount_mat_Covariates_vec)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullgammaCount_mat_Covariates_vec p_dWeibullgammaCount_mat_Covariates_vec = NULL;
        if (p_dWeibullgammaCount_mat_Covariates_vec == NULL) {
            validateSignature("arma::vec(*dWeibullgammaCount_mat_Covariates_vec)(arma::Col<unsigned>,arma::vec,double,double,arma::mat,arma::vec,double,bool,unsigned)");
            p_dWeibullgammaCount_mat_Covariates_vec = (Ptr_dWeibullgammaCount_mat_Covariates_vec)R_GetCCallable("Countr", "Countr_dWeibullgammaCount_mat_Covariates_vec");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullgammaCount_mat_Covariates_vec(Rcpp::wrap(x), Rcpp::wrap(cc), Rcpp::wrap(r), Rcpp::wrap(alpha), Rcpp::wrap(Xcovar), Rcpp::wrap(beta), Rcpp::wrap(t), Rcpp::wrap(logFlag), Rcpp::wrap(jmax));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

    inline arma::vec dWeibullgammaCount_acc(arma::Col<unsigned> x, double shape, double r, double alpha, double time = 1.0, bool logFlag = false, unsigned jmax = 100, int nmax = 300, double eps = 1e-10, bool printa = false) {
        typedef SEXP(*Ptr_dWeibullgammaCount_acc)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullgammaCount_acc p_dWeibullgammaCount_acc = NULL;
        if (p_dWeibullgammaCount_acc == NULL) {
            validateSignature("arma::vec(*dWeibullgammaCount_acc)(arma::Col<unsigned>,double,double,double,double,bool,unsigned,int,double,bool)");
            p_dWeibullgammaCount_acc = (Ptr_dWeibullgammaCount_acc)R_GetCCallable("Countr", "Countr_dWeibullgammaCount_acc");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullgammaCount_acc(Rcpp::wrap(x), Rcpp::wrap(shape), Rcpp::wrap(r), Rcpp::wrap(alpha), Rcpp::wrap(time), Rcpp::wrap(logFlag), Rcpp::wrap(jmax), Rcpp::wrap(nmax), Rcpp::wrap(eps), Rcpp::wrap(printa));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

    inline arma::vec dWeibullgammaCount_acc_vec(arma::Col<unsigned> x, arma::vec shape, double r, double alpha, double time = 1.0, bool logFlag = false, unsigned jmax = 100, int nmax = 300, double eps = 1e-10, bool printa = false) {
        typedef SEXP(*Ptr_dWeibullgammaCount_acc_vec)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullgammaCount_acc_vec p_dWeibullgammaCount_acc_vec = NULL;
        if (p_dWeibullgammaCount_acc_vec == NULL) {
            validateSignature("arma::vec(*dWeibullgammaCount_acc_vec)(arma::Col<unsigned>,arma::vec,double,double,double,bool,unsigned,int,double,bool)");
            p_dWeibullgammaCount_acc_vec = (Ptr_dWeibullgammaCount_acc_vec)R_GetCCallable("Countr", "Countr_dWeibullgammaCount_acc_vec");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullgammaCount_acc_vec(Rcpp::wrap(x), Rcpp::wrap(shape), Rcpp::wrap(r), Rcpp::wrap(alpha), Rcpp::wrap(time), Rcpp::wrap(logFlag), Rcpp::wrap(jmax), Rcpp::wrap(nmax), Rcpp::wrap(eps), Rcpp::wrap(printa));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

    inline arma::vec dWeibullgammaCount_acc_Covariates(arma::Col<unsigned> x, double cc, double r, double alpha, arma::mat Xcovar, arma::vec beta, double t = 1.0, bool logFlag = false, unsigned jmax = 100, int nmax = 300, double eps = 1e-10, bool printa = false) {
        typedef SEXP(*Ptr_dWeibullgammaCount_acc_Covariates)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullgammaCount_acc_Covariates p_dWeibullgammaCount_acc_Covariates = NULL;
        if (p_dWeibullgammaCount_acc_Covariates == NULL) {
            validateSignature("arma::vec(*dWeibullgammaCount_acc_Covariates)(arma::Col<unsigned>,double,double,double,arma::mat,arma::vec,double,bool,unsigned,int,double,bool)");
            p_dWeibullgammaCount_acc_Covariates = (Ptr_dWeibullgammaCount_acc_Covariates)R_GetCCallable("Countr", "Countr_dWeibullgammaCount_acc_Covariates");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullgammaCount_acc_Covariates(Rcpp::wrap(x), Rcpp::wrap(cc), Rcpp::wrap(r), Rcpp::wrap(alpha), Rcpp::wrap(Xcovar), Rcpp::wrap(beta), Rcpp::wrap(t), Rcpp::wrap(logFlag), Rcpp::wrap(jmax), Rcpp::wrap(nmax), Rcpp::wrap(eps), Rcpp::wrap(printa));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

    inline arma::vec dWeibullgammaCount_acc_Covariates_vec(arma::Col<unsigned> x, arma::vec cc, double r, double alpha, arma::mat Xcovar, arma::vec beta, double t = 1.0, bool logFlag = false, unsigned jmax = 100, int nmax = 300, double eps = 1e-10, bool printa = false) {
        typedef SEXP(*Ptr_dWeibullgammaCount_acc_Covariates_vec)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullgammaCount_acc_Covariates_vec p_dWeibullgammaCount_acc_Covariates_vec = NULL;
        if (p_dWeibullgammaCount_acc_Covariates_vec == NULL) {
            validateSignature("arma::vec(*dWeibullgammaCount_acc_Covariates_vec)(arma::Col<unsigned>,arma::vec,double,double,arma::mat,arma::vec,double,bool,unsigned,int,double,bool)");
            p_dWeibullgammaCount_acc_Covariates_vec = (Ptr_dWeibullgammaCount_acc_Covariates_vec)R_GetCCallable("Countr", "Countr_dWeibullgammaCount_acc_Covariates_vec");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullgammaCount_acc_Covariates_vec(Rcpp::wrap(x), Rcpp::wrap(cc), Rcpp::wrap(r), Rcpp::wrap(alpha), Rcpp::wrap(Xcovar), Rcpp::wrap(beta), Rcpp::wrap(t), Rcpp::wrap(logFlag), Rcpp::wrap(jmax), Rcpp::wrap(nmax), Rcpp::wrap(eps), Rcpp::wrap(printa));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

    inline arma::mat alphagen(double cc, unsigned jrow, unsigned ncol) {
        typedef SEXP(*Ptr_alphagen)(SEXP,SEXP,SEXP);
        static Ptr_alphagen p_alphagen = NULL;
        if (p_alphagen == NULL) {
            validateSignature("arma::mat(*alphagen)(double,unsigned,unsigned)");
            p_alphagen = (Ptr_alphagen)R_GetCCallable("Countr", "Countr_alphagen");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_alphagen(Rcpp::wrap(cc), Rcpp::wrap(jrow), Rcpp::wrap(ncol));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::mat >(__result);
    }

    inline arma::vec dWeibullCount_mat(arma::Col<unsigned> x, double shape, double scale, double time = 1.0, bool logFlag = false, unsigned jmax = 50) {
        typedef SEXP(*Ptr_dWeibullCount_mat)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullCount_mat p_dWeibullCount_mat = NULL;
        if (p_dWeibullCount_mat == NULL) {
            validateSignature("arma::vec(*dWeibullCount_mat)(arma::Col<unsigned>,double,double,double,bool,unsigned)");
            p_dWeibullCount_mat = (Ptr_dWeibullCount_mat)R_GetCCallable("Countr", "Countr_dWeibullCount_mat");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullCount_mat(Rcpp::wrap(x), Rcpp::wrap(shape), Rcpp::wrap(scale), Rcpp::wrap(time), Rcpp::wrap(logFlag), Rcpp::wrap(jmax));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

    inline double dWeibullCount_mat_scalar(unsigned x, double shape, double scale, double time = 1.0, bool logFlag = false, unsigned jmax = 50) {
        typedef SEXP(*Ptr_dWeibullCount_mat_scalar)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullCount_mat_scalar p_dWeibullCount_mat_scalar = NULL;
        if (p_dWeibullCount_mat_scalar == NULL) {
            validateSignature("double(*dWeibullCount_mat_scalar)(unsigned,double,double,double,bool,unsigned)");
            p_dWeibullCount_mat_scalar = (Ptr_dWeibullCount_mat_scalar)R_GetCCallable("Countr", "Countr_dWeibullCount_mat_scalar");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullCount_mat_scalar(Rcpp::wrap(x), Rcpp::wrap(shape), Rcpp::wrap(scale), Rcpp::wrap(time), Rcpp::wrap(logFlag), Rcpp::wrap(jmax));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<double >(__result);
    }

    inline arma::vec dWeibullCount_mat_vec(arma::Col<unsigned> x, arma::vec shape, arma::vec scale, double time = 1.0, bool logFlag = false, unsigned jmax = 50) {
        typedef SEXP(*Ptr_dWeibullCount_mat_vec)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullCount_mat_vec p_dWeibullCount_mat_vec = NULL;
        if (p_dWeibullCount_mat_vec == NULL) {
            validateSignature("arma::vec(*dWeibullCount_mat_vec)(arma::Col<unsigned>,arma::vec,arma::vec,double,bool,unsigned)");
            p_dWeibullCount_mat_vec = (Ptr_dWeibullCount_mat_vec)R_GetCCallable("Countr", "Countr_dWeibullCount_mat_vec");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullCount_mat_vec(Rcpp::wrap(x), Rcpp::wrap(shape), Rcpp::wrap(scale), Rcpp::wrap(time), Rcpp::wrap(logFlag), Rcpp::wrap(jmax));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

    inline arma::vec dWeibullCount_acc(arma::Col<unsigned> x, double shape, double scale, double time = 1.0, bool logFlag = false, unsigned jmax = 50, int nmax = 300, double eps = 1e-10, bool printa = false) {
        typedef SEXP(*Ptr_dWeibullCount_acc)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullCount_acc p_dWeibullCount_acc = NULL;
        if (p_dWeibullCount_acc == NULL) {
            validateSignature("arma::vec(*dWeibullCount_acc)(arma::Col<unsigned>,double,double,double,bool,unsigned,int,double,bool)");
            p_dWeibullCount_acc = (Ptr_dWeibullCount_acc)R_GetCCallable("Countr", "Countr_dWeibullCount_acc");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullCount_acc(Rcpp::wrap(x), Rcpp::wrap(shape), Rcpp::wrap(scale), Rcpp::wrap(time), Rcpp::wrap(logFlag), Rcpp::wrap(jmax), Rcpp::wrap(nmax), Rcpp::wrap(eps), Rcpp::wrap(printa));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

    inline arma::vec dWeibullCount_acc_vec(arma::Col<unsigned> x, arma::vec shape, arma::vec scale, double time = 1.0, bool logFlag = false, unsigned jmax = 50, int nmax = 300, double eps = 1e-10, bool printa = false) {
        typedef SEXP(*Ptr_dWeibullCount_acc_vec)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_dWeibullCount_acc_vec p_dWeibullCount_acc_vec = NULL;
        if (p_dWeibullCount_acc_vec == NULL) {
            validateSignature("arma::vec(*dWeibullCount_acc_vec)(arma::Col<unsigned>,arma::vec,arma::vec,double,bool,unsigned,int,double,bool)");
            p_dWeibullCount_acc_vec = (Ptr_dWeibullCount_acc_vec)R_GetCCallable("Countr", "Countr_dWeibullCount_acc_vec");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_dWeibullCount_acc_vec(Rcpp::wrap(x), Rcpp::wrap(shape), Rcpp::wrap(scale), Rcpp::wrap(time), Rcpp::wrap(logFlag), Rcpp::wrap(jmax), Rcpp::wrap(nmax), Rcpp::wrap(eps), Rcpp::wrap(printa));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<arma::vec >(__result);
    }

}

#endif // __Countr_RcppExports_h__
