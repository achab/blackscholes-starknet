%lang starknet
%builtins pedersen range_check bitwise

from starkware.cairo.common.cairo_builtins import BitwiseBuiltin
from starkware.cairo.common.math import assert_not_zero, sqrt, signed_div_rem, split_felt
from starkware.cairo.common.uint256 import (
    Uint256, uint256_eq, uint256_le, uint256_signed_nn, uint256_cond_neg, uint256_lt,
    uint256_unsigned_div_rem, uint256_neg)
from starkware.starknet.common.syscalls import get_caller_address, get_contract_address

from contracts.safe_math import (
    div_signed256, mul_signed256, mul256, div256, sub256, add256, add_signed256, sub_signed256)

# util functions

# this function is set to external for testing purpose
@external
func set_value_conditional{range_check_ptr}(
        lhs : Uint256, rhs : Uint256, res_if_true : Uint256, res_if_false : Uint256) -> (
        res : Uint256):
    alloc_locals
    let (is_lhs_below_rhs : felt) = uint256_le(lhs, rhs)
    if is_lhs_below_rhs == 1:
        return (res=res_if_true)
    else:
        return (res=res_if_false)
    end
end

func felt_to_uint256{range_check_ptr}(x : felt) -> (res : Uint256):
    let split = split_felt(x)
    return (res=Uint256(low=split.low, high=split.high))
end

func uint256_to_felt{range_check_ptr}(x : Uint256) -> (res : felt):
    return (res=x.high * (2 ** 128) + x.low)
end

# define constants

const SECONDS_PER_WEEK_FELT = 7 * 86400
const SECONDS_PER_YEAR_FELT = 31536000
# constant numbers
const ZERO_FELT = 0
const TWO_FELT = 2
const TEN_MILLIONS_FELT = 10 ** 7
const TEN_TO_FOURTEEN_FELT = 10 ** 14
# internally this library uses 27 decimals of precision
const BOUND_FELT = (2 ** 128) / 2
const PRECISE_UNIT_FELT = 10 ** 27
const SQRT_TWOPI_FELT = 2506628274631000502415765285
# below this value, return 0
const MIN_CDF_STD_DIST_INPUT_FELT = ((-45) * PRECISE_UNIT_FELT) / 10
# above this value, return 1
const MAX_CDF_STD_DIST_INPUT_FELT = 10 * PRECISE_UNIT_FELT
# value to use to avoid any division by 0 or values near 0
const MIN_T_ANNUALISED_FELT = PRECISE_UNIT_FELT / SECONDS_PER_YEAR_FELT  # 1 second
const MIN_VOLATILITY_FELT = PRECISE_UNIT_FELT / 10000

# math functions

@external
func abs_value{range_check_ptr}(x : Uint256) -> (res : Uint256):
    alloc_locals
    let (is_x_nn : felt) = uint256_signed_nn(x)
    let (x_abs : Uint256) = uint256_cond_neg(x, 1 - is_x_nn)
    return (res=x_abs)
end

@external
func exp{range_check_ptr}(x_ : Uint256) -> (res : Uint256):
    alloc_locals

    let (x : felt) = uint256_to_felt(x_)

    if x == 0:
        let (one : Uint256) = felt_to_uint256(1)
        return (res=one)
    end

    let (t2, _) = signed_div_rem(x * x, 2 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t3, _) = signed_div_rem(t2 * x, 3 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t4, _) = signed_div_rem(t3 * x, 4 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t5, _) = signed_div_rem(t4 * x, 5 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t6, _) = signed_div_rem(t5 * x, 6 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t7, _) = signed_div_rem(t6 * x, 7 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t8, _) = signed_div_rem(t7 * x, 8 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t9, _) = signed_div_rem(t8 * x, 9 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t10, _) = signed_div_rem(t9 * x, 10 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t11, _) = signed_div_rem(t10 * x, 11 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t12, _) = signed_div_rem(t11 * x, 12 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t13, _) = signed_div_rem(t12 * x, 13 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t14, _) = signed_div_rem(t13 * x, 14 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t15, _) = signed_div_rem(t14 * x, 15 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t16, _) = signed_div_rem(t15 * x, 16 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t17, _) = signed_div_rem(t16 * x, 17 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t18, _) = signed_div_rem(t17 * x, 18 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t19, _) = signed_div_rem(t18 * x, 19 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t20, _) = signed_div_rem(t19 * x, 20 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t21, _) = signed_div_rem(t20 * x, 21 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t22, _) = signed_div_rem(t21 * x, 22 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t23, _) = signed_div_rem(t22 * x, 23 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t24, _) = signed_div_rem(t23 * x, 24 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t25, _) = signed_div_rem(t24 * x, 25 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t26, _) = signed_div_rem(t25 * x, 26 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t27, _) = signed_div_rem(t26 * x, 27 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t28, _) = signed_div_rem(t27 * x, 28 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t29, _) = signed_div_rem(t28 * x, 29 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t30, _) = signed_div_rem(t29 * x, 30 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t31, _) = signed_div_rem(t30 * x, 31 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t32, _) = signed_div_rem(t31 * x, 32 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t33, _) = signed_div_rem(t32 * x, 33 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t34, _) = signed_div_rem(t33 * x, 34 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t35, _) = signed_div_rem(t34 * x, 35 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t36, _) = signed_div_rem(t35 * x, 36 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t37, _) = signed_div_rem(t36 * x, 37 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t38, _) = signed_div_rem(t37 * x, 38 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t39, _) = signed_div_rem(t38 * x, 39 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t40, _) = signed_div_rem(t39 * x, 40 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t41, _) = signed_div_rem(t40 * x, 41 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t42, _) = signed_div_rem(t41 * x, 42 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t43, _) = signed_div_rem(t42 * x, 43 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t44, _) = signed_div_rem(t43 * x, 44 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t45, _) = signed_div_rem(t44 * x, 45 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t46, _) = signed_div_rem(t45 * x, 46 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t47, _) = signed_div_rem(t46 * x, 47 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t48, _) = signed_div_rem(t47 * x, 48 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t49, _) = signed_div_rem(t48 * x, 49 * PRECISE_UNIT_FELT, BOUND_FELT)
    let (t50, _) = signed_div_rem(t49 * x, 50 * PRECISE_UNIT_FELT, BOUND_FELT)

    let sum = (PRECISE_UNIT_FELT + x + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 +
        t12 + t13 + t14 + t15 + t16 + t17 + t18 + t19 + t20 + t21 + t22 +
        t23 + t24 + t25 + t26 + t27 + t28 + t29 + t30 + t31 + t32 + t33 +
        t34 + t35 + t36 + t37 + t38 + t39 + t40 + t41 + t42 + t43 + t44 +
        t45 + t46 + t47 + t48 + t49 + t50)

    let (res : Uint256) = felt_to_uint256(sum)

    return (res=res)
end

func _update_rule{bitwise_ptr : BitwiseBuiltin*, range_check_ptr}(x : Uint256, y : Uint256) -> (
        res : Uint256, is_equal : felt):
    alloc_locals
    let (two : Uint256) = felt_to_uint256(TWO_FELT)

    let (exp_y : Uint256) = exp(y)
    let (x_plus_exp_y : Uint256) = add256(x, exp_y)
    let (x_minus_exp_y : Uint256) = sub256(x, exp_y)
    let (x_plus_exp_y_times_2 : Uint256) = mul256(x_minus_exp_y, two)
    let (num : Uint256) = add256(y, x_plus_exp_y_times_2)
    let (res : Uint256) = div256(num, x_plus_exp_y)

    let (is_equal : felt) = uint256_eq(res, y)

    return (res=res, is_equal=is_equal)
end

func ln{bitwise_ptr : BitwiseBuiltin*, range_check_ptr}(x : Uint256) -> (res : Uint256):
    alloc_locals

    let (zero : Uint256) = felt_to_uint256(ZERO_FELT)

    let (res : Uint256, is_equal) = _update_rule(x, zero)
    if is_equal == 1:
        return (res=res)
    end

    let (res : Uint256, is_equal) = _update_rule(x, res)
    if is_equal == 1:
        return (res=res)
    end

    let (res : Uint256, is_equal) = _update_rule(x, res)
    if is_equal == 1:
        return (res=res)
    end

    let (res : Uint256, is_equal) = _update_rule(x, res)
    if is_equal == 1:
        return (res=res)
    end

    let (res : Uint256, is_equal) = _update_rule(x, res)
    if is_equal == 1:
        return (res=res)
    end

    let (res : Uint256, is_equal) = _update_rule(x, res)
    if is_equal == 1:
        return (res=res)
    end

    let (res : Uint256, is_equal) = _update_rule(x, res)
    if is_equal == 1:
        return (res=res)
    end

    let (res : Uint256, is_equal) = _update_rule(x, res)
    if is_equal == 1:
        return (res=res)
    end

    return (res=res)
end

func sqrt_precise{range_check_ptr}(x : Uint256) -> (res : Uint256):
    alloc_locals
    let (PRECISE_UNIT : Uint256) = felt_to_uint256(PRECISE_UNIT_FELT)
    let (value_times_precision : Uint256) = mul256(x, PRECISE_UNIT)
    let (value_times_precision_felt : felt) = uint256_to_felt(value_times_precision)
    let (res_sqrt : felt) = sqrt(value_times_precision_felt)
    let (res : Uint256) = felt_to_uint256(res_sqrt)
    return (res=res)
end

func std_normal{range_check_ptr}(x : Uint256) -> (res : Uint256):
    alloc_locals

    let (SQRT_TWOPI : Uint256) = felt_to_uint256(SQRT_TWOPI_FELT)
    let (two : Uint256) = felt_to_uint256(TWO_FELT)

    let (x_squared : Uint256) = mul256(x, x)
    let (x_squared_over_2 : Uint256) = div256(x_squared, two)
    let (exp_x_squared_over_2 : Uint256) = exp(x_squared_over_2)
    let (res : Uint256) = div256(exp_x_squared_over_2, SQRT_TWOPI)
    return (res=res)
end

func std_normal_cdf{bitwise_ptr : BitwiseBuiltin*, range_check_ptr}(x : Uint256) -> (res : Uint256):
    alloc_locals

    let (PRECISE_UNIT : Uint256) = felt_to_uint256(PRECISE_UNIT_FELT)
    let (MIN_CDF_STD_DIST_INPUT : Uint256) = felt_to_uint256(MIN_CDF_STD_DIST_INPUT_FELT)
    let (MAX_CDF_STD_DIST_INPUT : Uint256) = felt_to_uint256(MAX_CDF_STD_DIST_INPUT_FELT)
    let (zero : Uint256) = felt_to_uint256(ZERO_FELT)
    let (two : Uint256) = felt_to_uint256(TWO_FELT)
    let (ten_millions : Uint256) = felt_to_uint256(TEN_MILLIONS_FELT)
    let (ten_to_fourteen : Uint256) = felt_to_uint256(TEN_TO_FOURTEEN_FELT)

    let (below_min) = uint256_lt(x, MIN_CDF_STD_DIST_INPUT)
    if below_min == 1:
        return (res=zero)
    end
    let (above_max) = uint256_lt(MAX_CDF_STD_DIST_INPUT, x)
    if above_max == 1:
        return (res=PRECISE_UNIT)
    end

    let (x_abs : Uint256) = abs_value(x)
    let (const_t1 : Uint256) = felt_to_uint256(2315419)
    let (product_t1 : Uint256) = mul256(x_abs, const_t1)
    let (ratio_t1 : Uint256) = div256(product_t1, PRECISE_UNIT)
    let (t1 : Uint256) = add256(ten_millions, ratio_t1)

    let (x_over_2 : Uint256) = div256(x, two)
    let (exponent : Uint256) = mul256(x, x_over_2)

    let (const_d) = felt_to_uint256(3989423)
    let (product_d) = mul256(const_d, PRECISE_UNIT)
    let (exp_exponent : Uint256) = exp(exponent)
    let (d : Uint256) = div256(product_d, exp_exponent)

    let (const_a : Uint256) = felt_to_uint256(13302740)
    let (prod_a : Uint256) = mul256(const_a, ten_millions)
    let (term_a : Uint256) = div256(prod_a, t1)

    let (const_b : Uint256) = felt_to_uint256(-18212560)
    let (sum_b : Uint256) = add_signed256(const_b, term_a)
    let (prod_b : Uint256) = mul_signed256(sum_b, ten_millions)
    let (term_b : Uint256) = div_signed256(prod_b, t1)

    let (const_c : Uint256) = felt_to_uint256(17814780)
    let (sum_c : Uint256) = add_signed256(const_c, term_b)
    let (prod_c : Uint256) = mul_signed256(sum_c, ten_millions)
    let (term_c : Uint256) = div_signed256(prod_c, t1)

    let (const_d : Uint256) = felt_to_uint256(-3565638)
    let (sum_d : Uint256) = add_signed256(const_d, term_c)
    let (prod_d : Uint256) = mul_signed256(sum_d, ten_millions)
    let (term_d : Uint256) = div_signed256(prod_d, t1)

    let (const_e : Uint256) = felt_to_uint256(3193815)
    let (sum_e : Uint256) = add_signed256(const_e, term_d)
    let (prod_e : Uint256) = mul_signed256(sum_e, ten_millions)
    let (term_e : Uint256) = div_signed256(prod_e, t1)

    let (is_x_positive) = uint256_lt(zero, x)
    if is_x_positive == 1:
        let (diff : Uint256) = sub_signed256(ten_to_fourteen, term_e)
        let (prod : Uint256) = mul_signed256(PRECISE_UNIT, diff)
        let (res : Uint256) = div_signed256(prod, ten_to_fourteen)
        return (res=res)
    else:
        let (prod : Uint256) = mul_signed256(PRECISE_UNIT, term_e)
        let (res : Uint256) = div_signed256(prod, ten_to_fourteen)
        return (res=res)
    end
end

func annualise{range_check_ptr}(x : Uint256) -> (res : Uint256):
    let (SECONDS_PER_YEAR : Uint256) = felt_to_uint256(SECONDS_PER_YEAR_FELT)
    let (res : Uint256) = div256(x, SECONDS_PER_YEAR)
    return (res=res)
end

# internal coefficients of Black-Scholes
@external
func d1d2{bitwise_ptr : BitwiseBuiltin*, range_check_ptr}(
        t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256,
        rate : Uint256) -> (d1 : Uint256, d2 : Uint256):
    alloc_locals

    let (MIN_T_ANNUALISED : Uint256) = felt_to_uint256(MIN_T_ANNUALISED_FELT)
    let (MIN_VOLATILITY : Uint256) = felt_to_uint256(MIN_VOLATILITY_FELT)
    let (two : Uint256) = felt_to_uint256(TWO_FELT)

    let (t_annualised : Uint256) = set_value_conditional(
        t_annualised, MIN_T_ANNUALISED, MIN_T_ANNUALISED, t_annualised)
    let (volatility : Uint256) = set_value_conditional(
        volatility, MIN_VOLATILITY, MIN_VOLATILITY, volatility)

    let (t_annualised_sqrt : Uint256) = sqrt_precise(t_annualised)
    let (v_t_sqrt : Uint256) = mul256(volatility, t_annualised_sqrt)

    let (spot_over_strike : Uint256) = div256(spot, strike)
    let (log : Uint256) = ln(spot_over_strike)

    let (v_squared : Uint256) = mul256(volatility, volatility)
    let (v_squared_over_2 : Uint256) = div256(v_squared, two)
    let (v_squared_over_2_plus_rate : Uint256) = add256(v_squared_over_2, rate)
    let (v_2_t : Uint256) = mul256(v_squared_over_2_plus_rate, t_annualised)

    let (log_plus_v_2_t : Uint256) = add256(log, v_2_t)
    let (d1 : Uint256) = div256(log_plus_v_2_t, v_t_sqrt)

    let (d2 : Uint256) = sub256(d1, v_t_sqrt)

    return (d1=d1, d2=d2)
end

# option greeks

# sensitivity of option price to a $1 change in spot price
@external
func delta{bitwise_ptr : BitwiseBuiltin*, range_check_ptr}(
        t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256,
        rate : Uint256) -> (call_delta : Uint256, put_delta : Uint256):
    alloc_locals

    let (PRECISE_UNIT : Uint256) = felt_to_uint256(PRECISE_UNIT_FELT)

    let (d1 : Uint256, _) = d1d2(t_annualised, volatility, spot, strike, rate)
    let (call_delta : Uint256) = std_normal_cdf(d1)

    let (put_delta : Uint256) = sub_signed256(call_delta, PRECISE_UNIT)

    return (call_delta=call_delta, put_delta=put_delta)
end

# sensitivity of option delta to a $1 change in spot price
@external
func gamma{bitwise_ptr : BitwiseBuiltin*, range_check_ptr}(
        t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256,
        rate : Uint256) -> (gamma : Uint256):
    alloc_locals

    let (d1 : Uint256, _) = d1d2(t_annualised, volatility, spot, strike, rate)
    let (std_normal_d1 : Uint256) = std_normal(d1)

    let (t_annualised_sqrt : Uint256) = sqrt_precise(t_annualised)
    let (t_annualised_sqrt_times_spot : Uint256) = mul256(t_annualised_sqrt, spot)
    let (t_annualised_sqrt_times_spot_times_vol : Uint256) = mul256(
        t_annualised_sqrt_times_spot, volatility)

    let (gamma : Uint256) = div256(std_normal_d1, t_annualised_sqrt_times_spot_times_vol)

    return (gamma=gamma)
end

# sensitivity of option price to a 1% change in IV
@external
func vega{bitwise_ptr : BitwiseBuiltin*, range_check_ptr}(
        t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256,
        rate : Uint256) -> (vega : Uint256):
    alloc_locals

    let (d1 : Uint256, _) = d1d2(t_annualised, volatility, spot, strike, rate)
    let (std_normal_d1 : Uint256) = std_normal(d1)
    let (std_normal_d1_times_spot : Uint256) = mul256(std_normal_d1, spot)

    let (t_annualised_sqrt : Uint256) = sqrt_precise(t_annualised)

    let (vega : Uint256) = mul256(t_annualised_sqrt, std_normal_d1_times_spot)

    return (vega=vega)
end

# sensitivity of option price to a 1% change in the rate
@external
func rho{bitwise_ptr : BitwiseBuiltin*, range_check_ptr}(
        t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256,
        rate : Uint256) -> (call_rho : Uint256, put_rho : Uint256):
    alloc_locals

    let (PRECISE_UNIT : Uint256) = felt_to_uint256(PRECISE_UNIT_FELT)

    let (_, d2 : Uint256) = d1d2(t_annualised, volatility, spot, strike, rate)
    let (std_normal_cdf_d2 : Uint256) = std_normal_cdf(d2)
    let (std_normal_cdf_d2_minus_unit) = sub_signed256(std_normal_cdf_d2, PRECISE_UNIT)

    let (rate_times_t : Uint256) = mul256(rate, t_annualised)
    let (neg_rate_times_t : Uint256) = uint256_neg(rate_times_t)
    let (exp_neg_rate_times_t : Uint256) = exp(neg_rate_times_t)
    let (strike_times_t : Uint256) = mul256(strike, t_annualised)
    let (factor : Uint256) = mul256(exp_neg_rate_times_t, strike_times_t)

    let (call_rho : Uint256) = mul256(factor, std_normal_cdf_d2)
    let (put_rho : Uint256) = mul_signed256(factor, std_normal_cdf_d2_minus_unit)

    return (call_rho=call_rho, put_rho=put_rho)
end

# how much value the option loses per day
@external
func theta{bitwise_ptr : BitwiseBuiltin*, range_check_ptr}(
        t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256,
        rate : Uint256) -> (call_theta : Uint256, put_theta : Uint256):
    alloc_locals

    let (PRECISE_UNIT : Uint256) = felt_to_uint256(PRECISE_UNIT_FELT)
    let (two : Uint256) = felt_to_uint256(TWO_FELT)

    let (d1 : Uint256, _) = d1d2(t_annualised, volatility, spot, strike, rate)
    let (std_normal_d1 : Uint256) = std_normal(d1)

    let (t_annualised_sqrt : Uint256) = sqrt_precise(t_annualised)
    let (denom : Uint256) = mul256(t_annualised_sqrt, two)

    let (volatility_times_spot : Uint256) = mul256(volatility, spot)
    let (num : Uint256) = mul256(std_normal_d1, volatility_times_spot)

    let (fraction : Uint256) = div256(num, denom)
    let (neg_fraction : Uint256) = uint256_neg(fraction)

    let (_, d2 : Uint256) = d1d2(t_annualised, volatility, spot, strike, rate)
    let (std_normal_cdf_d2 : Uint256) = std_normal_cdf(d2)
    let (std_normal_cdf_d2_minus_unit) = sub_signed256(std_normal_cdf_d2, PRECISE_UNIT)

    let (rate_times_t : Uint256) = mul256(rate, t_annualised)
    let (neg_rate_times_t : Uint256) = uint256_neg(rate_times_t)
    let (exp_neg_rate_times_t : Uint256) = exp(neg_rate_times_t)
    let (strike_times_rate : Uint256) = mul256(strike, rate)
    let (factor : Uint256) = mul256(exp_neg_rate_times_t, strike_times_rate)

    let (call_rhs : Uint256) = mul256(factor, std_normal_cdf_d2)
    let (put_rhs : Uint256) = mul_signed256(factor, std_normal_cdf_d2_minus_unit)

    let (call_theta : Uint256) = sub_signed256(neg_fraction, call_rhs)
    let (put_theta : Uint256) = sub_signed256(neg_fraction, put_rhs)

    return (call_theta=call_theta, put_theta=put_theta)
end

# value of the call/put option
@external
func option_prices{bitwise_ptr : BitwiseBuiltin*, range_check_ptr}(
        t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256,
        rate : Uint256) -> (call_price : Uint256, put_price : Uint256):
    alloc_locals

    let (zero : Uint256) = felt_to_uint256(ZERO_FELT)

    let (d1 : Uint256, d2 : Uint256) = d1d2(t_annualised, volatility, spot, strike, rate)

    let (rate_times_t : Uint256) = mul_signed256(rate, t_annualised)
    let (neg_rate_times_t : Uint256) = uint256_neg(rate_times_t)
    let (exp_neg_rate_times_t : Uint256) = exp(neg_rate_times_t)
    let (strike_pv : Uint256) = mul256(strike, exp_neg_rate_times_t)

    let (std_normal_cdf_d1 : Uint256) = std_normal_cdf(d1)
    let (spot_nd1 : Uint256) = mul256(spot, std_normal_cdf_d1)

    let (std_normal_cdf_d2 : Uint256) = std_normal_cdf(d2)
    let (strike_nd2 : Uint256) = mul256(strike_pv, std_normal_cdf_d2)

    let (spot_nd1_minus_strike_nd2) = sub_signed256(spot_nd1, strike_nd2)

    let (call_price : Uint256) = set_value_conditional(
        strike_nd2, spot_nd1, spot_nd1_minus_strike_nd2, zero)

    let (call_plus_strike_pv : Uint256) = add256(call_price, strike_pv)
    let (call_plus_strike_pv_minus_spot : Uint256) = sub_signed256(call_plus_strike_pv, spot)
    let (put_price : Uint256) = set_value_conditional(
        spot, call_plus_strike_pv, call_plus_strike_pv_minus_spot, zero)

    return (call_price=call_price, put_price=put_price)
end
