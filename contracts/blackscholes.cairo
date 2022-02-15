%lang starknet
%builtins pedersen range_check

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import assert_not_zero
from starkware.cairo.common.uint256 import (
    Uint256, uint256_add, uint256_sub, uint256_eq, uint256_le, uint256_check, uint256_mul,
    uint256_signed_nn, uint256_neg)
from starkware.starknet.common.syscalls import get_caller_address, get_contract_address

# define constants

const SECONDS_PER_YEAR = 31536000
# internally this library uses 27 decimals of precision
const PRECISE_UNIT = 10 ** 27
const LN_2_PRECISE = 693147180559945309417232122
const SQRT_TWOPI = 2506628274631000502415765285
# below this value, return 0
const MIN_CDF_STD_DIST_INPUT = (int(PRECISE_UNIT) * (-45)) / 10  # -4.5
# above this value, return 1
const MAX_CDF_STD_DIST_INPUT = int(PRECISE_UNIT) * 10
# below this value, the result is always 0
const MIN_EXP = (-63) * int(PRECISE_UNIT)
# above this value the a lot of precision is lost, and uint256s come close to not being able to handle the size
const MAX_EXP = 100 * PRECISE_UNIT
# value to use to avoid any division by 0 or values near 0
const MIN_T_ANNUALISED = PRECISE_UNIT / SECONDS_PER_YEAR  # 1 second
const MIN_VOLATILITY = PRECISE_UNIT / 10000  # 0.001%
const VEGA_STANDARDISATION_MIN_DAYS = 7 * 86400

# util functions
@external
func abs{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(x : Uint256) -> (res : Uint256):
    let (is_x_nonnegative : felt) = uint256_le(Uint256(0, 0), x)
    if is_x_nonnegative == 1:
        return (res=x)
    else:
        let (neg_x : Uint256) = uint256_neg(x)
        return (res=neg_x)
    end
end

func floor{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(x : Uint256) -> (
        res : Uint256):
    # to be implemented
    return (res=x)
end

func ln{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(x : Uint256) -> (
        res : Uint256):
    # to be implemented
    return (res=x)
end

@external
func exp{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(x : Uint256) -> (
        res : Uint256):
    # to be implemented
    return (res=x)
end

func sqrt_precise{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(x : Uint256) -> (
        res : Uint256):
    # to be implemented
    return (res=x)
end

@external
func sqrt{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(x : Uint256) -> (
        res : Uint256):
    # to be implemented
    return (res=x)
end


# internal coefficients of Black-Scholes
@external
func d1d2{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256, rate : Uint256) -> (
        d1 : Uint256, d2 : Uint256):
    # to be implemented
    return (d1=t_annualised, d2=t_annualised)
end


# option greeks

# sensitivity of option price to a $1 change in spot price
@external
func delta{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256, rate : Uint256) -> (
        call_delta : Uint256, put_delta : Uint256):
    # to be implemented
    return (call_delta=t_annualised, put_delta=t_annualised)
end

# sensitivity of option delta to a $1 change in spot price
@external
func gamma{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256, rate : Uint256) -> (
        gamma : Uint256):
    # to be implemented
    return (gamma=t_annualised)
end

# sensitivity of option price to a 1% change in IV
@external
func vega{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256, rate : Uint256) -> (
        vega : Uint256):
    # to be implemented
    return (vega=t_annualised)
end

# sensitivity of option price to a 1% change in the rate
@external
func rho{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256, rate : Uint256) -> (
        call_rho : Uint256, put_rho : Uint256):
    # to be implemented
    return (call_rho=t_annualised, put_rho=t_annualised)
end

# how much value the option loses per day
@external
func theta{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256, rate : Uint256) -> (
        call_theta : Uint256, put_theta : Uint256):
    # to be implemented
    return (call_theta=t_annualised, put_theta=t_annualised)
end

# value of the call/put option
@external
func option_prices{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(t_annualised : Uint256, volatility : Uint256, spot : Uint256, strike : Uint256, rate : Uint256) -> (
        call_price : Uint256, put_price : Uint256):
    # to be implemented
    return (call_price=t_annualised, put_price=t_annualised)
end
