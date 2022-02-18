from starkware.cairo.common.bitwise import bitwise_and
from starkware.cairo.common.cairo_builtins import BitwiseBuiltin
from starkware.cairo.common.uint256 import (
    Uint256, uint256_mul, uint256_unsigned_div_rem, uint256_signed_div_rem, uint256_add,
    uint256_signed_nn, uint256_cond_neg, uint256_le, uint256_not)

const MASK128 = 2 ** 128 - 1
const BOUND128 = 2 ** 128

func add256{range_check_ptr}(lhs : Uint256, rhs : Uint256) -> (res : Uint256):
    let (res : Uint256, carry : felt) = uint256_add(lhs, rhs)
    assert carry = 0
    return (res)
end

func add_signed256{range_check_ptr, bitwise_ptr : BitwiseBuiltin*}(
        lhs : Uint256, rhs : Uint256) -> (res : Uint256):
    let (res : Uint256, carry : felt) = uint256_add(lhs, rhs)
    let (msb) = bitwise_and(res.high, 0x80000000000000000000000000000000)
    assert (msb + carry) * (0x80000000000000000000000000000000 - msb + 1 - carry) = 0
    return (res)
end

func sub256{range_check_ptr, bitwise_ptr : BitwiseBuiltin*}(lhs : Uint256, rhs : Uint256) -> (
        res : Uint256):
    let (safe) = uint256_le(rhs, lhs)
    assert safe = 1
    # preemptively borrow from bit128
    let (low_safe) = bitwise_and(BOUND128 + lhs.low - rhs.low, MASK128)
    let low_unsafe = lhs.low - rhs.low
    if low_safe == low_unsafe:
        # the borrow was not used
        return (Uint256(low_safe, lhs.high - rhs.high))
    else:
        # the borrow was used
        return (Uint256(low_safe, lhs.high - rhs.high - 1))
    end
end

func sub_signed256{range_check_ptr, bitwise_ptr : BitwiseBuiltin*}(
        lhs : Uint256, rhs : Uint256) -> (res : Uint256):
    # First sign extend both operands
    let (left_msb : felt) = bitwise_and(lhs.high, 0x80000000000000000000000000000000)
    let (right_msb : felt) = bitwise_and(rhs.high, 0x80000000000000000000000000000000)
    let left_overflow : felt = left_msb / 0x80000000000000000000000000000000
    let right_overflow : felt = right_msb / 0x80000000000000000000000000000000

    # Now safely negate the rhs and add (l - r = l + (-r))
    let (right_flipped : Uint256) = uint256_not(rhs)
    let (right_neg, overflow) = uint256_add(right_flipped, Uint256(1, 0))
    let right_overflow_neg = overflow + 1 - right_overflow
    let (res, res_base_overflow) = uint256_add(lhs, right_neg)
    let res_overflow = res_base_overflow + left_overflow + right_overflow_neg

    # Check if the result fits in the correct width
    let (res_msb : felt) = bitwise_and(res.high, 0x80000000000000000000000000000000)
    let (res_overflow_lsb : felt) = bitwise_and(res_overflow, 1)
    assert res_overflow_lsb * 0x80000000000000000000000000000000 = res_msb

    # Narrow and return
    return (res)
end

func mul256{range_check_ptr}(lhs : Uint256, rhs : Uint256) -> (res : Uint256):
    let (result : Uint256, overflow : Uint256) = uint256_mul(lhs, rhs)
    assert overflow.low = 0
    assert overflow.high = 0
    return (result)
end

func mul_signed256{range_check_ptr, bitwise_ptr : BitwiseBuiltin*}(
        lhs : Uint256, rhs : Uint256) -> (result : Uint256):
    alloc_locals
    let (lhs_nn) = uint256_signed_nn(lhs)
    let (local rhs_nn) = uint256_signed_nn(rhs)
    let (lhs_abs) = uint256_cond_neg(lhs, 1 - lhs_nn)
    let (rhs_abs) = uint256_cond_neg(rhs, 1 - rhs_nn)
    let (res_abs, overflow) = uint256_mul(lhs_abs, rhs_abs)
    assert overflow.low = 0
    assert overflow.high = 0
    let (msb) = bitwise_and(res_abs.high, 0x80000000000000000000000000000000)
    assert msb = 0
    let (res) = uint256_cond_neg(res_abs, (lhs_nn + rhs_nn) * (2 - lhs_nn - rhs_nn))
    return (res)
end

func div256{range_check_ptr}(lhs : Uint256, rhs : Uint256) -> (res : Uint256):
    let (res : Uint256, _) = uint256_unsigned_div_rem(lhs, rhs)
    return (res)
end

func div_signed256{range_check_ptr}(lhs : Uint256, rhs : Uint256) -> (res : Uint256):
    let (res : Uint256, _) = uint256_signed_div_rem(lhs, rhs)
    return (res)
end
