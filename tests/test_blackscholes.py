"""blackscholes.cairo test file."""
import os
from typing import Type, Tuple

import pytest
from starkware.starknet.definitions.error_codes import StarknetErrorCode
from starkware.starknet.testing.contract import StarknetContract
from starkware.starknet.testing.starknet import Starknet
from starkware.starkware_utils.error_handling import StarkException

# The path to the contract source code.

CONTRACT_FILE = os.path.join("contracts", "blackscholes.cairo")


# Util functions

def to_split_uint(a: int) -> Tuple:
    return (a & ((1 << 128) - 1), a >> 128)

def to_uint(a: Tuple) -> int:
    return a[0] + (a[1] << 128)

async def assert_revert(expression, expected_err_msg=None):
    try:
        await expression
        assert False
    except StarkException as err:
        if expected_err_msg:
            assert expected_err_msg in err.message, "Could not find error message:\nexpected: {}\nactual: {}\n".format(expected_err_msg, err.message)
        _, error = err.args
        assert error['code'] == StarknetErrorCode.TRANSACTION_FAILED


# Constants

CALLER = 1234
ZERO = to_split_uint(0)
TWO = to_split_uint(2)
FIVE = to_split_uint(5)


# Fixtures

@pytest.fixture
async def starknet() -> Type[Starknet]:
    return await Starknet.empty()

@pytest.fixture
async def contract(starknet: Type[Starknet]) -> Type[StarknetContract]:
    return await starknet.deploy(source=CONTRACT_FILE)

@pytest.mark.asyncio
async def test_set_value_conditional(contract: Type[StarknetContract]):
    """Test set_value_conditional method."""
    await contract.set_value_conditional(lhs=TWO, rhs=FIVE, res_if_true=ZERO, res_if_false=TWO).call()
