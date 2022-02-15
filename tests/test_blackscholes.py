"""blackscholes.cairo test file."""
import os

import pytest
from starkware.starknet.definitions.error_codes import StarknetErrorCode
from starkware.starknet.testing.contract import StarknetContract
from starkware.starknet.testing.starknet import Starknet
from starkware.starkware_utils.error_handling import StarkException

# The path to the contract source code.

CONTRACT_FILE = os.path.join("contracts", "blackscholes.cairo")


# Util functions

def to_split_uint(a: int) -> tuple:
    return (a & ((1 << 128) - 1), a >> 128)

def to_uint(a: tuple) -> int:
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


# The testing library uses python's asyncio. So the following
# decorator and the ``async`` keyword are needed.
@pytest.mark.asyncio
async def test_increase_balance():
    """Test increase_balance method."""
    # Create a new Starknet class that simulates the StarkNet
    # system.
    starknet = await Starknet.empty()

    # Deploy the contract.
    contract = await starknet.deploy(
        source=CONTRACT_FILE,
    )

    # Invoke increase_balance() twice.
    await contract.increase_balance(amount=10).invoke()
    await contract.increase_balance(amount=20).invoke()

    # Check the result of get_balance().
    execution_info = await contract.get_balance().call()
    assert execution_info.result == (30,)
