# Black Scholes contract written in Cairo

Project built with [nile](https://github.com/OpenZeppelin/nile)

## Remark

- Methods in the file `contracts/safe_math.cairo` are taken from [Cairo-SafeMath](https://github.com/NethermindEth/Cairo-SafeMath) repo from Nethermind.

## Useful commands

- **Compile contracts**

  ```
  nile compile
  ```

- **Start starknet-dev**

  ```
  nile node
  ```

- **Launch the environment**

  ```
  source env/bin/activate
  ```

- **Run the test**

  ```
  pytest tests/test_blackscholes.py
  ```
