# api

## Getting Started

```shell
# Setup GHCup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
cd api/
stack setup

# Build the api
stack build

# Run the api
stack exec api-exe

# Format the code
ormolu --mode inplace $(git ls-files '*.hs')
```
