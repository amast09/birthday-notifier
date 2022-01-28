# api

## Getting Started

```shell
# Setup Stack
curl -sSL https://get.haskellstack.org/ | sh
cd api/
stack setup

# Build the api
stack build

# Run the api
stack exec api-exe

# Format the code
ormolu --mode inplace $(git ls-files '*.hs')
```
