FROM haskell:9.2.1-buster

RUN apt-get update && apt-get install -y libpq-dev

WORKDIR /app
COPY api.cabal /app/api.cabal
COPY stack.yaml /app/stack.yaml
COPY stack.yaml.lock /app/stack.yaml.lock
RUN stack setup
RUN stack build --only-dependencies

COPY / /app
RUN stack build

CMD sh -c "sleep 5 && stack exec api-exe"
