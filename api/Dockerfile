FROM haskell:9.2.1-buster

WORKDIR /app
COPY api.cabal /app/api.cabal
COPY stack.yaml /app/stack.yaml
COPY stack.yaml.lock /app/stack.yaml.lock
RUN stack setup

COPY / /app
RUN stack build

CMD ["stack", "exec", "api-exe"]
