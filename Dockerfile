FROM haskell:9.2.4 as builder
WORKDIR /usr/src
COPY app ./app
COPY lib ./lib
COPY finale.cabal .
RUN cabal update && cabal build

FROM ubuntu:18.04
COPY --from=builder /usr/src/dist-newstyle/build/x86_64-linux/ghc-9.2.4/finale-0.1.0.0/x/animation/build/animation/animation .
ENTRYPOINT ["./animation"]
