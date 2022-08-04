FROM haskell:9.2.4 as builder
RUN git clone https://github.com/gerardVM/brick-breaker-multi-ball.git
WORKDIR /brick-breaker-multi-ball
RUN cabal update
RUN cabal build

FROM ubuntu:18.04
COPY --from=builder /brick-breaker-multi-ball/dist-newstyle/build/x86_64-linux/ghc-9.2.4/finale-0.1.0.0/x/animation/build/animation/animation .
ENTRYPOINT ["./animation"]
