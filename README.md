# Brick Breaker Videogame (CLI)

Brick Breaker Multi-Ball is the Multi-Ball fork of my own Brick Breaker videogame.

![example-gif](example.gif)

![GitHub last commit](https://img.shields.io/github/last-commit/gerardVM/brick-breaker-multi-ball)

## Installation

Clone this repository and run cabal

```bash
git clone https://github.com/gerardVM/brick-breaker-multi-ball.git &&
cd brick-breaker-multi-ball &&
cabal run
```

## Trying it with Docker

Option 1: By building the Docker image from the Dockerfile

```bash
git clone https://github.com/gerardVM/brick-breaker-multi-ball.git &&
cd brick-breaker-multi-ball &&
docker build --no-cache -t haskell-app . &&
docker run --rm -it haskell-app
```
Option 2: Run the pre-built Docker image

```bash
docker run --rm -it gerardvm/brick-breaker-multi-ball
```

## Usage

Player's objective is to break all the bricks by maintaining at least one ball above the floor

- A - Left (Same key for Move and Stop) 
- D - Right (Same key for Move and Stop) 
- P - Pause
- SPACE - Auto Mode ON (Pause and unpause game to set Auto Mode OFF)

## Contributing

Pull requests are welcome

## License

[MIT](LICENSE.txt)
