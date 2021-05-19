# Data Structures and Algorithms: Comparison between Functional and Imperative Approaches

Benchmarks between functional and imperative implementations of basic data structures and algorithms.

## Tools

### Elm

**Version:** 0.19.1

**Node Version:** v13.14.0

**Compile command:** `elm make --optimize input.elm output.js`

**Formatter:** [Elm-format](https://github.com/avh4/elm-format)

**Benchmark:** [Elm-explorations' Benchmark](https://github.com/elm-explorations/benchmark)

### Typescript

**Version:** 4.2.3

**Node Version:** v13.14.0

**Compile command:** `tsc input.ts`

**Formatter:** [Prettier](https://prettier.io/)

**Benchmark:** [Benny](https://github.com/caderek/benny)

### Haskell

**Version:** The Glorious Glasgow Haskell Compilation System, version 8.0.2

**Compile command:** `ghc -O2 input.hs`

**Formatter:** [Brittany](https://hackage.haskell.org/package/brittany)

**Benchmark:** [Criterion](http://www.serpentine.com/criterion/)

### C++

**Version:** g++ (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0

**Compile command:** `g++ -std=c++11 -O3 input.cpp`

**Formatter:** [VsCode's formatter with Google config](https://code.visualstudio.com/)

**Benchmark:** [Google's Benchmark](https://github.com/google/benchmark)
