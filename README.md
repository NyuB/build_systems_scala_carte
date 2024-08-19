# Build systems 'scala' carte

Original paper: [https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems.pdf](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems.pdf)

This code re-implements the abstractions presented in the paper, using Scala instead of Haskell, and explores or extends other functionalities permitted by the proposed framework. I try to mimic the paper code to ease reading them side by side, but I do not hesitate to rewrite it in an arguably less 'haskellish' style when I find it more ergonomic, especially for observability and tests features where objects and/or side effects are of great helps.

Due to some Haskell StdLib features being absent from the Scala StdLib, quick implementations of Monads & Co are also present.