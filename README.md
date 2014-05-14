# MVC Service library

*Note: this library is work in progress* 

This library provides an abstraction for asynchronous request/response(s) services based on the MVC framework.  

## Setup (sandboxed)

    git clone https://github.com/cmahon/mvc-service.git
    cd mvc-service
    cabal sandbox init
    cabal install --only-dependencies --enable-tests --enable-benchmarks
    cabal configure 
    cabal build
    cabal test
    cabal bench
    cabal run

## Reference

* [MVC library article](http://www.haskellforall.com/2014/04/model-view-controller-haskell-style.html)
* [MVC library on hackage](http://hackage.haskell.org/package/mvc)
* [Haskell project skeleton](https://github.com/tfausak/haskeleton)


