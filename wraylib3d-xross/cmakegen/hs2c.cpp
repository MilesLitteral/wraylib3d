//M.hs
module Foo where

foreign export ccall foo :: Int -> Int

foo :: Int -> Int
foo = floor . sqrt . fromIntegral

//test.cpp
#include <iostream>
#include "M_stub.h"

int main(int argc, char *argv[])
{
    std::cout << "hello\n";
    hs_init(&argc, &argv);
    std::cout << foo(500) << "\n";
    hs_exit();
    return 0;
}

//ghc -XForeignFunctionInterface -c M.hs
//Or: ghc -no-hs-main M.o test.o -lstdc++
