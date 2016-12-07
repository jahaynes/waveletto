module Data.Wavelet where

import Data.Bits    (Bits)

import Data.Wavelet.Internal.Types

class Wavelet structure where

    {- Return the element at a given position -}
    access :: Bits a => structure -> Position -> a

    {- Count the occurrences of a given symbol up to a given position -}
    rank :: Bits a => structure -> a -> Position -> Count

    {- For a given symbol, find its nth occurrence -}
    select :: Bits a => structure -> a -> Int -> Position

    {- Return the length of the source -}
    length :: structure -> Int
