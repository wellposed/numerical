{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numerics.Types.Layout where

{-| RowMajor will be defined by a foldRight  over the  ix :^ ixes list
ColumnMajor

-}