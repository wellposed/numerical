{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numerics.Types.MArray where


class MArray mv world rep lay (view:: Locality) sh elem where