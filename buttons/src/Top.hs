{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TupleSections              #-}
module Top where

import Clash.Prelude
import Control.Lens (Iso', iso, over)

{-# ANN topEntity
  (Synthesize
    { t_name     = "pwm"
    , t_inputs   =
        [ PortName "clk"
        , PortName "pmod1"
        , PortName "pmod2"
        , PortName "pmod3"
        , PortName "pmod4"
        ]
    , t_output   = PortProduct "out"
        [ PortName "led1"
        , PortName "led2"
        , PortName "led3"
        , PortName "led4"
        ]
    }) #-}
topEntity
  :: Clock System Source
  -> Bit -> Bit -> Bit -> Bit
  -> Signal System (Bit, Bit, Bit, Bit)
topEntity clk button1 _ _ _ =
  (, low, low, low) <$> button clk button1

button
  :: Clock domain source
  -> Bit
  -> Signal domain Bit
button _ b = pure b
{-# NOINLINE button #-}

main :: IO ()
main = print "hello world"
