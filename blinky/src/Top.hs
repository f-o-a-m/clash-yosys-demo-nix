{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE RankNTypes #-}
module Top where

import Clash.Prelude
import Control.Lens as Lens (Lens, Lens', over)

{-# ANN topEntity
  (Synthesize
    { t_name     = "blinky"
    , t_inputs   =
        [ PortName "clk"
        , PortName "pmod1_1"
        , PortName "pmod1_2"
        , PortName "pmod1_3"
        , PortName "pmod1_4"
        ]
    , t_output   = PortProduct "out"
        [ PortName "led1"
        , PortName "led2"
        , PortName "led3"
        ]
    }) #-}
topEntity
  :: Clock System Source
  -> Bit -> Bit -> Bit
  -> Signal System (Bit, Bit, Bit)
topEntity clk _ _ _ =
  withClockReset clk (unsafeToSyncReset (pure False)) $
    leds <$> counter

  where
    counter = register (0 :: Unsigned 32) ((+1) <$> counter)

    leds c =
      ( c!24
      , c!23
      , c!22
      )

main :: IO ()
main = print "hello world"
