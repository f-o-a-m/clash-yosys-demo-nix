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
      { t_name = "megapickle"
      , t_inputs = [PortName "signal"]
      , t_output = [PortName "out"]
      }
    ) #-}


  {- | Direct form FIR filter -}
fir 
    :: (HiddenClockReset dom gated sync, Num a, KnownNat n, Undefined a) 
    => Vec (n + 1) a -- ^ Coefficients
    -> Signal dom Bool   -- ^ Input enable
    -> Signal dom a      -- ^ Input samples
    -> Signal dom a      -- ^ Output samples
fir coeffs en x = dotp (map pure coeffs) (iterateI (regEn 0 en) x)
    where
          dotp as bs = fold (+) (zipWith (*) as bs)

topEntity :: Signal (Signed 16) -> Signal (Signed 16)
topEntity = fir (3 :> 5 :> 7 :> 9 :> 11 :> 13 :> 15 :> Nil)

--{-# ANN topEntity
--  (Synthesize
--    { t_name     = "megapickle"
--    , t_inputs   =
--        [ PortName "clk"
--        , PortName "pmod1"
--        , PortName "pmod2"
--        , PortName "pmod3"
--        , PortName "pmod4"
--        ]
--    , t_output   = PortProduct "out"
--        [ PortName "led1"
--        , PortName "led2"
--        , PortName "led3"
--        , PortName "led4"
--        , PortName "led5"
--        ]
--    }) #-}
--topEntity
--  :: Clock System Source
--  -> Bit -> Bit -> Bit
--  -> Signal System (Bit, Bit, Bit, Bit, Bit)
--topEntity clk _ _ _ =
--  withClockReset clk (unsafeToSyncReset (pure False)) $
--    leds <$> counter
--
--  where
--    counter = register (0 :: Unsigned 32) ((+1) <$> counter)
--
--    leds c =
--      ( c!24
--      , c!23
--      , c!22
--      , c!21
--      , c!20
--      )

main :: IO ()
main = print "hello world"
