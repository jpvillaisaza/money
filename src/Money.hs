{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Money
  ( COP
  , EUR
  , USD
  , Amount (..)
  , ExchangeRate (..)
  , convert
  , convert'
  )
  where


-- |
--
--

data COP


-- |
--
--

data EUR


-- |
--
--

data USD


-- |
--
--

newtype Amount currency =
  Amount
    { getAmount :: Rational
    }
  deriving (Eq, Fractional, Num, Ord)


-- |
--
--

instance Show (Amount COP) where
  show Amount {..} =
    '$' : show (fromRational getAmount :: Double)


-- |
--
--

instance Show (Amount EUR) where
  show Amount {..} =
    '€' : show (fromRational getAmount :: Double)


-- |
--
--

instance Show (Amount USD) where
  show Amount {..} =
    '$' : show (fromRational getAmount :: Double)


-- |
--
--
--
-- >>> usdToCop = 3182.01 :: ExchangeRate USD COP

newtype ExchangeRate currency1 currency2 =
  ExchangeRate
    { getExchangeRate :: Rational
    }
  deriving (Eq, Fractional, Num)


-- |
--
--

instance Show (ExchangeRate currency1 currency2) where
  show ExchangeRate {..} =
    show (fromRational getExchangeRate :: Double)


-- |
--
--

convert
  :: ExchangeRate currency1 currency2
  -> Amount currency1
  -> Amount currency2
convert ExchangeRate {..} Amount {..} =
  Amount (getAmount * getExchangeRate)


-- |
--
--

convert'
  :: ExchangeRate currency2 currency1
  -> Amount currency1
  -> Amount currency2
convert' ExchangeRate {..} =
  convert (ExchangeRate (1 / getExchangeRate))
