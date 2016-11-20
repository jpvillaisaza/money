{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Money
  ( COP
  , EUR
  , USD
  , Money (..)
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

newtype Money currency =
  Money
    { getAmount :: Rational
    }
  deriving (Eq, Fractional, Num, Ord)


-- |
--
--

instance Show (Money COP) where
  show Money {..} =
    "COP " ++ show (fromRational getAmount :: Double)


-- |
--
--

instance Show (Money EUR) where
  show Money {..} =
    "EUR " ++ show (fromRational getAmount :: Double)


-- |
--
--

instance Show (Money USD) where
  show Money {..} =
    "USD " ++ show (fromRational getAmount :: Double)


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
-- >>> usdToCop = 3182.01 :: ExchangeRate USD COP
-- >>> convert usdToCop 1000
-- COP 3182010.0

convert
  :: ExchangeRate currency1 currency2
  -> Money currency1
  -> Money currency2
convert ExchangeRate {..} Money {..} =
  Money (getAmount * getExchangeRate)


-- |
--
--
--
-- >>> usdToCop = 3182.01 :: ExchangeRate USD COP
-- >>> convert' usdToCop (convert usdToCop 1000) == 1000
-- True

convert'
  :: ExchangeRate currency2 currency1
  -> Money currency1
  -> Money currency2
convert' ExchangeRate {..} =
  convert (ExchangeRate (1 / getExchangeRate))
