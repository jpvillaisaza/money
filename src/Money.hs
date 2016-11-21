{-# LANGUAGE FlexibleContexts #-}
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
--
-- Examples:
--
-- >>> 1000 :: Money COP
-- COP 1000.0
--
-- >>> 1000 :: Money EUR
-- EUR 1000.0
--
-- >>> 1000 :: Money USD
-- USD 1000.0
--
-- >>> 999.99 :: Money USD
-- USD 999.99
--
-- >>> 1000 + 500 :: Money COP
-- COP 1500.0
--
-- >>> (1000 :: Money COP) + (500 :: Money EUR)
-- ...
--
-- >>> 1000 == (500 * 2 :: Money USD)
-- True
--
-- >>> 1000 < (500 :: Money EUR)
-- False

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
-- Examples:
--
-- >>> 3167.20 :: ExchangeRate USD COP
-- COP 3167.2
--
-- >>> 1.06 :: ExchangeRate EUR USD
-- USD 1.06
--
-- >>> 0.94 :: ExchangeRate USD EUR
-- EUR 0.94

newtype ExchangeRate currency1 currency2 =
  ExchangeRate
    { getExchangeRate :: Money currency2
    }
  deriving (Eq, Fractional, Num)


-- |
--
--

instance Show (Money cur2) => Show (ExchangeRate cur1 cur2) where
  show =
    show . getExchangeRate


-- |
--
--
--
-- Examples:
--
-- >>> interchange (1.06 :: ExchangeRate EUR USD)
-- EUR 0.94...
--
-- >>> interchange (0.94 :: ExchangeRate USD EUR)
-- USD 1.06...
--
-- >>> interchange (interchange (1.06 :: ExchangeRate EUR USD))
-- USD 1.06
--
-- >>> interchange (interchange (0.94 :: ExchangeRate USD EUR))
-- EUR 0.94

interchange
  :: ExchangeRate cur1 cur2
  -> ExchangeRate cur2 cur1
interchange (ExchangeRate exchangeRate) =
  ExchangeRate (Money (1 / getAmount exchangeRate))


-- |
--
--
--
-- Examples:
--
-- >>> usdToCop = 3182.01 :: ExchangeRate USD COP
--
-- >>> convert usdToCop 1000
-- COP 3182010.0
--
-- >>> convert (interchange usdToCop) 1000
-- USD 0.31...
--
-- >>> convert (interchange usdToCop) (convert usdToCop 1000)
-- USD 1000.0

convert
  :: ExchangeRate cur1 cur2
  -> Money cur1
  -> Money cur2
convert (ExchangeRate exchangeRate) (Money amount) =
  Money (amount * getAmount exchangeRate)


-- |
--
--
--
-- Examples:
--
-- >>> usdToCop = 3182.01 :: ExchangeRate USD COP
--
-- >>> convert' usdToCop 1000
-- USD 0.31...
--
-- >>> convert' (interchange usdToCop) 1000
-- COP 3182010.0
--
-- >>> convert' usdToCop (convert' (interchange usdToCop) 1000)
-- USD 1000.0
--
-- >>> convert' usdToCop (convert usdToCop 1000)
-- USD 1000.0

convert'
  :: ExchangeRate cur2 cur1
  -> Money cur1
  -> Money cur2
convert' exchangeRate =
  convert (interchange exchangeRate)
