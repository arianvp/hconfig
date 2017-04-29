# hconfig

Applicative configuration inspired by the talk
[Move Over Free Monads: Make Way for Free Applicatives!][talk]

[talk]: https://www.youtube.com/watch?v=H28QqxO7Ihc

## Example

You can use the applicative DSL in `Data.Config` to build a description of your
configuration. This description contains the keys and types of your
configuration, for consumption by various _interpreters_. Here is an example of
such a description, for PostgreSQL connections:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Database.PostgreSQL (PoolConfiguration)

postgreSQLPool :: Config Text PoolConfiguration
postgreSQLPool =
  PoolConfiguration
  <$> string "user"
  <*> string "password"
  <*> string "host"
  <*> int    "port"
  <*> string "database"
```
