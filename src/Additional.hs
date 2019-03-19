module Additional where

import qualified Data.Aeson as J

-- had to move this out due to GHC staging restriction
jsonOpts :: Int -> J.Options
jsonOpts n = J.defaultOptions {J.fieldLabelModifier = J.camelTo2 '_' . drop n}
