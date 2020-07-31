{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import Operator.Helm.OpenAPI.Model
import Operator.Helm.OpenAPI.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmRelease)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseList)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseListMetadata)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpec)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpecChart)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpecChartChartPullSecret)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpecChartFileRef)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpecChartSecretRef)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpecConfigMapKeyRef)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpecExternalSourceRef)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpecRollback)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpecSecretKeyRef)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpecTest)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpecValueFileSecrets)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseSpecValuesFrom)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseStatus)
      propMimeEq MimeJSON (Proxy :: Proxy V1HelmReleaseStatusConditions)
      
