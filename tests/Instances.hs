{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import Operator.Helm.OpenAPI.Model
import Operator.Helm.OpenAPI.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)
    
arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models
 
instance Arbitrary V1HelmRelease where
  arbitrary = sized genV1HelmRelease

genV1HelmRelease :: Int -> Gen V1HelmRelease
genV1HelmRelease n =
  V1HelmRelease
    <$> arbitraryReducedMaybe n -- v1HelmReleaseApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpec :: Maybe V1HelmReleaseSpec
    <*> arbitraryReducedMaybe n -- v1HelmReleaseStatus :: Maybe V1HelmReleaseStatus
  
instance Arbitrary V1HelmReleaseList where
  arbitrary = sized genV1HelmReleaseList

genV1HelmReleaseList :: Int -> Gen V1HelmReleaseList
genV1HelmReleaseList n =
  V1HelmReleaseList
    <$> arbitraryReducedMaybe n -- v1HelmReleaseListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1HelmReleaseListItems :: [V1HelmRelease]
    <*> arbitraryReducedMaybe n -- v1HelmReleaseListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseListMetadata :: Maybe V1HelmReleaseListMetadata
  
instance Arbitrary V1HelmReleaseListMetadata where
  arbitrary = sized genV1HelmReleaseListMetadata

genV1HelmReleaseListMetadata :: Int -> Gen V1HelmReleaseListMetadata
genV1HelmReleaseListMetadata n =
  V1HelmReleaseListMetadata
    <$> arbitraryReducedMaybe n -- v1HelmReleaseListMetadataContinue :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseListMetadataRemainingItemCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1HelmReleaseListMetadataResourceVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseListMetadataSelfLink :: Maybe Text
  
instance Arbitrary V1HelmReleaseSpec where
  arbitrary = sized genV1HelmReleaseSpec

genV1HelmReleaseSpec :: Int -> Gen V1HelmReleaseSpec
genV1HelmReleaseSpec n =
  V1HelmReleaseSpec
    <$> arbitraryReduced n -- v1HelmReleaseSpecChart :: V1HelmReleaseSpecChart
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecDisableOpenApiValidation :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecForceUpgrade :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecHelmVersion :: Maybe E'HelmVersion
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecMaxHistory :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecReleaseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecResetValues :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecRollback :: Maybe V1HelmReleaseSpecRollback
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecSkipCrDs :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecTargetNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecTest :: Maybe V1HelmReleaseSpecTest
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecTimeout :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecValueFileSecrets :: Maybe [V1HelmReleaseSpecValueFileSecrets]
    <*> arbitraryReducedMaybeValue n -- v1HelmReleaseSpecValues :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecValuesFrom :: Maybe [V1HelmReleaseSpecValuesFrom]
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecWait :: Maybe Bool
  
instance Arbitrary V1HelmReleaseSpecChart where
  arbitrary = sized genV1HelmReleaseSpecChart

genV1HelmReleaseSpecChart :: Int -> Gen V1HelmReleaseSpecChart
genV1HelmReleaseSpecChart n =
  V1HelmReleaseSpecChart
    <$> arbitraryReducedMaybe n -- v1HelmReleaseSpecChartChartPullSecret :: Maybe V1HelmReleaseSpecChartChartPullSecret
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecChartGit :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecChartName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecChartPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecChartRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecChartRepository :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecChartSecretRef :: Maybe V1HelmReleaseSpecChartSecretRef
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecChartSkipDepUpdate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecChartVersion :: Maybe Text
  
instance Arbitrary V1HelmReleaseSpecChartChartPullSecret where
  arbitrary = sized genV1HelmReleaseSpecChartChartPullSecret

genV1HelmReleaseSpecChartChartPullSecret :: Int -> Gen V1HelmReleaseSpecChartChartPullSecret
genV1HelmReleaseSpecChartChartPullSecret n =
  V1HelmReleaseSpecChartChartPullSecret
    <$> arbitrary -- v1HelmReleaseSpecChartChartPullSecretName :: Text
  
instance Arbitrary V1HelmReleaseSpecChartFileRef where
  arbitrary = sized genV1HelmReleaseSpecChartFileRef

genV1HelmReleaseSpecChartFileRef :: Int -> Gen V1HelmReleaseSpecChartFileRef
genV1HelmReleaseSpecChartFileRef n =
  V1HelmReleaseSpecChartFileRef
    <$> arbitraryReducedMaybe n -- v1HelmReleaseSpecChartFileRefOptional :: Maybe Bool
    <*> arbitrary -- v1HelmReleaseSpecChartFileRefPath :: Text
  
instance Arbitrary V1HelmReleaseSpecChartSecretRef where
  arbitrary = sized genV1HelmReleaseSpecChartSecretRef

genV1HelmReleaseSpecChartSecretRef :: Int -> Gen V1HelmReleaseSpecChartSecretRef
genV1HelmReleaseSpecChartSecretRef n =
  V1HelmReleaseSpecChartSecretRef
    <$> arbitrary -- v1HelmReleaseSpecChartSecretRefName :: Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecChartSecretRefNamespace :: Maybe Text
  
instance Arbitrary V1HelmReleaseSpecConfigMapKeyRef where
  arbitrary = sized genV1HelmReleaseSpecConfigMapKeyRef

genV1HelmReleaseSpecConfigMapKeyRef :: Int -> Gen V1HelmReleaseSpecConfigMapKeyRef
genV1HelmReleaseSpecConfigMapKeyRef n =
  V1HelmReleaseSpecConfigMapKeyRef
    <$> arbitraryReducedMaybe n -- v1HelmReleaseSpecConfigMapKeyRefKey :: Maybe Text
    <*> arbitrary -- v1HelmReleaseSpecConfigMapKeyRefName :: Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecConfigMapKeyRefNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecConfigMapKeyRefOptional :: Maybe Bool
  
instance Arbitrary V1HelmReleaseSpecExternalSourceRef where
  arbitrary = sized genV1HelmReleaseSpecExternalSourceRef

genV1HelmReleaseSpecExternalSourceRef :: Int -> Gen V1HelmReleaseSpecExternalSourceRef
genV1HelmReleaseSpecExternalSourceRef n =
  V1HelmReleaseSpecExternalSourceRef
    <$> arbitraryReducedMaybe n -- v1HelmReleaseSpecExternalSourceRefOptional :: Maybe Bool
    <*> arbitrary -- v1HelmReleaseSpecExternalSourceRefUrl :: Text
  
instance Arbitrary V1HelmReleaseSpecRollback where
  arbitrary = sized genV1HelmReleaseSpecRollback

genV1HelmReleaseSpecRollback :: Int -> Gen V1HelmReleaseSpecRollback
genV1HelmReleaseSpecRollback n =
  V1HelmReleaseSpecRollback
    <$> arbitraryReducedMaybe n -- v1HelmReleaseSpecRollbackDisableHooks :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecRollbackEnable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecRollbackForce :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecRollbackMaxRetries :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecRollbackRecreate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecRollbackRetry :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecRollbackTimeout :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecRollbackWait :: Maybe Bool
  
instance Arbitrary V1HelmReleaseSpecSecretKeyRef where
  arbitrary = sized genV1HelmReleaseSpecSecretKeyRef

genV1HelmReleaseSpecSecretKeyRef :: Int -> Gen V1HelmReleaseSpecSecretKeyRef
genV1HelmReleaseSpecSecretKeyRef n =
  V1HelmReleaseSpecSecretKeyRef
    <$> arbitraryReducedMaybe n -- v1HelmReleaseSpecSecretKeyRefKey :: Maybe Text
    <*> arbitrary -- v1HelmReleaseSpecSecretKeyRefName :: Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecSecretKeyRefNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecSecretKeyRefOptional :: Maybe Bool
  
instance Arbitrary V1HelmReleaseSpecTest where
  arbitrary = sized genV1HelmReleaseSpecTest

genV1HelmReleaseSpecTest :: Int -> Gen V1HelmReleaseSpecTest
genV1HelmReleaseSpecTest n =
  V1HelmReleaseSpecTest
    <$> arbitraryReducedMaybe n -- v1HelmReleaseSpecTestCleanup :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecTestEnable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecTestIgnoreFailures :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecTestTimeout :: Maybe Integer
  
instance Arbitrary V1HelmReleaseSpecValueFileSecrets where
  arbitrary = sized genV1HelmReleaseSpecValueFileSecrets

genV1HelmReleaseSpecValueFileSecrets :: Int -> Gen V1HelmReleaseSpecValueFileSecrets
genV1HelmReleaseSpecValueFileSecrets n =
  V1HelmReleaseSpecValueFileSecrets
    <$> arbitrary -- v1HelmReleaseSpecValueFileSecretsName :: Text
  
instance Arbitrary V1HelmReleaseSpecValuesFrom where
  arbitrary = sized genV1HelmReleaseSpecValuesFrom

genV1HelmReleaseSpecValuesFrom :: Int -> Gen V1HelmReleaseSpecValuesFrom
genV1HelmReleaseSpecValuesFrom n =
  V1HelmReleaseSpecValuesFrom
    <$> arbitraryReducedMaybe n -- v1HelmReleaseSpecValuesFromChartFileRef :: Maybe V1HelmReleaseSpecChartFileRef
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecValuesFromConfigMapKeyRef :: Maybe V1HelmReleaseSpecConfigMapKeyRef
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecValuesFromExternalSourceRef :: Maybe V1HelmReleaseSpecExternalSourceRef
    <*> arbitraryReducedMaybe n -- v1HelmReleaseSpecValuesFromSecretKeyRef :: Maybe V1HelmReleaseSpecSecretKeyRef
  
instance Arbitrary V1HelmReleaseStatus where
  arbitrary = sized genV1HelmReleaseStatus

genV1HelmReleaseStatus :: Int -> Gen V1HelmReleaseStatus
genV1HelmReleaseStatus n =
  V1HelmReleaseStatus
    <$> arbitraryReducedMaybe n -- v1HelmReleaseStatusConditions :: Maybe [V1HelmReleaseStatusConditions]
    <*> arbitraryReducedMaybe n -- v1HelmReleaseStatusLastAttemptedRevision :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1HelmReleaseStatusPhase :: Maybe E'Phase
    <*> arbitraryReducedMaybe n -- v1HelmReleaseStatusReleaseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseStatusReleaseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseStatusRevision :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseStatusRollbackCount :: Maybe Integer
  
instance Arbitrary V1HelmReleaseStatusConditions where
  arbitrary = sized genV1HelmReleaseStatusConditions

genV1HelmReleaseStatusConditions :: Int -> Gen V1HelmReleaseStatusConditions
genV1HelmReleaseStatusConditions n =
  V1HelmReleaseStatusConditions
    <$> arbitraryReducedMaybe n -- v1HelmReleaseStatusConditionsLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1HelmReleaseStatusConditionsLastUpdateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1HelmReleaseStatusConditionsMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HelmReleaseStatusConditionsReason :: Maybe Text
    <*> arbitrary -- v1HelmReleaseStatusConditionsStatus :: E'Status
    <*> arbitrary -- v1HelmReleaseStatusConditionsType :: E'Type
  

