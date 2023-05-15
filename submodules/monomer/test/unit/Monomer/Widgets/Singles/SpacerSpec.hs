{-|
Module      : Monomer.Widgets.Singles.SpacerSpec
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Spacer widget.
-}
module Monomer.Widgets.Singles.SpacerSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Core
import Monomer.TestUtil
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Grid
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Lens as L

spec :: Spec
spec = describe "Spacer/Filler" $ do
  spacerSpec
  fillerSpec

spacerSpec :: Spec
spacerSpec = describe "Spacer" $ do
  spacerSizeReqBox
  spacerSizeReqGrid
  spacerSizeReqStack

spacerSizeReqBox :: Spec
spacerSizeReqBox = describe "spacerSizeReqBox" $ do
  it "should return (Fixed 10, Fixed 10)" $ do
    sizeReqW1 `shouldBe` fixedSize 10
    sizeReqH1 `shouldBe` fixedSize 10

  where
    wenv = mockWenvEvtUnit ()
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv (box spacer)

spacerSizeReqGrid :: Spec
spacerSizeReqGrid = describe "spacerSizeReqGrid" $ do
  it "should return (Fixed 10, Flex 5 0.5) for horizontal" $ do
    sizeReqW1 `shouldBe` fixedSize 10
    sizeReqH1 `shouldBe` flexSize 5 0.5

  it "should return (Flex 5 0.5, Fixed 10) for vertical" $ do
    sizeReqW2 `shouldBe` flexSize 5 0.5
    sizeReqH2 `shouldBe` fixedSize 10

  where
    wenv = mockWenvEvtUnit ()
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv (hgrid [spacer])
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv (vgrid [spacer])

spacerSizeReqStack :: Spec
spacerSizeReqStack = describe "spacerSizeReqStack" $ do
  it "should return (Fixed 10, Flex 5 0.5) for horizontal" $ do
    sizeReqW1 `shouldBe` fixedSize 10
    sizeReqH1 `shouldBe` flexSize 5 0.5

  it "should return (Flex 10 0.5, Fixed 5) for vertical" $ do
    sizeReqW2 `shouldBe` flexSize 5 0.5
    sizeReqH2 `shouldBe` fixedSize 10

  where
    wenv = mockWenv ()
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv (hstack [spacer])
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv (vstack [spacer])

fillerSpec :: Spec
fillerSpec = describe "Filler" $ do
  fillerSizeReqBox
  fillerSizeReqGrid
  fillerSizeReqStack

fillerSizeReqBox :: Spec
fillerSizeReqBox = describe "fillerSizeReqBox" $ do
  it "should return (Expand 5 0.5, Expand 5 0.5)" $ do
    sizeReqW1 `shouldBe` expandSize 5 0.5
    sizeReqH1 `shouldBe` expandSize 5 0.5

  where
    wenv = mockWenvEvtUnit ()
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv (box filler)

fillerSizeReqGrid :: Spec
fillerSizeReqGrid = describe "fillerSizeReqGrid" $ do
  it "should return (Expand 5 0.5, Flex 5 0.5) for horizontal" $ do
    sizeReqW1 `shouldBe` expandSize 5 0.5
    sizeReqH1 `shouldBe` flexSize 5 0.5

  it "should return (Flex 5 0.5, Expand 5 0.5) for vertical" $ do
    sizeReqW2 `shouldBe` flexSize 5 0.5
    sizeReqH2 `shouldBe` expandSize 5 0.5

  where
    wenv = mockWenvEvtUnit ()
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv (hgrid [filler])
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv (vgrid [filler])

fillerSizeReqStack :: Spec
fillerSizeReqStack = describe "fillerSizeReqStack" $ do
  it "should return (Expand 5 0.5, Flex 5 0.5) for horizontal" $ do
    sizeReqW1 `shouldBe` expandSize 5 0.5
    sizeReqH1 `shouldBe` flexSize 5 0.5

  it "should return (Flex 5 0.5, Expand 5 0.5) for vertical" $ do
    sizeReqW2 `shouldBe` flexSize 5 0.5
    sizeReqH2 `shouldBe` expandSize 5 0.5

  where
    wenv = mockWenv ()
    (sizeReqW1, sizeReqH1) = nodeGetSizeReq wenv (hstack [filler])
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv (vstack [filler])
