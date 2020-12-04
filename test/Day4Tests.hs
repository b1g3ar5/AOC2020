{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day4Tests(
    day4Tests
  , day4Props
) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Bifunctor (Bifunctor(first))
import Data.List.Split ( splitOn )
import Test.Tasty.QuickCheck as QC

import Day4 ( byrOK, eclOK, hclOK, hgtOK, pidOK, valid2, getPassportData )

day4Props :: TestTree
day4Props = testGroup "Day4 Props"
  [
  -- Check Ref2 first
    --QC.testProperty "Ref2" $ \r -> neRef2 (r::Ref2) == (eRef2 . nRef2 $ r)
  -- Check TMap
  ]


day4Tests :: TestTree
day4Tests = testGroup "Day4 tests" $
  [ testCase "t1" (byrOK "2002" @?= True)
  , testCase "t2" (byrOK "2003" @?= False)
  , testCase "t3" (hgtOK "60in" @?= True)
  , testCase "t4" (hgtOK "190cm" @?= True)
  , testCase "t5" (hgtOK "190" @?= False)
  , testCase "t6" (hgtOK "190in" @?= False)
  , testCase "t7" (hclOK "#123abc" @?= True)
  , testCase "t8" (hclOK "#123abz" @?= False)
  , testCase "t9" (hclOK "123abc" @?= False)
  , testCase "t10" (eclOK "brn" @?= True)
  , testCase "t11" (eclOK "wat" @?= False)
  , testCase "t12" (pidOK "000000001" @?= True)
  , testCase "t13" (pidOK "0123456789" @?= False)
  ] ++ day4InvalidTests ++ day4ValidTests
  

day4InvalidTests :: [TestTree]
day4InvalidTests = (\p -> testCase "invalids" (valid2 p @?= False)) <$> getPassportData invalidTests

day4ValidTests :: [TestTree]
day4ValidTests = (\p -> testCase "valids" (valid2 p @?= True)) <$> getPassportData validTests



invalidTests = ["eyr:1972 cid:100"
  , "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
  , ""
  , "iyr:2019"
  , "hcl:#602927 eyr:1967 hgt:170cm"
  , "ecl:grn pid:012533040 byr:1946"
  , ""
  , "hcl:dab227 iyr:2012"
  , "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
  , ""
  , "hgt:59cm ecl:zzz"
  , "eyr:2038 hcl:74454a iyr:2023"
  , "pid:3556412378 byr:2007"]

validTests = ["pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
  , "hcl:#623a2f"
  , ""
  , "eyr:2029 ecl:blu cid:129 byr:1989"
  , "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
  , ""
  , "hcl:#888785"
  , "hgt:164cm byr:2001 iyr:2015 cid:88"
  , "pid:545766238 ecl:hzl"
  , "eyr:2022"
  , ""
  , "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"]
