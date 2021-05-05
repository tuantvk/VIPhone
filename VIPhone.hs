{-# LANGUAGE ScopedTypeVariables #-}

module VIPhone where


import Data.Csv
import Text.Regex
import Text.Regex.Posix
import Text.Regex.Base.RegexLike hiding (getAllTextMatches)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL


rule =
  [ ( "\\+84|⁰|o|O|ko|h(ô|u)ng|kh(o|ô)ng|zero" , "0" )
  , ( "\\.|\\_|\\s|-|,"                        , ""  )
  , ( "n(a|ă)m|nh(a|ă)m|ng(ũ)|five"            , "5" )
  , ( "b(a|ả)y|th(a|ấ)t|seven"                 , "7" )
  , ( "ba|three"                               , "3" )
  , ( "m(o|ộ)t|nh(ấ)t|one"                     , "1" )
  , ( "hai|nh(ị)|two"                          , "2" )
  , ( "b(o|ố)n|t(ư|ứ)|four"                    , "4" )
  , ( "s(a|á)u|l(ụ)c|six"                      , "6" )
  , ( "t(a|á)m|b(á)t|eight"                    , "8" )
  , ( "ch(i|í)n|ch(i|í)nh|c(ử)u|nine"          , "9" )
  ]

getRule = map  (\(sa, se) -> (mkRegex sa, se))  rule


regexByRule :: [(Regex, String)] -> String -> String
regexByRule = flip $ foldl (\acc (re, v) -> subRegex re acc v)


formatNumbers :: String -> String
formatNumbers = regexByRule getRule


main :: IO ()
main = do
    csvData <- BL.readFile "data_test.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (userId :: Int, message) -> do
            let str = formatNumbers message
            let result = getAllTextMatches (str =~ "[0-9]{10}") :: [String]
            print result
