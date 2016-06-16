module ExampleData exposing (..)
-- where

import Dict exposing (Dict)

import Viewer exposing (..)


source =
  [ "fac n ="
  , "  if n == 1 then"
  , "    1"
  , "  else"
  , "    n * (fac (n-1))"
  , ""
  , "main = fac 3"
  ]


funcDefinitionSpans =
  [ ("main", onOneLine 7 (0, 12))
  , ("fac", { start = { line = 1, col = 0 }, end = { line = 5, col = 19 } })
  ]
  |> Dict.fromList


onOneLine line (startCol, endCol) =
  { start = { line = line, col = startCol }
  , end = { line = line, col = endCol }
  }


literalThree =
  (IntV 3, Literal (onOneLine 7 (11, 12)))


literalOne =
  (IntV 1, Literal (onOneLine 5 (16, 17)))


returnLiteralOne =
  (IntV 1, Literal (onOneLine 3 (4, 5)))


facCallSpan =
  onOneLine 5 (9, 19)


timesCallSpan =
  onOneLine 5 (9, 19)


minusCallSpan =
  onOneLine 7 (14, 17)


threeMinusOne =
  (IntV 2, FuncCall 2)


twoMinusOne =
  (IntV 2, FuncCall 5)


threeTimesTwo =
  (IntV 6, FuncCall 3)


twoTimesOne =
  (IntV 2, FuncCall 6)


callTree =
  { root = 0
  , calls =
      [ (0, { name = "main"
            , args = []
            , result = threeTimesTwo
            , caller = Nothing
            , calls = [1]
            }
        )
      -- fac(3) => 6
      , (1, { name = "fac"
            , args = [ literalThree ]
            , result = threeTimesTwo
            , caller = Just (0, onOneLine 7 (7, 12))
            , calls = [2, 3]
            }
        )
      -- n - 1 => 2
      , (2, { name = "-"
            , args = [ literalThree, literalOne ]
            , result = threeMinusOne
            , caller = Just (1, minusCallSpan)
            , calls = []
            }
        )
      -- fac(2) => 2
      , (3, { name = "fac"
            , args = [ twoMinusOne ]
            , result = twoTimesOne
            , caller = Just (1, facCallSpan)
            , calls = [4, 6]
            }
        )
      -- 2 - 1 => 1
      , (4, { name = "-"
            , args = [ threeMinusOne, literalOne ]
            , result = twoMinusOne
            , caller = Just (1, minusCallSpan)
            , calls = []
            }
        )
      -- fac(1)
      , (5, { name = "fac"
            , args = [ twoMinusOne ]
            , result = returnLiteralOne
            , caller = Just (2, facCallSpan)
            , calls = []
            }
        )
      -- 2 * fac 1
      , (6, { name = "*"
            , args = [ threeMinusOne, returnLiteralOne ]
            , result = twoTimesOne
            , caller = Just (1, timesCallSpan)
            , calls = []
            }
        )
      -- 3 * fac 2
      , (7, { name = "*"
            , args = [ literalThree, twoTimesOne ]
            , result = threeTimesTwo
            , caller = Just (1, timesCallSpan)
            , calls = []
            }
        )
      ]
      |> Dict.fromList
  }
