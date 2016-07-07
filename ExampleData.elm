module ExampleData exposing (..)


import Dict exposing (Dict)

import Model exposing (..)


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
  (IntV 3, Literal 0 (onOneLine 7 (11, 12)))


literalOne callId =
  (IntV 1, Literal callId (onOneLine 5 (16, 17)))


returnLiteralOne =
  (IntV 1, Literal 5 (onOneLine 3 (4, 5)))


facCallSpan =
  onOneLine 5 (9, 19)


timesCallSpan =
  onOneLine 5 (9, 19)


minusCallSpan =
  onOneLine 7 (14, 17)


threeMinusOne =
  (IntV 2, FuncCall 2)


twoMinusOne =
  (IntV 1, FuncCall 5)


threeTimesTwo =
  (IntV 6, FuncCall 7)


twoTimesOne =
  (IntV 2, FuncCall 6)


callTree =
  { root = 0
  , calls =
      [ (0, { name = "main"
            , args = []
            , result = threeTimesTwo
            , caller = Nothing
            , subcalls = [1]
            }
        )
      -- fac(3) => 6
      , (1, { name = "fac"
            , args = [ literalThree ]
            , result = threeTimesTwo
            , caller = Just (0, onOneLine 7 (7, 12))
            , subcalls = [2, 3, 7]
            }
        )
      -- n - 1 => 2
      , (2, { name = "-"
            , args = [ literalThree, literalOne 1 ]
            , result = threeMinusOne
            , caller = Just (1, minusCallSpan)
            , subcalls = []
            }
        )
      -- fac(2) => 2
      , (3, { name = "fac"
            , args = [ threeMinusOne ]
            , result = twoTimesOne
            , caller = Just (1, facCallSpan)
            , subcalls = [4, 5, 6]
            }
        )
      -- 2 - 1 => 1
      , (4, { name = "-"
            , args = [ threeMinusOne, literalOne 3 ]
            , result = twoMinusOne
            , caller = Just (3, minusCallSpan)
            , subcalls = []
            }
        )
      -- fac(1)
      , (5, { name = "fac"
            , args = [ twoMinusOne ]
            , result = returnLiteralOne
            , caller = Just (3, facCallSpan)
            , subcalls = []
            }
        )
      -- 2 * fac 1
      , (6, { name = "*"
            , args = [ threeMinusOne, returnLiteralOne ]
            , result = twoTimesOne
            , caller = Just (3, timesCallSpan)
            , subcalls = []
            }
        )
      -- 3 * fac 2
      , (7, { name = "*"
            , args = [ literalThree, twoTimesOne ]
            , result = threeTimesTwo
            , caller = Just (1, timesCallSpan)
            , subcalls = []
            }
        )
      ]
      |> Dict.fromList
  }
