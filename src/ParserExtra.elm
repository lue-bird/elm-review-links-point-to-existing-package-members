module ParserExtra exposing (find, manySeparated)

import Elm.Syntax.Range exposing (Range)
import Parser exposing ((|.), (|=), Parser)


{-| `range` is relative to the string start (so 0,0).
-}
find :
    Parser a
    -> String
    -> List { parsed : a, range : Range }
find parser string =
    string
        |> Parser.run (findParser parser)
        |> Result.withDefault []


findParser :
    Parser a
    -> Parser (List { parsed : a, range : Range })
findParser parser =
    Parser.loop []
        (\parsedList ->
            Parser.oneOf
                [ Parser.succeed (\parsed -> parsed :: parsedList)
                    |= (Parser.succeed
                            (\( startRow, startCol ) parsed ( endRow, endCol ) ->
                                { parsed = parsed
                                , range =
                                    -- parser position starts at ( 1 , 1 )
                                    -- but we want the relative location
                                    { start = { row = startRow - 1, column = startCol - 1 }
                                    , end = { row = endRow - 1, column = endCol - 2 }
                                    }
                                }
                            )
                            |= Parser.getPosition
                            |= parser
                            |= Parser.getPosition
                       )
                    |> Parser.map Parser.Loop
                , Parser.succeed parsedList
                    |. Parser.chompIf (\_ -> True)
                    |> Parser.map Parser.Loop
                , Parser.succeed (List.reverse parsedList)
                    |. Parser.end
                    |> Parser.map Parser.Done
                ]
        )


{-| 0 or more things directly separated by a string like "go-gi-ga".
-}
manySeparated :
    { by : String
    , item : Parser between
    }
    -> Parser (List between)
manySeparated { by, item } =
    Parser.sequence
        { start = ""
        , separator = by
        , end = ""
        , spaces = Parser.symbol ""
        , item = item
        , trailing = Parser.Forbidden
        }
