module ParserExtra exposing (find)

import Elm.Syntax.Range exposing (Range)
import Parser exposing ((|.), (|=), Parser)


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
        (\links ->
            Parser.oneOf
                [ Parser.succeed (\link -> link :: links)
                    |= (Parser.succeed
                            (\( startRow, startCol ) x ( endRow, endCol ) ->
                                { parsed = x
                                , range =
                                    -- parser position starts at ( 1 , 1 )
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
                , Parser.succeed links
                    |. Parser.chompIf (\_ -> True)
                    |> Parser.map Parser.Loop
                , Parser.succeed (List.reverse links)
                    |. Parser.end
                    |> Parser.map Parser.Done
                ]
        )
