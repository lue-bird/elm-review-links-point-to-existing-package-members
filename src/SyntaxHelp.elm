module SyntaxHelp exposing (Link, LinkKind(..), ModuleInfo, addLocation, addRange, docOfDeclaration, isExposed, linkParser, moduleInfo)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser


docOfDeclaration : Declaration -> Maybe (Node Documentation)
docOfDeclaration declaration =
    case declaration of
        FunctionDeclaration { documentation } ->
            documentation

        AliasDeclaration { documentation } ->
            documentation

        CustomTypeDeclaration { documentation } ->
            documentation

        PortDeclaration _ ->
            Nothing

        InfixDeclaration _ ->
            Nothing

        Destructuring _ _ ->
            Nothing


type alias ModuleInfo =
    { moduleName : List String
    , exposedDefinitions : Exposing
    }


moduleInfo : Module -> ModuleInfo
moduleInfo module_ =
    let
        extract { moduleName, exposingList } =
            { moduleName = moduleName |> Node.value
            , exposedDefinitions = exposingList |> Node.value
            }
    in
    case module_ of
        NormalModule data ->
            extract data

        PortModule data ->
            extract data

        EffectModule data ->
            extract data


isExposed : String -> Exposing -> Bool
isExposed definition exposings =
    case exposings of
        All _ ->
            True

        Explicit list ->
            list
                |> List.any
                    (Node.value
                        >> nameOfExpose
                        >> (==) definition
                    )


nameOfExpose : TopLevelExpose -> String
nameOfExpose expose =
    case expose of
        InfixExpose name ->
            name

        FunctionExpose name ->
            name

        TypeOrAliasExpose name ->
            name

        TypeExpose { name } ->
            name


addRange aRange bRange =
    { start = addLocation aRange.start bRange.start
    , end = addLocation aRange.end bRange.end
    }


addLocation aRange bRange =
    { row = aRange.row + bRange.row
    , column = aRange.column + bRange.column
    }


type alias Link =
    { moduleName : ( String, List String )
    , kind : LinkKind
    }


type LinkKind
    = ModuleLink
    | DefinitionLink String


nameParser : Parser ()
nameParser =
    Parser.chompWhile
        (\c -> Char.isAlphaNum c || c == '_')


linkParser : Parser Link
linkParser =
    Parser.succeed
        (\firstModulePart modulePartsAfter kind ->
            { moduleName = ( firstModulePart, modulePartsAfter )
            , kind = kind
            }
        )
        |. Parser.symbol "["
        |. Parser.chompUntil "]"
        |. Parser.symbol "]("
        |= Parser.getChompedString nameParser
        |= Parser.many
            (Parser.succeed identity
                |. Parser.symbol "-"
                |= (nameParser
                        |> Parser.getChompedString
                   )
            )
        |= Parser.oneOf
            [ Parser.succeed DefinitionLink
                |. Parser.symbol "#"
                |= Parser.getChompedString (Parser.chompUntil ")")
            , Parser.succeed ModuleLink
                |. Parser.symbol ")"
            ]
