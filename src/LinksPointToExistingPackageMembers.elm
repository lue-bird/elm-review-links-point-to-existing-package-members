module LinksPointToExistingPackageMembers exposing (rule)

{-|

@docs rule

-}

import Dict
import Elm.Module as Module exposing (Name(..))
import Elm.Project as Project
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Node as Node exposing (Node(..))
import Parser
import ParserExtra as Parser
import Review.Rule as Rule exposing (Rule)
import SyntaxHelp exposing (LinkKind(..), ModuleInfo, addLocation, docOfDeclaration, isExposed, linkParser, moduleInfo)
import Testable exposing (definitionInLinkNotExposedMessage, linkPointsToNonExistentMemberDetails, moduleInLinkNotExposed)


{-| Reports links to nonexistent package definitions or modules.

    config =
        [ LinksPointToExistingPackageMembers.rule
        ]


## Fails

    module A exposing (a)

    b =
        "b"

    {-| Not [`b`](A#b).
    -}
    a =
        "a"

Fails because `b` must be exposed.


## Success

    module A.And.B exposing (a, b)

    b =
        "b"

    {-| Not [`b`](A-And-B#b).
    -}
    a =
        "a"


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template lue-bird/elm-review-links-point-to-existing-package-members/example --rules LinksPointToExistingPackageMembers
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "LinksPointToExistingPackageMembers" []
        |> Rule.withElmJsonModuleVisitor
            (\project _ ->
                case project of
                    Just (Project.Package { exposed }) ->
                        let
                            exposedModules =
                                case exposed of
                                    Project.ExposedList exposedList ->
                                        exposedList

                                    Project.ExposedDict exposedDict ->
                                        exposedDict
                                            |> List.concatMap (\( _, names ) -> names)
                        in
                        exposedModules
                            |> List.map
                                (\name ->
                                    { moduleName = name |> Module.toString |> String.split "."
                                    , exposedDefinitions = Exposing.Explicit []
                                    }
                                )

                    Just (Project.Application _) ->
                        []

                    Nothing ->
                        []
            )
        |> Rule.withModuleDefinitionVisitor
            (\(Node _ module_) moduleInfos ->
                ( []
                , let
                    info =
                        module_ |> moduleInfo
                  in
                  if
                    moduleInfos
                        |> List.any
                            (.moduleName >> (==) info.moduleName)
                  then
                    moduleInfos |> (::) info

                  else
                    moduleInfos
                )
            )
        |> Rule.withDeclarationEnterVisitor
            (\(Node _ declaration) exposed ->
                ( declaration
                    |> docOfDeclaration
                    |> Maybe.map (checkLinks exposed)
                    |> Maybe.withDefault []
                , exposed
                )
            )
        |> Rule.fromModuleRuleSchema


checkLinks : List ModuleInfo -> Node String -> List (Rule.Error {})
checkLinks moduleInfo doc =
    (doc |> Node.value)
        |> Parser.run (Parser.find linkParser)
        |> Result.toMaybe
        |> Maybe.withDefault []
        |> List.concatMap
            (\match ->
                let
                    { moduleName, kind } =
                        match.parsed

                    moduleNameParts =
                        let
                            ( first, tail ) =
                                moduleName
                        in
                        first :: tail

                    range =
                        { start = addLocation (Node.range doc).start match.range.start
                        , end = addLocation (Node.range doc).start match.range.end
                        }
                in
                case kind of
                    ModuleLink ->
                        if
                            moduleInfo
                                |> List.any
                                    (.moduleName >> (==) moduleNameParts)
                        then
                            []

                        else
                            [ Rule.error
                                { message = moduleInLinkNotExposed
                                , details = linkPointsToNonExistentMemberDetails
                                }
                                range
                            ]

                    DefinitionLink definition ->
                        if
                            moduleInfo
                                |> List.any
                                    (\m ->
                                        m.moduleName
                                            == moduleNameParts
                                            && (m.exposedDefinitions
                                                    |> isExposed definition
                                               )
                                    )
                        then
                            []

                        else
                            [ Rule.error
                                { message = definitionInLinkNotExposedMessage
                                , details = linkPointsToNonExistentMemberDetails
                                }
                                range
                            ]
            )
