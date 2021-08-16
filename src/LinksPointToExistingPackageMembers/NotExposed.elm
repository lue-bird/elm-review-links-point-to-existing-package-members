module LinksPointToExistingPackageMembers.NotExposed exposing (definitionInLinkNotExposedMessage, linkPointsToNonExistentMemberDetails, moduleInLinkNotExposed, rule)

import Elm.Module as Module
import Elm.Project as Project
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import EverySet as Set exposing (EverySet)
import Parser
import ParserExtra as Parser
import Review.Rule as Rule exposing (Rule)
import SyntaxHelp exposing (Link, LinkKind(..), ModuleInfo, addLocation, docOfDeclaration, exposedModules, isExposed, linkParser, moduleInfo)


type alias Set a =
    EverySet a



--


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
    Rule.newProjectRuleSchema "LinksPointToExistingPackageMembers"
        { exposed = Set.empty
        , inDeclarationDoc = Set.empty
        , readme = Nothing
        }
        |> Rule.withReadmeProjectVisitor
            (\maybeReadme context ->
                ( []
                , case maybeReadme of
                    Just { readmeKey, content } ->
                        { context
                            | readme =
                                { key = readmeKey
                                , links =
                                    content
                                        |> Parser.find linkParser
                                        |> Set.fromList
                                }
                                    |> Just
                        }

                    Nothing ->
                        context
                )
            )
        |> Rule.withElmJsonProjectVisitor
            (\maybeProject context ->
                ( []
                , { context
                    | exposed =
                        case maybeProject of
                            Just { project } ->
                                case project of
                                    Project.Package { exposed } ->
                                        exposedModules exposed
                                            |> Set.fromList
                                            |> Set.map
                                                (\name ->
                                                    { moduleName = name |> Module.toString |> String.split "."
                                                    , exposedDefinitions = Exposing.Explicit []
                                                    }
                                                )

                                    Project.Application _ ->
                                        Set.empty

                            Nothing ->
                                Set.empty
                  }
                )
            )
        |> Rule.withModuleVisitor
            (Rule.withModuleDefinitionVisitor
                (\(Node _ module_) context ->
                    ( []
                    , { context
                        | exposed =
                            let
                                info =
                                    module_ |> moduleInfo
                            in
                            if
                                context.exposed
                                    |> Set.toList
                                    |> List.any
                                        (.moduleName
                                            >> (==) info.moduleName
                                        )
                            then
                                context.exposed |> Set.insert info

                            else
                                context.exposed
                      }
                    )
                )
                >> Rule.withDeclarationEnterVisitor
                    linksInDeclaration
            )
        |> (let
                toModule : ProjectContext -> ModuleContext
                toModule context =
                    { exposed = context.exposed
                    , inDeclarationDoc = Set.empty
                    }

                toProject : Rule.ModuleKey -> name_ -> ModuleContext -> ProjectContext
                toProject moduleKey _ { exposed, inDeclarationDoc } =
                    { readme = Nothing
                    , exposed = exposed
                    , inDeclarationDoc =
                        Set.singleton
                            { moduleKey = moduleKey
                            , links = inDeclarationDoc
                            }
                    }

                merge : ProjectContext -> ProjectContext -> ProjectContext
                merge a b =
                    { exposed = Set.union a.exposed b.exposed
                    , inDeclarationDoc =
                        Set.union a.inDeclarationDoc b.inDeclarationDoc
                    , readme =
                        a.readme
                            |> Maybe.map Just
                            |> Maybe.withDefault b.readme
                    }
            in
            Rule.withModuleContext
                { fromProjectToModule = \_ _ -> toModule
                , fromModuleToProject = toProject
                , foldProjectContexts = merge
                }
           )
        |> Rule.withFinalProjectEvaluation
            (\{ readme, exposed, inDeclarationDoc } ->
                let
                    checkLink error match =
                        let
                            { moduleName, kind } =
                                match.parsed

                            moduleNameParts =
                                let
                                    ( first, tail ) =
                                        moduleName
                                in
                                first :: tail
                        in
                        case kind of
                            ModuleLink ->
                                if
                                    exposed
                                        |> Set.toList
                                        |> List.any
                                            (.moduleName
                                                >> (==) moduleNameParts
                                            )
                                then
                                    []

                                else
                                    [ error
                                        { message = moduleInLinkNotExposed
                                        , details = linkPointsToNonExistentMemberDetails
                                        }
                                        match.range
                                    ]

                            DefinitionLink definition ->
                                if
                                    exposed
                                        |> Set.toList
                                        |> List.any
                                            (\m ->
                                                (m.moduleName == moduleNameParts)
                                                    && (m.exposedDefinitions
                                                            |> isExposed definition
                                                       )
                                            )
                                then
                                    []

                                else
                                    [ error
                                        { message = definitionInLinkNotExposedMessage
                                        , details = linkPointsToNonExistentMemberDetails
                                        }
                                        match.range
                                    ]
                in
                [ readme
                    |> Maybe.map
                        (\{ key, links } ->
                            links
                                |> Set.toList
                                |> List.concatMap
                                    (checkLink (Rule.errorForReadme key))
                        )
                    |> Maybe.withDefault []
                , inDeclarationDoc
                    |> Set.toList
                    |> List.concatMap
                        (\{ moduleKey, links } ->
                            links
                                |> Set.toList
                                |> List.concatMap
                                    (checkLink (Rule.errorForModule moduleKey))
                        )
                ]
                    |> List.concat
            )
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { readme :
        Maybe
            { key : Rule.ReadmeKey
            , links :
                Set
                    { parsed : Link
                    , range : Range
                    }
            }
    , inDeclarationDoc :
        Set
            { moduleKey : Rule.ModuleKey
            , links :
                Set
                    { parsed : Link
                    , range : Range
                    }
            }
    , exposed : Set ModuleInfo
    }


type alias ModuleContext =
    { inDeclarationDoc :
        Set
            { parsed : Link
            , range : Range
            }
    , exposed : Set ModuleInfo
    }


linksInDeclaration : Node Declaration -> ModuleContext -> ( List error_, ModuleContext )
linksInDeclaration (Node _ declaration) context =
    ( []
    , { context
        | inDeclarationDoc =
            declaration
                |> docOfDeclaration
                |> Maybe.map
                    (\doc ->
                        doc
                            |> Node.value
                            |> Parser.find linkParser
                            |> List.map
                                (\match ->
                                    { match
                                        | range =
                                            let
                                                inDoc =
                                                    addLocation (Node.range doc).start
                                            in
                                            { start = inDoc match.range.start
                                            , end = inDoc match.range.end
                                            }
                                    }
                                )
                            |> Set.fromList
                            |> Set.union context.inDeclarationDoc
                    )
                |> Maybe.withDefault context.inDeclarationDoc
      }
    )


definitionInLinkNotExposedMessage : String
definitionInLinkNotExposedMessage =
    "The link points to a definition that isn't exposed from any exposed module in this package."


moduleInLinkNotExposed : String
moduleInLinkNotExposed =
    "The link points to a module that isn't listed in \"exposed-modules\"."


linkPointsToNonExistentMemberDetails : List String
linkPointsToNonExistentMemberDetails =
    [ "Links are only useful when they point to exposed package members." ]
