module LinksPointToExistingPackageMembers.NotExposed exposing (definitionInLinkNotExposedMessage, linkPointsToNonExistentMemberDetails, moduleInLinkNotExposed, rule)

import Elm.Module as Module
import Elm.Project as Project exposing (Project)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import EverySet as Set exposing (EverySet)
import JaroWinkler
import ParserExtra as Parser
import Review.Rule as Rule exposing (Rule)
import SyntaxHelp exposing (Link, LinkKind(..), ModuleInfo, addLocation, docOfDeclaration, exposedModules, isExposed, isFileComment, linkParser, moduleInfo, nameOfExpose)


type alias Set a =
    EverySet a



--


rule : Rule
rule =
    Rule.newProjectRuleSchema "LinksPointToExistingPackageMembers"
        { exposed = Set.empty
        , inModules = Set.empty
        , inReadme = Nothing
        }
        |> Rule.withReadmeProjectVisitor
            linksInReadme
        |> Rule.withElmJsonProjectVisitor
            exposedModulesInElmJson
        |> Rule.withModuleVisitor
            (Rule.withModuleDefinitionVisitor
                exposedInModule
                >> Rule.withDeclarationEnterVisitor
                    (\(Node _ declaration) context ->
                        ( []
                        , declaration
                            |> docOfDeclaration
                            |> Maybe.map
                                (\doc ->
                                    { context
                                        | linksInDoc =
                                            Set.union context.linksInDoc
                                                (linksIn doc)
                                    }
                                )
                            |> Maybe.withDefault context
                        )
                    )
                >> Rule.withCommentsVisitor
                    (\comments context ->
                        ( []
                        , comments
                            |> List.filter
                                (isFileComment << Node.value)
                            |> List.head
                            |> Maybe.map
                                (\fileComment ->
                                    { context
                                        | linksInDoc =
                                            Set.union context.linksInDoc
                                                (linksIn fileComment)
                                    }
                                )
                            |> Maybe.withDefault context
                        )
                    )
            )
        |> (let
                toModule : ProjectContext -> ModuleContext
                toModule context =
                    { exposed = context.exposed
                    , linksInDoc = Set.empty
                    }

                toProject :
                    Rule.ModuleKey
                    -> name_
                    -> ModuleContext
                    -> ProjectContext
                toProject moduleKey _ { exposed, linksInDoc } =
                    { inReadme = Nothing
                    , exposed = exposed
                    , inModules =
                        Set.singleton
                            { key = moduleKey
                            , links = linksInDoc
                            }
                    }

                merge : ProjectContext -> ProjectContext -> ProjectContext
                merge a b =
                    { exposed = Set.union a.exposed b.exposed
                    , inModules =
                        Set.union a.inModules b.inModules
                    , inReadme =
                        a.inReadme
                            |> Maybe.map Just
                            |> Maybe.withDefault b.inReadme
                    }
            in
            Rule.withModuleContext
                { fromProjectToModule = \_ _ -> toModule
                , fromModuleToProject = toProject
                , foldProjectContexts = merge
                }
           )
        |> Rule.withFinalProjectEvaluation check
        |> Rule.fromProjectRuleSchema


exposedModulesInElmJson :
    Maybe { elmJsonKey : Rule.ElmJsonKey, project : Project }
    -> ProjectContext
    -> ( List (Rule.Error e_), ProjectContext )
exposedModulesInElmJson maybeProject context =
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
                                        { moduleName =
                                            name |> Module.toString |> String.split "."
                                        , exposedDefinitions = Exposing.Explicit []
                                        }
                                    )

                        Project.Application _ ->
                            Set.empty

                Nothing ->
                    Set.empty
      }
    )


type alias ProjectContext =
    { inReadme :
        Maybe
            { key : Rule.ReadmeKey
            , links :
                Set { parsed : Link, range : Range }
            }
    , inModules :
        Set
            { key : Rule.ModuleKey
            , links :
                Set { parsed : Link, range : Range }
            }
    , exposed : Set ModuleInfo
    }


type alias ModuleContext =
    { linksInDoc :
        Set { parsed : Link, range : Range }
    , exposed : Set ModuleInfo
    }


linksIn : Node String -> Set { parsed : Link, range : Range }
linksIn fileComment =
    fileComment
        |> Node.value
        |> Parser.find linkParser
        |> List.map
            (\match ->
                { match
                    | range =
                        let
                            inComment =
                                addLocation (Node.range fileComment).start
                        in
                        { start = inComment match.range.start
                        , end = inComment match.range.end
                        }
                }
            )
        |> Set.fromList


linksInReadme :
    Maybe { readmeKey : Rule.ReadmeKey, content : String }
    -> ProjectContext
    -> ( List error_, ProjectContext )
linksInReadme maybeReadme context =
    ( []
    , case maybeReadme of
        Just { readmeKey, content } ->
            { context
                | inReadme =
                    { key = readmeKey
                    , links =
                        let
                            at1 =
                                { row = 1, column = 1 }
                        in
                        linksIn
                            (Node { start = at1, end = at1 }
                                content
                            )
                    }
                        |> Just
            }

        Nothing ->
            context
    )


exposedInModule :
    Node Module
    -> ModuleContext
    -> ( List error_, ModuleContext )
exposedInModule (Node _ module_) context =
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


check : ProjectContext -> List (Rule.Error e_)
check { inReadme, exposed, inModules } =
    let
        exposedMembers : Set String
        exposedMembers =
            exposed
                |> Set.toList
                |> List.concatMap
                    (\{ moduleName, exposedDefinitions } ->
                        let
                            moduleNameString =
                                moduleName |> String.join "."
                        in
                        case exposedDefinitions of
                            Exposing.Explicit exposedDefs ->
                                exposedDefs
                                    |> List.map
                                        (\(Node _ def) ->
                                            moduleNameString ++ "." ++ nameOfExpose def
                                        )
                                    |> (::) moduleNameString

                            Exposing.All _ ->
                                -- we don't suggest stuff from exposing (..)
                                [ moduleNameString ]
                    )
                |> Set.fromList

        details moduleNameParts =
            linkPointsToNonExistentMemberDetails
                { exposed = exposedMembers |> Set.toList
                , badLink = moduleNameParts |> String.join "."
                }

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
                            , details = details moduleNameParts
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
                            , details = details moduleNameParts
                            }
                            match.range
                        ]
    in
    [ inReadme
        |> Maybe.map
            (\{ key, links } ->
                links
                    |> Set.toList
                    |> List.concatMap
                        (checkLink (Rule.errorForReadme key))
            )
        |> Maybe.withDefault []
    , inModules
        |> Set.toList
        |> List.concatMap
            (\{ key, links } ->
                links
                    |> Set.toList
                    |> List.concatMap
                        (checkLink (Rule.errorForModule key))
            )
    ]
        |> List.concat


definitionInLinkNotExposedMessage : String
definitionInLinkNotExposedMessage =
    "The link points to a definition that isn't exposed from any exposed module in this package."


moduleInLinkNotExposed : String
moduleInLinkNotExposed =
    "The link points to a module that isn't listed in \"exposed-modules\"."


linkPointsToNonExistentMemberDetails :
    { exposed : List String, badLink : String }
    -> List String
linkPointsToNonExistentMemberDetails { exposed, badLink } =
    [ "Links are only useful when they point to exposed package members."
    , "Maybe you meant one of those: "
        ++ (exposed
                |> List.sortBy (JaroWinkler.similarity badLink)
                |> List.take 4
                |> String.join ", "
           )
    ]
