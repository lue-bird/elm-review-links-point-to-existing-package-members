module LinksPointToExistingPackageMembersTest exposing (all)

import Elm.Project
import Json.Decode
import LinksPointToExistingPackageMembers exposing (rule)
import LinksPointToExistingPackageMembers.NotExposed exposing (definitionInLinkNotExposedMessage, linkPointsToNonExistentMemberDetails, moduleInLinkNotExposed)
import Review.Project as Project
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "LinksPointToExistingPackageMembers"
        [ describe "succeeds"
            [ test "exposed function"
                (\() ->
                    """module A.And.B exposing (a, b)

b =
    "b"

{-| Not [`b`](A-And-B#b).
-}
a =
    "a"
"""
                        |> Review.Test.runWithProjectData
                            (Project.new
                                |> Project.addElmJson
                                    (elmJson { exposedModules = [ "A.And.B" ] })
                            )
                            rule
                        |> Review.Test.expectNoErrors
                )
            , test "exposed alias"
                (\() ->
                    """module A.And.B exposing (a, B)

type alias B =
    B

{-| Not [`B`](A-And-B#B).
-}
a =
    "a"
"""
                        |> Review.Test.runWithProjectData
                            (Project.new
                                |> Project.addElmJson
                                    (elmJson { exposedModules = [ "A.And.B" ] })
                            )
                            rule
                        |> Review.Test.expectNoErrors
                )
            , test "definition exposed in different module"
                (\() ->
                    [ """module A exposing (a)

{-| Not [`b`](B#b).
-}
a =
    "a"
"""
                    , """module B exposing (b)

b =
    "b"
"""
                    ]
                        |> Review.Test.runOnModulesWithProjectData
                            (Project.new
                                |> Project.addElmJson
                                    (elmJson { exposedModules = [ "A", "B" ] })
                            )
                            rule
                        |> Review.Test.expectNoErrors
                )
            ]
        , describe "fails"
            [ describe "definition link"
                [ test "because it isn't exposed"
                    (\() ->
                        """module A.And.B exposing (a)

b =
    "b"

{-| Not [`b`](A-And-B#b).
-}
a =
    "a"
"""
                            |> Review.Test.runWithProjectData
                                (Project.new
                                    |> Project.addElmJson
                                        (elmJson { exposedModules = [ "A.And.B" ] })
                                )
                                rule
                            |> Review.Test.expectErrors
                                [ Review.Test.error
                                    { message = definitionInLinkNotExposedMessage
                                    , details = linkPointsToNonExistentMemberDetails
                                    , under = "[`b`](A-And-B#b)"
                                    }
                                ]
                    )
                , test "because its module isn't in exposed-modules"
                    (\() ->
                        """module A.And.B exposing (a)

b =
    "b"

{-| Not [`b`](A-And-B#b).
-}
a =
    "a"
"""
                            |> Review.Test.runWithProjectData
                                (Project.new
                                    |> Project.addElmJson
                                        (elmJson { exposedModules = [] })
                                )
                                rule
                            |> Review.Test.expectErrors
                                [ Review.Test.error
                                    { message = definitionInLinkNotExposedMessage
                                    , details = linkPointsToNonExistentMemberDetails
                                    , under = "[`b`](A-And-B#b)"
                                    }
                                ]
                    )
                , test "in readme"
                    (\() ->
                        """module A exposing (a)

a =
    "a"
"""
                            |> Review.Test.runWithProjectData
                                (Project.new
                                    |> Project.addElmJson
                                        (elmJson { exposedModules = [ "A" ] })
                                    |> Project.addReadme
                                        { path = "README.md"
                                        , content = "See [`B`](B)."
                                        }
                                )
                                rule
                            |> Review.Test.expectErrorsForReadme
                                [ Review.Test.error
                                    { message = moduleInLinkNotExposed
                                    , details = linkPointsToNonExistentMemberDetails
                                    , under = "[`B`](B)"
                                    }
                                ]
                    )
                ]
            , test "module link because it isn't in exposed-modules"
                (\() ->
                    """module A.And.B exposing (a, b)

b =
    "b"

{-| Not [`A.And.B`](A-And-B).
-}
a =
    "a"
"""
                        |> Review.Test.runWithProjectData
                            (Project.new
                                |> Project.addElmJson
                                    (elmJson { exposedModules = [] })
                            )
                            rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = moduleInLinkNotExposed
                                , details = linkPointsToNonExistentMemberDetails
                                , under = "[`A.And.B`](A-And-B)"
                                }
                            ]
                )
            ]
        ]


createElmJson :
    String
    -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    case Json.Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok project ->
            { path = "elm.json"
            , raw = rawElmJson
            , project = project
            }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


elmJson :
    { exposedModules : List String }
    -> { path : String, raw : String, project : Elm.Project.Project }
elmJson { exposedModules } =
    """
{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [

        """
        ++ (exposedModules
                |> List.map (\s -> "\"" ++ s ++ "\"")
                |> String.join ",\n        "
           )
        ++ """

    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {
    }
}"""
        |> createElmJson
