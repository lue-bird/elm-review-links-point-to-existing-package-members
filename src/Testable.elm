module Testable exposing (definitionInLinkNotExposedMessage, linkPointsToNonExistentMemberDetails, moduleInLinkNotExposed)


definitionInLinkNotExposedMessage : String
definitionInLinkNotExposedMessage =
    "The link points to a definition that isn't exposed or doesn't exist"


moduleInLinkNotExposed : String
moduleInLinkNotExposed =
    "The link points to a module that isn't listed in \"exposed-modules\" or doesn't exist"


linkPointsToNonExistentMemberDetails : List String
linkPointsToNonExistentMemberDetails =
    [ "Links should point to exposed package members." ]
