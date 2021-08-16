module Testable exposing (definitionInLinkNotExposedMessage, linkPointsToNonExistentMemberDetails, moduleInLinkNotExposed)


definitionInLinkNotExposedMessage : String
definitionInLinkNotExposedMessage =
    "The link points to a definition that isn't exposed. This could also mean that it or its module doesn't exist. It could also be in a module that the package doesn't expose."


moduleInLinkNotExposed : String
moduleInLinkNotExposed =
    "The link points to a module that isn't listed in \"exposed-modules\" or that doesn't exist."


linkPointsToNonExistentMemberDetails : List String
linkPointsToNonExistentMemberDetails =
    [ "Links should point to exposed package members." ]
