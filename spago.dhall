{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "yzrh"
, dependencies =
    [ "bouzuya-command-line-option-parser"
    , "bouzuya-http-method"
    , "bouzuya-http-status-code"
    , "node-fs"
    , "node-process"
    , "prelude"
    , "psci-support"
    , "simple-json"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
