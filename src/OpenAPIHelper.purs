module OpenAPIHelper
  ( buildInfo
  , buildOpenApi
  , buildOperation
  , buildPathItem
  ) where

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import OpenAPI as OpenAPI
import Prelude (map)

buildInfo :: String -> String -> OpenAPI.Info
buildInfo title version =
  { title
  , description: Nothing
  , termsOfService: Nothing
  , contact: Nothing
  , license: Nothing
  , version
  }

buildOpenApi :: OpenAPI.Info -> OpenAPI.Paths -> OpenAPI.OpenAPI
buildOpenApi info paths =
  { openapi: "3.0.1"
  , info
  , servers: Nothing
  , paths
  , components: Nothing
  , security: Nothing
  , tags: Nothing
  , externalDocs: Nothing
  }

type Method = String

buildOperation :: Array String -> OpenAPI.Operation
buildOperation pathParams =
  { tags: Nothing
  , summary: Nothing
  , description: Nothing
  , externalDocs: Nothing
  , operationId: Nothing
  , parameters:
    Just
    ( map
        (\name ->
          { name
          , in: "path" -- TODO
          , description: Nothing
          , required: Just true
          , deprecated: Nothing
          , allowEmptyValue: Just false
          }
        )
        pathParams
    )
  , requestBody: Nothing
  , responses: -- TODO
    Object.fromFoldable
      [ Tuple
          "default"
          { description: "default"
          , headers: Nothing
          , content: Nothing
          , links: Nothing
          }
      ]
  , deprecated: Nothing
  , security: Nothing
  , servers: Nothing
  }

buildPathItem :: String -> Map Method OpenAPI.Operation -> OpenAPI.PathItem
buildPathItem summary operationMap =
  { "$ref": Nothing
  , summary: Just summary
  , description: Nothing
  , get: Map.lookup "get" operationMap
  , put: Map.lookup "put" operationMap
  , post: Map.lookup "post" operationMap
  , delete: Map.lookup "delete" operationMap
  , options: Map.lookup "options" operationMap
  , head: Map.lookup "head" operationMap
  , patch: Map.lookup "patch" operationMap
  , trace: Map.lookup "trace" operationMap
  , servers: Nothing
  , parameters: Nothing
  }
