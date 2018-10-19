module OpenAPIHelper
  ( buildInfo
  , buildOpenApi
  , buildPathItem
  ) where

import Data.Maybe (Maybe(..))
import OpenAPI as OpenAPI

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

buildPathItem :: String -> OpenAPI.PathItem
buildPathItem summary =
  { "$ref": Nothing
  , summary: Just summary
  , description: Nothing
  , get: Nothing
  , put: Nothing
  , post: Nothing
  , delete: Nothing
  , options: Nothing
  , head: Nothing
  , patch: Nothing
  , trace: Nothing
  , servers: Nothing
  , parameters: Nothing
  }
