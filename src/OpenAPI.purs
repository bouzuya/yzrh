module OpenAPI where

import Data.Maybe (Maybe)
import Foreign (Foreign)
import Foreign.Object (Object)
import Type.Data.Boolean (kind Boolean)

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#openapi-object
type OpenAPI =
  { openapi :: String
  , info :: Info
  , servers :: Maybe (Array Server)
  , paths :: Paths
  , components :: Maybe Components
  , security :: Maybe (Array SecurityRequirement)
  , tags :: Maybe (Array Tag)
  , externalDocs :: Maybe ExternalDocumentation
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#info-object
type Info =
  { title :: String
  , description :: Maybe String
  , termsOfService :: Maybe String
  , contact :: Maybe Contact
  , license :: Maybe License
  , version :: String
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#contact-object
type Contact =
  { name :: String
  , url :: String
  , email :: String
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#license-object
type License =
  { name :: String
  , url :: Maybe String
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#server-object
type Server =
  { url :: String
  , description :: Maybe String
  , variables :: Maybe (Object ServerVariable)
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#server-variable-object
type ServerVariable =
  { enum :: Maybe (Array String)
  , default :: String
  , description :: Maybe String
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#components-object
type Components =
  { schemas :: Maybe (Object Schema) -- TODO: | Reference Object
  , responses :: Maybe (Object Response) -- TODO: | Reference Object
  , parameters :: Maybe (Object Parameter) -- TODO: | Reference Object
  , examples :: Maybe (Object Example) -- TODO: | Reference Object
  , requestBodies :: Maybe (Object RequestBody) -- TODO: | Reference Object
  , headers :: Maybe (Object Header) -- TODO: | Reference Object
  , securitySchemes :: Maybe (Object SecurityScheme) -- TODO: | Reference Object
  , links :: Maybe (Object Link) -- TODO: | Reference Object
  -- , callback :: Maybe (Object Callback) -- TODO: | Reference Object
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#paths-object
type Paths = Object PathItem

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#path-item-object
type PathItem =
  { "$ref" :: Maybe String
  , summary :: Maybe String
  , description :: Maybe String
  , get :: Maybe Operation
  , put :: Maybe Operation
  , post :: Maybe Operation
  , delete :: Maybe Operation
  , options :: Maybe Operation
  , head :: Maybe Operation
  , patch :: Maybe Operation
  , trace :: Maybe Operation
  , servers :: Maybe (Array Server)
  , parameters :: Maybe (Array Parameter) -- TODO: | Reference Object
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#operation-object
type Operation =
  { tags :: Maybe (Array String)
  , summary :: Maybe String
  , description :: Maybe String
  , externalDocs :: Maybe ExternalDocumentation
  , operationId :: Maybe String
  , parameters :: Maybe (Array Parameter) -- TODO: | Reference Object
  , requestBody :: Maybe RequestBody -- TODO: | Reference Object
  , responses :: Responses
  -- , callbacks :: Maybe (Object Callback) -- TODO: | Reference Object
  , deprecated :: Maybe Boolean
  , security :: Maybe (Array SecurityRequirement) -- TODO: | Reference Object
  , servers :: Maybe (Array Server)
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#external-documentation-object
type ExternalDocumentation =
  { description :: Maybe String
  , url :: String
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#parameter-object
-- TODO
type Parameter =
  { name :: String
  , in :: String
  , description :: Maybe String
  , required :: Maybe Boolean
  , deprecated :: Maybe Boolean
  , allowEmptyValue :: Maybe Boolean
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#request-body-object
type RequestBody =
  { description :: Maybe String
  , content :: Object MediaType
  , required :: Maybe Boolean
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#media-type-object
type MediaType =
  { schema :: Maybe Schema -- TODO: | Reference Object
  , example :: Foreign -- TODO: any
  , examples :: Maybe (Object Example) -- TODO: | Reference Object
  , encoding :: Maybe (Object Encoding)
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#encoding-object
type Encoding =
  { contentType :: Maybe String
  , headers :: Maybe (Object Header) -- TODO: | Reference Object
  , style :: Maybe String
  , explode :: Maybe Boolean
  , allowReserved :: Maybe Boolean
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#responses-object
type Responses = Object Response

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#response-object
type Response =
  { description :: String
  , headers :: Maybe (Object Header) -- TODO: | Reference Object
  , content :: Maybe (Object MediaType)
  , links :: Maybe (Object Link) -- TODO: | Reference Object
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#callback-object
-- type Callback = Object PathItem -- TODO:

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#example-object
type Example =
  { summary :: Maybe String
  , description :: Maybe String
  , value :: Maybe Foreign -- TODO: any
  , externalValue :: Maybe String
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#link-object
type Link =
  { operationRef :: String
  , operationId :: String
  , parameters :: Object String -- TODO: any | {expression}
  , requestBody :: String -- TODO: any | {expression}
  , description :: String
  , server :: Server
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#header-object
type Header = {} -- TODO:

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#tag-object
type Tag =
  { name :: String
  , description :: Maybe String
  , externalDocs :: Maybe ExternalDocumentation
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#reference-object
type Reference =
  { "$ref" :: String
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#schema-object
type Schema = {} -- TODO:

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#discriminator-object
type Discriminator = {} -- TODO:

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#xml-object
type Xml = {} -- TODO:

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#security-scheme-object
type SecurityScheme = {} -- TODO:

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#oauthFlowsObject
type OAuthFlows =
  { implicit :: Maybe OAuthFlow
  , password :: Maybe OAuthFlow
  , clientCredentials :: Maybe OAuthFlow
  , authorizationCode :: Maybe OAuthFlow
  }

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#oauth-flow-object
type OAuthFlow = {} -- TODO:

-- https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md#security-requirement-object
type SecurityRequirement = {} -- TODO:
