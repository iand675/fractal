-- | OpenAPI 3.x types
module Fractal.OpenApi.Types
  ( -- * Core Types
    OpenApiSpec(..)
  , OpenApiVersion(..)
  , ReferenceOr(..)
  
    -- * Document Structure
  , Info(..)
  , Contact(..)
  , License(..)
  , Server(..)
  , ServerVariable(..)
  
    -- * Paths and Operations
  , Paths
  , PathItem(..)
  , Operation(..)
  , Parameter(..)
  , ParameterLocation(..)
  , RequestBody(..)
  , Response(..)
  , Responses
  
    -- * Components
  , Components(..)
  , SecurityScheme(..)
  , SecurityRequirement
  
    -- * Media Types
  , MediaType(..)
  , Encoding(..)
  
    -- * Additional Types  
  , Discriminator(..)
  , ExternalDocs(..)
  , Tag(..)
  ) where

import Fractal.JsonSchema.Types (Schema)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value)
import GHC.Generics (Generic)

-- | OpenAPI/Swagger specification version
data OpenApiVersion
  = Swagger20 Text  -- ^ Swagger 2.0 (legacy)
  | OpenApi30 Text  -- ^ OpenAPI 3.0.x
  | OpenApi31 Text  -- ^ OpenAPI 3.1.x
  deriving (Eq, Show, Ord, Generic)

-- | Either a reference or an inline value
-- Common pattern in OpenAPI for reusable components
data ReferenceOr a
  = Ref Text
    -- ^ $ref pointer to component
  | Inline a
    -- ^ Inline definition
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

-- | Complete OpenAPI specification
-- Supports both Swagger 2.0 and OpenAPI 3.x
data OpenApiSpec = OpenApiSpec
  { specOpenApi :: Text
    -- ^ Version string (e.g., "2.0", "3.0.0", "3.1.0")
    -- For Swagger 2.0, this is the "swagger" field value
  
  , specInfo :: Info
    -- ^ Metadata about the API
  
  , specServers :: Maybe [Server]
    -- ^ Server connectivity (OpenAPI 3.x)
    -- For Swagger 2.0, derived from host/basePath/schemes
  
  , specHost :: Maybe Text
    -- ^ Host (Swagger 2.0 only)
  
  , specBasePath :: Maybe Text
    -- ^ Base path (Swagger 2.0 only)
  
  , specSchemes :: Maybe [Text]
    -- ^ URL schemes (Swagger 2.0 only)
  
  , specPaths :: Paths
    -- ^ Available paths and operations
  
  , specComponents :: Maybe Components
    -- ^ Reusable components (OpenAPI 3.x)
    -- For Swagger 2.0, this is "definitions"
  
  , specDefinitions :: Maybe (Map Text Schema)
    -- ^ Schema definitions (Swagger 2.0 only)
  
  , specSecurity :: Maybe [SecurityRequirement]
    -- ^ Global security requirements
  
  , specTags :: Maybe [Tag]
    -- ^ List of tags with metadata
  
  , specExternalDocs :: Maybe ExternalDocs
    -- ^ External documentation
  }
  deriving (Eq, Show, Generic)

-- | API metadata
data Info = Info
  { infoTitle :: Text
  , infoDescription :: Maybe Text
  , infoTermsOfService :: Maybe Text
  , infoContact :: Maybe Contact
  , infoLicense :: Maybe License
  , infoVersion :: Text
  }
  deriving (Eq, Show, Generic)

-- | Contact information
data Contact = Contact
  { contactName :: Maybe Text
  , contactUrl :: Maybe Text
  , contactEmail :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | License information
data License = License
  { licenseName :: Text
  , licenseUrl :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | Server information
data Server = Server
  { serverUrl :: Text
  , serverDescription :: Maybe Text
  , serverVariables :: Maybe (Map Text ServerVariable)
  }
  deriving (Eq, Show, Generic)

-- | Server variable for URL templating
data ServerVariable = ServerVariable
  { serverVarEnum :: Maybe [Text]
  , serverVarDefault :: Text
  , serverVarDescription :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | Map of path patterns to PathItems
type Paths = Map Text PathItem

-- | Describes operations available on a single path
data PathItem = PathItem
  { pathSummary :: Maybe Text
  , pathDescription :: Maybe Text
  , pathGet :: Maybe Operation
  , pathPut :: Maybe Operation
  , pathPost :: Maybe Operation
  , pathDelete :: Maybe Operation
  , pathOptions :: Maybe Operation
  , pathHead :: Maybe Operation
  , pathPatch :: Maybe Operation
  , pathTrace :: Maybe Operation
  , pathServers :: Maybe [Server]
  , pathParameters :: Maybe [ReferenceOr Parameter]
  }
  deriving (Eq, Show, Generic)

-- | Describes a single API operation
data Operation = Operation
  { operationTags :: Maybe [Text]
  , operationSummary :: Maybe Text
  , operationDescription :: Maybe Text
  , operationExternalDocs :: Maybe ExternalDocs
  , operationOperationId :: Maybe Text
  , operationParameters :: Maybe [ReferenceOr Parameter]
  , operationRequestBody :: Maybe (ReferenceOr RequestBody)
  , operationResponses :: Responses
  , operationCallbacks :: Maybe (Map Text Value)  -- Simplified for now
  , operationDeprecated :: Maybe Bool
  , operationSecurity :: Maybe [SecurityRequirement]
  , operationServers :: Maybe [Server]
  }
  deriving (Eq, Show, Generic)

-- | Parameter location
data ParameterLocation
  = QueryParam
  | HeaderParam
  | PathParam
  | CookieParam
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- | Operation parameter
data Parameter = Parameter
  { parameterName :: Text
  , parameterIn :: ParameterLocation
  , parameterDescription :: Maybe Text
  , parameterRequired :: Maybe Bool
  , parameterDeprecated :: Maybe Bool
  , parameterAllowEmptyValue :: Maybe Bool
  , parameterSchema :: Maybe (ReferenceOr Schema)
  , parameterStyle :: Maybe Text
  , parameterExplode :: Maybe Bool
  , parameterAllowReserved :: Maybe Bool
  , parameterExample :: Maybe Value
  , parameterExamples :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Generic)

-- | Request body
data RequestBody = RequestBody
  { requestBodyDescription :: Maybe Text
  , requestBodyContent :: Map Text MediaType
  , requestBodyRequired :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

-- | Map of status codes to Response objects
type Responses = Map Text (ReferenceOr Response)

-- | API response
data Response = Response
  { responseDescription :: Text
  , responseHeaders :: Maybe (Map Text Value)  -- Simplified
  , responseContent :: Maybe (Map Text MediaType)
  , responseLinks :: Maybe (Map Text Value)  -- Simplified
  }
  deriving (Eq, Show, Generic)

-- | Media type definition
data MediaType = MediaType
  { mediaTypeSchema :: Maybe (ReferenceOr Schema)
  , mediaTypeExample :: Maybe Value
  , mediaTypeExamples :: Maybe (Map Text Value)
  , mediaTypeEncoding :: Maybe (Map Text Encoding)
  }
  deriving (Eq, Show, Generic)

-- | Encoding definition
data Encoding = Encoding
  { encodingContentType :: Maybe Text
  , encodingHeaders :: Maybe (Map Text Value)  -- Simplified
  , encodingStyle :: Maybe Text
  , encodingExplode :: Maybe Bool
  , encodingAllowReserved :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

-- | Reusable components
data Components = Components
  { componentsSchemas :: Maybe (Map Text Schema)
  , componentsResponses :: Maybe (Map Text Response)
  , componentsParameters :: Maybe (Map Text Parameter)
  , componentsExamples :: Maybe (Map Text Value)
  , componentsRequestBodies :: Maybe (Map Text RequestBody)
  , componentsHeaders :: Maybe (Map Text Value)
  , componentsSecuritySchemes :: Maybe (Map Text SecurityScheme)
  , componentsLinks :: Maybe (Map Text Value)
  , componentsCallbacks :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Generic)

-- | Security scheme definition
data SecurityScheme = SecurityScheme
  { securitySchemeType :: Text
  , securitySchemeDescription :: Maybe Text
  , securitySchemeName :: Maybe Text
  , securitySchemeIn :: Maybe Text
  , securitySchemeScheme :: Maybe Text
  , securitySchemeBearerFormat :: Maybe Text
  , securitySchemeFlows :: Maybe Value  -- OAuth flows, simplified
  , securitySchemeOpenIdConnectUrl :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | Security requirement (scope requirements for schemes)
type SecurityRequirement = Map Text [Text]

-- | External documentation
data ExternalDocs = ExternalDocs
  { externalDocsDescription :: Maybe Text
  , externalDocsUrl :: Text
  }
  deriving (Eq, Show, Generic)

-- | Tag metadata
data Tag = Tag
  { tagName :: Text
  , tagDescription :: Maybe Text
  , tagExternalDocs :: Maybe ExternalDocs
  }
  deriving (Eq, Show, Generic)

-- | Discriminator for polymorphic schemas
data Discriminator = Discriminator
  { discriminatorPropertyName :: Text
  , discriminatorMapping :: Maybe (Map Text Text)
  }
  deriving (Eq, Show, Generic)
