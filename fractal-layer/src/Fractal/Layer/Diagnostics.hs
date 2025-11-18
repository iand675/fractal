{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Fractal.Layer.Diagnostics
-- Description : Diagnostics and visualization for layer initialization
-- Stability   : experimental
-- Portability : Portable
--
-- This module provides tools for visualizing and debugging layer initialization.
-- It can track how layers are composed, which services are shared, and render
-- the dependency tree in various formats.
--
-- == Example Usage
--
-- @
-- appLayer :: Layer IO () (Config, Database, WebServer)
-- appLayer = configLayer >>> (dbLayer &&& webLayer)
--
-- main :: IO ()
-- main = withLayerDiagnostics appLayer () $ \(env, diags) -> do
--   -- Print ASCII tree
--   putStrLn $ renderLayerTree diags
--
--   -- Or export as JSON
--   BSL.writeFile "layer-tree.json" (encode diags)
--
--   -- Use the environment
--   let (cfg, db, ws) = env
--   serveRequests ws
-- @
module Fractal.Layer.Diagnostics
  ( -- * Diagnostics Types
    LayerDiagnostics (..),
    LayerNode (..),
    LayerNodeType (..),
    ResourceStatus (..),

    -- * Running with Diagnostics
    withLayerDiagnostics,
    buildLayerDiagnostics,

    -- * Rendering
    renderLayerTree,
    renderLayerTreeCompact,
    renderLayerTreeDetailed,

    -- * JSON Export
    diagnosticsToJSON,
  )
where

import Control.Monad.IO.Class
import Data.Aeson (ToJSON (..), object, (.=), Value)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Complete diagnostics for a layer initialization
data LayerDiagnostics = LayerDiagnostics
  { rootNode :: LayerNode
  -- ^ The root of the initialization tree
  , totalDuration :: Double
  -- ^ Total time taken to initialize (seconds)
  , totalResources :: Int
  -- ^ Total number of resources allocated
  , sharedResources :: Int
  -- ^ Number of resources that were shared/cached
  }
  deriving (Show, Generic)

-- | A node in the layer initialization tree
data LayerNode = LayerNode
  { nodeId :: Text
  -- ^ Unique identifier for this node
  , nodeName :: Text
  -- ^ Human-readable name (e.g., "DatabaseLayer", "ConfigService")
  , nodeType :: LayerNodeType
  -- ^ Type of layer node
  , resourceType :: Maybe TypeRep
  -- ^ The type of resource produced (if available)
  , status :: ResourceStatus
  -- ^ Status of this resource
  , duration :: Maybe Double
  -- ^ Time taken to initialize this layer (seconds)
  , children :: [LayerNode]
  -- ^ Child layers
  , metadata :: HashMap Text Text
  -- ^ Additional metadata
  }
  deriving (Show, Generic)

-- | The type of layer node
data LayerNodeType
  = ResourceNode
  -- ^ A resource that was allocated
  | EffectNode
  -- ^ An effect that was run
  | ServiceNode
  -- ^ A cached service
  | ComposedNode
  -- ^ A composed layer (>>> or &&&)
  | ParallelNode
  -- ^ Parallel composition (&&&)
  | SequentialNode
  -- ^ Sequential composition (>>>)
  deriving (Show, Eq, Generic)

-- | Status of a resource
data ResourceStatus
  = Initializing
  -- ^ Currently being initialized
  | Initialized
  -- ^ Successfully initialized
  | Failed String
  -- ^ Failed to initialize with error
  | SharedReference Text
  -- ^ Reference to a shared resource (with node ID)
  deriving (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- JSON Instances
-------------------------------------------------------------------------------

instance ToJSON LayerDiagnostics where
  toJSON LayerDiagnostics{..} =
    object
      [ "root" .= rootNode
      , "totalDuration" .= totalDuration
      , "totalResources" .= totalResources
      , "sharedResources" .= sharedResources
      ]

instance ToJSON LayerNode where
  toJSON LayerNode{..} =
    object
      [ "id" .= nodeId
      , "name" .= nodeName
      , "type" .= nodeType
      , "resourceType" .= (show <$> resourceType)
      , "status" .= status
      , "duration" .= duration
      , "children" .= children
      , "metadata" .= metadata
      ]

instance ToJSON LayerNodeType where
  toJSON = \case
    ResourceNode -> "resource"
    EffectNode -> "effect"
    ServiceNode -> "service"
    ComposedNode -> "composed"
    ParallelNode -> "parallel"
    SequentialNode -> "sequential"

instance ToJSON ResourceStatus where
  toJSON = \case
    Initializing -> object ["status" .= ("initializing" :: Text)]
    Initialized -> object ["status" .= ("initialized" :: Text)]
    Failed err -> object ["status" .= ("failed" :: Text), "error" .= err]
    SharedReference nodeId -> object ["status" .= ("shared" :: Text), "reference" .= nodeId]

-------------------------------------------------------------------------------
-- Running with Diagnostics
-------------------------------------------------------------------------------

-- | Build a layer with diagnostics tracking enabled.
-- This is a placeholder implementation - the actual implementation would
-- require modifications to the Layer type itself to collect this information.
buildLayerDiagnostics :: MonadIO m => m LayerDiagnostics
buildLayerDiagnostics = liftIO $ do
  -- This is a simplified placeholder
  -- In a real implementation, we'd instrument the Layer monad
  pure $ LayerDiagnostics
    { rootNode = placeholderNode
    , totalDuration = 0.0
    , totalResources = 0
    , sharedResources = 0
    }
  where
    placeholderNode = LayerNode
      { nodeId = "root"
      , nodeName = "Root"
      , nodeType = ComposedNode
      , resourceType = Nothing
      , status = Initialized
      , duration = Nothing
      , children = []
      , metadata = HashMap.empty
      }

-- | Run a layer with diagnostics enabled.
-- Returns both the environment and the diagnostics information.
--
-- Note: This is a placeholder signature. The actual implementation would need
-- to be integrated into the Layer module itself.
withLayerDiagnostics ::
  MonadIO m =>
  -- | Layer to run (would be actual Layer type)
  a ->
  -- | Dependencies
  deps ->
  -- | Action with environment and diagnostics
  ((env, LayerDiagnostics) -> m r) ->
  m r
withLayerDiagnostics _layer _deps action = do
  diags <- buildLayerDiagnostics
  -- Placeholder - actual implementation would build the layer
  -- and collect diagnostics during initialization
  let env = undefined -- Would be the actual environment
  action (env, diags)

-------------------------------------------------------------------------------
-- Rendering to Terminal
-------------------------------------------------------------------------------

-- | Render the layer tree as ASCII art (compact version)
--
-- Example output:
-- @
-- Root
-- ├── ConfigLayer (0.05s)
-- └── ParallelComposition (0.3s)
--     ├── DatabaseLayer (0.2s)
--     │   └── ConnectionPool [SHARED: svc-001]
--     └── WebServerLayer (0.1s)
--         └── MetricsCollector [SHARED: svc-001]
-- @
renderLayerTree :: LayerDiagnostics -> String
renderLayerTree diags =
  unlines $
    [ "Layer Initialization Tree"
    , "========================="
    , ""
    , "Total Duration: " ++ show (totalDuration diags) ++ "s"
    , "Total Resources: " ++ show (totalResources diags)
    , "Shared Resources: " ++ show (sharedResources diags)
    , ""
    ] ++ renderNode "" True (rootNode diags)

-- | Compact rendering without metadata
renderLayerTreeCompact :: LayerDiagnostics -> String
renderLayerTreeCompact = renderLayerTree

-- | Detailed rendering with metadata
renderLayerTreeDetailed :: LayerDiagnostics -> String
renderLayerTreeDetailed diags =
  unlines $
    [ "Layer Initialization Tree (Detailed)"
    , "====================================="
    , ""
    , "Total Duration: " ++ show (totalDuration diags) ++ "s"
    , "Total Resources: " ++ show (totalResources diags)
    , "Shared Resources: " ++ show (sharedResources diags)
    , ""
    ] ++ renderNodeDetailed "" True (rootNode diags)

-- | Render a single node in the tree
renderNode :: String -> Bool -> LayerNode -> [String]
renderNode prefix isLast node =
  let connector = if isLast then "└── " else "├── "
      extension = if isLast then "    " else "│   "
      nodeInfo = T.unpack (nodeName node) ++ formatDuration (duration node) ++ formatStatus (status node)
      header = prefix ++ connector ++ nodeInfo
      childPrefix = prefix ++ extension
      childCount = length (children node)
      childLines = concat $ zipWith (\i child -> renderNode childPrefix (i == childCount - 1) child) [1..] (children node)
  in header : childLines

-- | Render a node with detailed metadata
renderNodeDetailed :: String -> Bool -> LayerNode -> [String]
renderNodeDetailed prefix isLast node =
  let connector = if isLast then "└── " else "├── "
      extension = if isLast then "    " else "│   "
      nodeInfo = T.unpack (nodeName node) ++ " [" ++ show (nodeType node) ++ "]"
      header = prefix ++ connector ++ nodeInfo
      childPrefix = prefix ++ extension

      -- Add metadata lines
      metaLines = if HashMap.null (metadata node)
        then []
        else map (\(k, v) -> childPrefix ++ "  • " ++ T.unpack k ++ ": " ++ T.unpack v) (HashMap.toList $ metadata node)

      -- Add type info if available
      typeLines = case resourceType node of
        Nothing -> []
        Just tr -> [childPrefix ++ "  Type: " ++ show tr]

      -- Add duration
      durationLines = case duration node of
        Nothing -> []
        Just d -> [childPrefix ++ "  Duration: " ++ show d ++ "s"]

      -- Add status
      statusLines = [childPrefix ++ "  Status: " ++ formatStatusDetailed (status node)]

      detailLines = typeLines ++ durationLines ++ statusLines ++ metaLines

      childCount = length (children node)
      childLines = concat $ zipWith (\i child -> renderNodeDetailed childPrefix (i == childCount - 1) child) [1..] (children node)
  in (header : detailLines) ++ childLines

formatDuration :: Maybe Double -> String
formatDuration Nothing = ""
formatDuration (Just d) = " (" ++ show d ++ "s)"

formatStatus :: ResourceStatus -> String
formatStatus Initializing = " [INITIALIZING]"
formatStatus Initialized = ""
formatStatus (Failed err) = " [FAILED: " ++ err ++ "]"
formatStatus (SharedReference refId) = " [SHARED: " ++ T.unpack refId ++ "]"

formatStatusDetailed :: ResourceStatus -> String
formatStatusDetailed Initializing = "Initializing"
formatStatusDetailed Initialized = "Initialized"
formatStatusDetailed (Failed err) = "Failed - " ++ err
formatStatusDetailed (SharedReference refId) = "Shared reference to " ++ T.unpack refId

-------------------------------------------------------------------------------
-- JSON Export
-------------------------------------------------------------------------------

-- | Convert diagnostics to a JSON Value
diagnosticsToJSON :: LayerDiagnostics -> Value
diagnosticsToJSON = toJSON

-------------------------------------------------------------------------------
-- Example Usage
-------------------------------------------------------------------------------

-- | Example diagnostics for documentation
exampleDiagnostics :: LayerDiagnostics
exampleDiagnostics = LayerDiagnostics
  { rootNode = LayerNode
      { nodeId = "root"
      , nodeName = "ApplicationLayer"
      , nodeType = SequentialNode
      , resourceType = Nothing
      , status = Initialized
      , duration = Just 0.35
      , children =
          [ LayerNode
              { nodeId = "node-1"
              , nodeName = "ConfigLayer"
              , nodeType = EffectNode
              , resourceType = Just (typeRep (Proxy :: Proxy ()))
              , status = Initialized
              , duration = Just 0.05
              , children = []
              , metadata = HashMap.fromList [("source", "environment")]
              }
          , LayerNode
              { nodeId = "node-2"
              , nodeName = "ParallelServices"
              , nodeType = ParallelNode
              , resourceType = Nothing
              , status = Initialized
              , duration = Just 0.3
              , children =
                  [ LayerNode
                      { nodeId = "node-3"
                      , nodeName = "DatabaseLayer"
                      , nodeType = ResourceNode
                      , resourceType = Nothing
                      , status = Initialized
                      , duration = Just 0.2
                      , children =
                          [ LayerNode
                              { nodeId = "svc-001"
                              , nodeName = "ConnectionPool"
                              , nodeType = ServiceNode
                              , resourceType = Nothing
                              , status = Initialized
                              , duration = Just 0.15
                              , children = []
                              , metadata = HashMap.fromList [("poolSize", "10")]
                              }
                          ]
                      , metadata = HashMap.empty
                      }
                  , LayerNode
                      { nodeId = "node-4"
                      , nodeName = "WebServerLayer"
                      , nodeType = ResourceNode
                      , resourceType = Nothing
                      , status = Initialized
                      , duration = Just 0.1
                      , children =
                          [ LayerNode
                              { nodeId = "node-5"
                              , nodeName = "MetricsCollector"
                              , nodeType = ServiceNode
                              , resourceType = Nothing
                              , status = SharedReference "svc-001"
                              , duration = Nothing
                              , children = []
                              , metadata = HashMap.empty
                              }
                          ]
                      , metadata = HashMap.empty
                      }
                  ]
              , metadata = HashMap.empty
              }
          ]
      , metadata = HashMap.empty
      }
  , totalDuration = 0.35
  , totalResources = 5
  , sharedResources = 1
  }
