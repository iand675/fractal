{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example demonstrating the Layer Diagnostics system
--
-- This example shows how to use the diagnostics module to:
-- - Visualize layer initialization
-- - Track shared resources
-- - Export to JSON
-- - Debug performance issues

module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T
import Fractal.Layer.Diagnostics
import System.IO

main :: IO ()
main = do
  putStrLn "=== Fractal.Layer Diagnostics Example ==="
  putStrLn ""

  -- Show example diagnostics
  demonstrateTerminalOutput
  putStrLn ""

  demonstrateJSONExport
  putStrLn ""

  demonstrateDetailedOutput
  putStrLn ""

-- | Demonstrate terminal ASCII art output
demonstrateTerminalOutput :: IO ()
demonstrateTerminalOutput = do
  putStrLn "1. Terminal ASCII Tree Output"
  putStrLn "-----------------------------"
  putStrLn $ renderLayerTree exampleDiagnostics

-- | Demonstrate JSON export
demonstrateJSONExport :: IO ()
demonstrateJSONExport = do
  putStrLn "2. JSON Export"
  putStrLn "--------------"
  let json = encode exampleDiagnostics
  putStrLn "JSON exported to: layer-diagnostics.json"
  BSL.writeFile "layer-diagnostics.json" json
  putStrLn $ "Size: " ++ show (BSL.length json) ++ " bytes"

-- | Demonstrate detailed output with metadata
demonstrateDetailedOutput :: IO ()
demonstrateDetailedOutput = do
  putStrLn "3. Detailed Output with Metadata"
  putStrLn "--------------------------------"
  putStrLn $ renderLayerTreeDetailed exampleDiagnostics

-- | Example showing typical application layer structure
exampleDiagnostics :: LayerDiagnostics
exampleDiagnostics = LayerDiagnostics
  { rootNode = rootLayerNode
  , totalDuration = 0.45
  , totalResources = 7
  , sharedResources = 2
  }

rootLayerNode :: LayerNode
rootLayerNode = LayerNode
  { nodeId = "root"
  , nodeName = "ApplicationLayer"
  , nodeType = SequentialNode
  , resourceType = Nothing
  , status = Initialized
  , duration = Just 0.45
  , children =
      [ configLayerNode
      , servicesNode
      ]
  , metadata = HashMap.fromList
      [ ("version", "1.0.0")
      , ("environment", "production")
      ]
  }

configLayerNode :: LayerNode
configLayerNode = LayerNode
  { nodeId = "node-1"
  , nodeName = "ConfigLayer"
  , nodeType = EffectNode
  , resourceType = Nothing
  , status = Initialized
  , duration = Just 0.05
  , children = []
  , metadata = HashMap.fromList
      [ ("source", "environment-variables")
      , ("validated", "true")
      ]
  }

servicesNode :: LayerNode
servicesNode = LayerNode
  { nodeId = "node-2"
  , nodeName = "ParallelServices"
  , nodeType = ParallelNode
  , resourceType = Nothing
  , status = Initialized
  , duration = Just 0.4
  , children =
      [ databaseLayerNode
      , cacheLayerNode
      , webServerNode
      ]
  , metadata = HashMap.empty
  }

databaseLayerNode :: LayerNode
databaseLayerNode = LayerNode
  { nodeId = "node-3"
  , nodeName = "DatabaseLayer"
  , nodeType = ResourceNode
  , resourceType = Nothing
  , status = Initialized
  , duration = Just 0.25
  , children =
      [ connectionPoolNode
      ]
  , metadata = HashMap.fromList
      [ ("host", "localhost")
      , ("port", "5432")
      , ("database", "myapp")
      ]
  }

connectionPoolNode :: LayerNode
connectionPoolNode = LayerNode
  { nodeId = "svc-001"
  , nodeName = "ConnectionPool"
  , nodeType = ServiceNode
  , resourceType = Nothing
  , status = Initialized
  , duration = Just 0.2
  , children = []
  , metadata = HashMap.fromList
      [ ("poolSize", "20")
      , ("maxLifetime", "1800s")
      , ("strategy", "fifo")
      ]
  }

cacheLayerNode :: LayerNode
cacheLayerNode = LayerNode
  { nodeId = "node-4"
  , nodeName = "CacheLayer"
  , nodeType = ResourceNode
  , resourceType = Nothing
  , status = Initialized
  , duration = Just 0.15
  , children =
      [ redisClientNode
      , metricsCollectorNode1
      ]
  , metadata = HashMap.fromList
      [ ("host", "localhost")
      , ("port", "6379")
      ]
  }

redisClientNode :: LayerNode
redisClientNode = LayerNode
  { nodeId = "node-5"
  , nodeName = "RedisClient"
  , nodeType = ResourceNode
  , resourceType = Nothing
  , status = Initialized
  , duration = Just 0.1
  , children = []
  , metadata = HashMap.fromList
      [ ("maxConnections", "50")
      ]
  }

metricsCollectorNode1 :: LayerNode
metricsCollectorNode1 = LayerNode
  { nodeId = "svc-002"
  , nodeName = "MetricsCollector"
  , nodeType = ServiceNode
  , resourceType = Nothing
  , status = Initialized
  , duration = Just 0.05
  , children = []
  , metadata = HashMap.fromList
      [ ("endpoint", "http://metrics:9090")
      , ("flushInterval", "10s")
      ]
  }

webServerNode :: LayerNode
webServerNode = LayerNode
  { nodeId = "node-6"
  , nodeName = "WebServerLayer"
  , nodeType = ResourceNode
  , resourceType = Nothing
  , status = Initialized
  , duration = Just 0.3
  , children =
      [ httpServerNode
      , metricsCollectorSharedNode
      ]
  , metadata = HashMap.fromList
      [ ("port", "8080")
      , ("threads", "4")
      ]
  }

httpServerNode :: LayerNode
httpServerNode = LayerNode
  { nodeId = "node-7"
  , nodeName = "HTTPServer"
  , nodeType = ResourceNode
  , resourceType = Nothing
  , status = Initialized
  , duration = Just 0.25
  , children = []
  , metadata = HashMap.fromList
      [ ("middleware", "logging,compression,cors")
      ]
  }

-- This node references the shared MetricsCollector service
metricsCollectorSharedNode :: LayerNode
metricsCollectorSharedNode = LayerNode
  { nodeId = "node-8"
  , nodeName = "MetricsCollector"
  , nodeType = ServiceNode
  , resourceType = Nothing
  , status = SharedReference "svc-002"
  , duration = Nothing
  , children = []
  , metadata = HashMap.empty
  }

-- | Example showing a failed layer
failedExampleDiagnostics :: LayerDiagnostics
failedExampleDiagnostics = LayerDiagnostics
  { rootNode = LayerNode
      { nodeId = "root"
      , nodeName = "FailedApplication"
      , nodeType = SequentialNode
      , resourceType = Nothing
      , status = Failed "Database connection failed"
      , duration = Just 0.1
      , children =
          [ LayerNode
              { nodeId = "node-1"
              , nodeName = "ConfigLayer"
              , nodeType = EffectNode
              , resourceType = Nothing
              , status = Initialized
              , duration = Just 0.05
              , children = []
              , metadata = HashMap.empty
              }
          , LayerNode
              { nodeId = "node-2"
              , nodeName = "DatabaseLayer"
              , nodeType = ResourceNode
              , resourceType = Nothing
              , status = Failed "Connection timeout after 30s"
              , duration = Just 0.05
              , children = []
              , metadata = HashMap.fromList
                  [ ("host", "unreachable-db:5432")
                  , ("retries", "3")
                  ]
              }
          ]
      , metadata = HashMap.empty
      }
  , totalDuration = 0.1
  , totalResources = 2
  , sharedResources = 0
  }
