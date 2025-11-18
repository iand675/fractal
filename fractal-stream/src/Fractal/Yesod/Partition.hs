-- Utilities for partitioning a Yesod application into multiple sub-applications.
--
-- This is intended to be used in conjunction with the 'Fractal.Layer' module.
--
-- The idea is that we can partition a Yesod application into multiple sub-applications,
-- each of which can be run in a separate process.
--
-- This differs from Yesod's built-in subsite functionality in that it allows for
-- top-level routes to be dispatched to different subsystems. When modularizing
-- a monolithic application, this is a useful way to split the application into
-- multiple subsystems without invasive changes to the application.
--
-- The partitioning is done by dispatching the request to the appropriate subsystem
-- based on the route.
module Fractal.Yesod.Partition where

import Control.Lens (Lens')
import Fractal.Layer

