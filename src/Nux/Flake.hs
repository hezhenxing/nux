{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Nux.Flake where

import RIO
import Data.Default
import Nux.Expr
import Nux.Pretty

data Input
  = Input
    { url     :: String
    , follows :: Maybe String
    }
  | NonFlakeInput
    { url :: String
    }
  deriving (Eq)

instance ToNixExpr Input where
  toNixExpr Input{..} =
    NixAttrs
      [ ("url", toNixExpr url)
      , ("follows", maybe NixNull toNixExpr follows)
      ]
  toNixExpr (NonFlakeInput url) =
    NixAttrs
      [ ("url", toNixExpr url)
      , ("flake", NixBool False)
      ]

instance Show Input where
  show = show . toNixExpr

instance Pretty Input where
  pretty = pretty . toNixExpr

input :: String -> Input
input url = Input
  { url     = url
  , follows = Nothing
  }

inputWithFollows :: String -> String -> Input
inputWithFollows url follows = Input
  { url     = url
  , follows = Just follows
  }

inputNoFlake :: String -> Input
inputNoFlake = NonFlakeInput

nixpkgs :: Input
nixpkgs = Input
  { url     = "github:NixOS/nixpkgs/nixos-unstable"
  , follows = Nothing
  }

data Module
  = PathModule String
  | ExprModule NixExpr
  deriving (Eq)

instance ToNixExpr Module where
  toNixExpr (PathModule path) = NixPath path
  toNixExpr (ExprModule expr) = NixParen expr

instance Show Module where
  show = show . toNixExpr

data NixosConfiguration = NixosConfiguration
  { system          :: Maybe System
  , modules         :: [Module]
  , specialArgs     :: [(String, NixExpr)]
  , modulesLocation :: Maybe String
  } deriving (Eq)

instance Show NixosConfiguration where
  show = show . toNixExpr

instance ToNixExpr NixosConfiguration where
  toNixExpr (NixosConfiguration sys mods specialArgs modLoc) =
    NixAttrs
      [ ("system", maybe NixNull toNixExpr sys)
      , ("modules", toNixExpr mods)
      , ("specialArgs", toNixExpr specialArgs)
      , ("modulesLocation", maybe NixNull toNixExpr modLoc)
      ]

instance Pretty NixosConfiguration where
  pretty = pretty . toNixExpr

emptyNixosConfiguration :: NixosConfiguration
emptyNixosConfiguration = NixosConfiguration
  { system          = Nothing
  , modules         = []
  , specialArgs     = []
  , modulesLocation = Nothing
  }

data System
  = X86_64_Linux
  | Aarch64_Linux
  | X86_64_Darwin
  | Aarch64_Darwin
  deriving (Eq)

instance ToNixExpr System where
  toNixExpr X86_64_Linux   = NixStr "x86_64-linux"
  toNixExpr Aarch64_Linux  = NixStr "aarch64-linux"
  toNixExpr X86_64_Darwin  = NixStr "x86_64-darwin"
  toNixExpr Aarch64_Darwin = NixStr "aarch64-darwin"

instance Show System where
  show X86_64_Linux   = "x86_64-linux"
  show Aarch64_Linux  = "aarch64-linux"
  show X86_64_Darwin  = "x86_64-darwin"
  show Aarch64_Darwin = "aarch64-darwin"

instance Pretty System where
  pretty = pretty . toNixExpr

data Outputs = Outputs
  { nixosConfigurations :: [(String, NixosConfiguration)]
  -- , outputsChecks         :: [((System, String) ,Derivation)]
  -- , outputsPackages       :: [((System, String) ,Derivation)]
  -- , outputsApps           :: [((System, String) ,Derivation)]
  -- , outputsFormatters     :: [(System ,Derivation)]
  -- , outputsLegacyPackages :: [((System, String) ,Derivation)]
  -- , outputsOverlays       :: [(String, Overlay)]
  -- , outputsNixosModules   :: [(String, Module)]
  -- , outputsDevShells      :: [((System, String) ,Derivation)]
  -- , outputsHydraJobs      :: [(String, HydraJob)]
  -- , outputsTemplates      :: [(String, Template)]
  }

instance Default Outputs where
  def = Outputs
    { nixosConfigurations = []
    -- , checks         = []
    -- , packages       = []
    -- , apps           = []
    -- , formatters     = []
    -- , legacyPackages = []
    -- , overlays       = []
    -- , nixosModules   = []
    -- , devShells      = []
    -- , hydraJobs      = []
    -- , templates      = []
    }

instance ToNixExpr Outputs where
  toNixExpr (Outputs nixosConfigs) =
    NixAttrs
      [ ("nixosConfigurations", toNixExpr nixosConfigs)
      -- , ("checks", toNixExpr checks)
      -- , ("packages", toNixExpr packages)
      -- , ("apps", toNixExpr apps)
      -- , ("formatters", toNixExpr formatters)
      -- , ("legacyPackages", toNixExpr legacyPackages)
      -- , ("overlays", toNixExpr overlays)
      -- , ("nixosModules", toNixExpr nixosModules)
      -- , ("devShells", toNixExpr devShells)
      -- , ("hydraJobs", toNixExpr hydraJobs)
      -- , ("templates", toNixExpr templates)
      ]

instance Show Outputs where
  show = show . toNixExpr

instance Pretty Outputs where
  pretty = pretty . toNixExpr

data Flake = Flake
  { description :: String
  , inputs      :: [(String, Input)]
  , outputs     :: Outputs
  , nixConfig   :: [(String, NixExpr)]
  }

instance Default Flake where
  def = Flake
    { description = ""
    , inputs      = []
    , outputs     = def
    , nixConfig   = []
    }

instance ToNixExpr Flake where
  toNixExpr Flake {..} =
    NixAttrs
      [ ("description", toNixExpr description)
      , ("inputs", toNixExpr inputs)
      , ("outputs", NixFunc (NixArg "inputs") (toNixExpr outputs))
      , ("nixConfig", toNixExpr nixConfig)
      ]

instance Show Flake where
  show = show. toNixExpr

instance Pretty Flake where
  pretty = pretty . toNixExpr

-- addInput :: Flake -> String -> Input -> Flake
-- addInput flake name input =
--   flake { inputs = (name, input) : inputs flake }

exampleFlake :: Flake
exampleFlake = Flake
  { description = "Example flake"
  , inputs      =
      [ ("nixpkgs", input "github:NixOS/nixpkgs/nixos-unstable")
      ]
  , outputs     = Outputs
      { nixosConfigurations =
          [ ("example", NixosConfiguration
              { system          = Just X86_64_Linux
              , modules         =
                [ PathModule "./configuration.nix"
                , ExprModule
                  ( NixFunc
                    (NixVArgs
                      [("pkgs", Nothing), ("lib", Nothing)
                      ]
                    )
                    (NixAttrs
                      [ ("bool.loader.enable", NixBool True)
                      , ("bool.loader.efi.canTouchEfiVariables", NixBool True)
                      ]
                    )
                  )
                ]
              , specialArgs     =
                [ ("host", NixStr "example")
                ]
              , modulesLocation = Nothing
              })
          ]
      }
  , nixConfig   = [("allowUnfree", NixBool True)]
  }
