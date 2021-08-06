{ pkgsFun ? import <nixpkgs>
, pkgs ? pkgsFun {}
}:
rec {
  sources = {
    flaw-al = ./flaw-al;
    flaw-app = ./flaw-app;
    flaw-asset = ./flaw-asset;
    flaw-asset-dxt = ./flaw-asset-dxt;
    flaw-asset-wai = ./flaw-asset-wai;
    flaw-audio = ./flaw-audio;
    flaw-base = ./flaw-base;
    flaw-build = ./flaw-build;
    flaw-bullet = ./flaw-bullet;
    flaw-canvas = ./flaw-canvas;
    flaw-collada = ./flaw-collada;
    # flaw-dx11 = ./flaw-dx11;
    flaw-editor = ./flaw-editor;
    flaw-ffi = ./flaw-ffi;
    # flaw-ffmpeg = ./flaw-ffmpeg;
    flaw-font = ./flaw-font;
    flaw-font-fhi = ./flaw-font-fhi;
    # flaw-font-icu = ./flaw-font-icu;
    flaw-game = ./flaw-game;
    flaw-gamejolt-webapi = ./flaw-gamejolt-webapi;
    flaw-gl = ./flaw-gl;
    flaw-gl-mesa = ./flaw-gl-mesa;
    flaw-gl-sdl = ./flaw-gl-sdl;
    # flaw-gl-win32 = ./flaw-gl-win32;
    flaw-graphics = ./flaw-graphics;
    flaw-input = ./flaw-input;
    flaw-itch-webapi = ./flaw-itch-webapi;
    flaw-lmdb = ./flaw-lmdb;
    flaw-lua = ./flaw-lua;
    flaw-lua-refimpl = ./flaw-lua-refimpl;
    flaw-math = ./flaw-math;
    flaw-math-determ = ./flaw-math-determ;
    flaw-network = ./flaw-network;
    flaw-oil = ./flaw-oil;
    flaw-oil-client = ./flaw-oil-client;
    flaw-oil-server = ./flaw-oil-server;
    flaw-physics = ./flaw-physics;
    flaw-script = ./flaw-script;
    flaw-sdl = ./flaw-sdl;
    flaw-sl = ./flaw-sl;
    flaw-social = ./flaw-social;
    flaw-sqlite = ./flaw-sqlite;
    flaw-steam = ./flaw-steam;
    # flaw-steam-encryptedticket = ./flaw-steam-encryptedticket;
    flaw-steam-webapi = ./flaw-steam-webapi;
    flaw-ui = ./flaw-ui;
    flaw-ui-default-style = ./flaw-ui-default-style;
    flaw-ui-default-style-data = ./flaw-ui-default-style-data;
    flaw-visual = ./flaw-visual;
    flaw-vulkan = ./flaw-vulkan;
    # flaw-websocket-server = ./flaw-websocket-server;
    flaw-window = ./flaw-window;
  };

  jsSources = {
    flaw-base = ./flaw-base;
    flaw-js = ./flaw-js;
    flaw-network = ./flaw-network;
    flaw-websocket-client = ./flaw-websocket-client;
  };

  packageNames = pkgs.lib.attrNames sources;
  sourceOverrides = pkgs.haskell.lib.packageSourceOverrides sources;
  tweaks = self: super: {
    OSMesa = pkgs.mesa.osmesa;
    vulkan = pkgs.vulkan-loader;
  };

  jsPackageNames = pkgs.lib.attrNames jsSources;
  jsSourceOverrides = pkgs.haskell.lib.packageSourceOverrides jsSources;

  overrides = pkgs.lib.composeExtensions sourceOverrides tweaks;

  integerSimpleTweaks = self: super: with pkgs.haskell.lib; {
    cryptonite = disableCabalFlag super.cryptonite "integer-gmp";
  };

  packages = pkgs.haskell.packages.ghc8104.override {
    inherit overrides;
  };

  integerSimplePackages = pkgs.haskell.packages.integer-simple.ghc8104.override {
    overrides = pkgs.lib.composeExtensions overrides integerSimpleTweaks;
  };

  pkgsJs = pkgsFun {
    crossSystem = {
      config = "js-unknown-ghcjs";
    };
  };
  jsPackages = pkgsJs.haskell.packages.ghc8104.override {
    overrides = jsSourceOverrides;
  };

  bins = with (builtins.mapAttrs (name: pkg: pkgs.haskell.lib.justStaticExecutables pkg) packages); {
    inherit flaw-editor;
  };

  touch = let
    touchPackages = map (name: pkgs.lib.nameValuePair "gmp-${name}" packages."${name}") packageNames;
    touchIntegerSimplePackages = map (name: pkgs.lib.nameValuePair "is-${name}" integerSimplePackages."${name}") packageNames;
  in pkgs.lib.listToAttrs (touchPackages ++ touchIntegerSimplePackages);
}
