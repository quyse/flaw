{ pkgs }:
rec {
  packageOverrides = pkgs.haskell.lib.packageSourceOverrides {
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
    flaw-dx11 = ./flaw-dx11;
    flaw-editor = ./flaw-editor;
    flaw-ffi = ./flaw-ffi;
    flaw-ffmpeg = ./flaw-ffmpeg;
    flaw-font = ./flaw-font;
    flaw-font-fhi = ./flaw-font-fhi;
    flaw-font-icu = ./flaw-font-icu;
    flaw-game = ./flaw-game;
    flaw-gamejolt-webapi = ./flaw-gamejolt-webapi;
    flaw-gl = ./flaw-gl;
    flaw-gl-mesa = ./flaw-gl-mesa;
    flaw-gl-sdl = ./flaw-gl-sdl;
    flaw-gl-win32 = ./flaw-gl-win32;
    flaw-graphics = ./flaw-graphics;
    flaw-input = ./flaw-input;
    flaw-itch-webapi = ./flaw-itch-webapi;
    flaw-js = ./flaw-js;
    flaw-lmdb = ./flaw-lmdb;
    flaw-lua = ./flaw-lua;
    flaw-lua-refimpl = ./flaw-lua-refimpl;
    flaw-math = ./flaw-math;
    flaw-math-determ = ./flaw-math-determ;
    flaw-media = ./flaw-media;
    flaw-mess = ./flaw-mess;
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
    flaw-steam-encryptedticket = ./flaw-steam-encryptedticket;
    flaw-steam-webapi = ./flaw-steam-webapi;
    flaw-ui = ./flaw-ui;
    flaw-ui-default-style = ./flaw-ui-default-style;
    flaw-ui-default-style-data = ./flaw-ui-default-style-data;
    flaw-visual = ./flaw-visual;
    flaw-vulkan = ./flaw-vulkan;
    flaw-websocket-client = ./flaw-websocket-client;
    flaw-websocket-server = ./flaw-websocket-server;
    flaw-window = ./flaw-window;
  };

  integerSimpleOverrides = self: super: with pkgs.haskell.lib; {
    cryptonite = disableCabalFlag super.cryptonite "integer-gmp";
  };

  packages = pkgs.haskell.packages.ghc883.override {
    overrides = packageOverrides;
  };

  integerSimplePackages = pkgs.haskell.packages.integer-simple.ghc883.override {
    overrides = pkgs.lib.composeExtensions packageOverrides integerSimpleOverrides;
  };

  bins = with (builtins.mapAttrs (name: pkg: pkgs.haskell.lib.justStaticExecutables pkg) packages); {
    inherit flaw-editor;
  };
}
