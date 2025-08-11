{ config, lib, pkgs, mylib, ... }: {
  imports = mylib.scanPaths ./.;
}
