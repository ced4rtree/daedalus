{ config, lib, inputs, mylib, ... }: {
  imports = mylib.scanPaths ./.;
}
