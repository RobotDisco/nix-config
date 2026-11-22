# SPDX-FileCopyrightText: 2026 Gaelan D'costa <gaelan@fastmail.ca>
# SPDX-License-Identifier: 0BSD

{
  sources ? import ./npins,
}:
let
  pkgs = import sources.nixpkgs { };
in
pkgs.mkShell {
  packages = [
    pkgs.npins
    pkgs.reuse
  ];
}
