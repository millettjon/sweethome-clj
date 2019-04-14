with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "sweethome-clj";

  # The packages in the `buildInputs` list will be added to the PATH in our shell
  # See https://nixos.org/nixos/packages.html for available packages.
  buildInputs = with pkgs; [
    openjdk
    boot
    sweethome3d.application
  ];

  shellHook = ''
    export BOOT_CLOJURE_VERSION=1.10.0
    export SWEETHOME3D_HOME=${pkgs.sweethome3d.application}
    export SWEETHOME3D_JAR=$(ls $SWEETHOME3D_HOME/share/java/SweetHome3D*.jar)
  '';
}
