{ mkDerivation, base, hashable, pure, pure-cond, pure-prop, pure-css, pure-styles, pure-theme, pure-txt, stdenv
}:
mkDerivation {
  pname = "pure-grid";
  version = "0.8.0.0";
  src = ./.;
  isLibrary = true;
  libraryHaskellDepends = [
    base hashable pure pure-cond pure-prop pure-css pure-styles pure-theme pure-txt
  ];
  license = stdenv.lib.licenses.bsd3;
}
