let
	sources = import ./sources.nix {};
	pkgs = import <nixpkgs> {
		overlays = [
			(import "${sources.fenix}/overlay.nix")
		];
	};
	fenix = pkgs.fenix;

in with pkgs;
mkShell {
	buildInputs = [
		libiconv
		wasm-pack
		(fenix.combine [
			(fenix.stable.withComponents [ "cargo" "rustc" "rust-src" ])
			(fenix.targets.wasm32-unknown-unknown.stable.rust-std)
		])
	];
}
