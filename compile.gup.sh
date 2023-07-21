#!/usr/bin/env bash
set -eu
if [ "${GUP_XTRACE:-}" = 1 ]; then
	set -x
fi
PROFILE_FLAGS=""
if [ "$PROFILE" = release ]; then
	PROFILE_FLAGS="--release"
fi
cargo build $PROFILE_FLAGS --color always --target=wasm32-unknown-unknown --package ambl-builtin-cargo
cargo build $PROFILE_FLAGS --color always --package ambl-runtime
mkdir -p "target/wasm32-unknown-unknown/$PROFILE/component"
wasm-tools component \
	new "target/wasm32-unknown-unknown/$PROFILE/ambl_builtin_cargo.wasm" \
	-o  "target/wasm32-unknown-unknown/$PROFILE/component/ambl_builtin_cargo.wasm"
