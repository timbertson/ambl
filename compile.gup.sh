#!/usr/bin/env bash
set -eu
if [ "${GUP_XTRACE:-}" = 1 ]; then
	set -x
fi
PROFILE_FLAGS=""
if [ "$PROFILE" = release ]; then
	PROFILE_FLAGS="--release"
fi
cargo build $PROFILE_FLAGS --color always --package ambl-runtime
mkdir -p "target/wasm32-unknown-unknown/$PROFILE/component"

for builtin in cargo ninja; do
	cargo build $PROFILE_FLAGS --color always --target=wasm32-unknown-unknown --package ambl-builtin-$builtin
	wasm-tools component \
		new "target/wasm32-unknown-unknown/$PROFILE/ambl_builtin_$builtin.wasm" \
		-o "target/wasm32-unknown-unknown/$PROFILE/component/ambl_builtin_$builtin.wasm"
done
