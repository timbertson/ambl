# plugin support
pants_requirements(name="pants")

# generic entrypoint, defines `trou` target
# TODO: how to define arbitraty targets?
trou(name="trou", builder="foo.wasm")

# Ways to specify a wasm target:
wasm_module(name="builder", envvar="FOO_WASM")
wasm_module(name="builder2", url="https://.../foo.wasm")

# Implicit wasm targets, based on contents of builder directory?
rust_wasm(name="builder3", path="./builder", target="main.wasm")
assemblyscript_wasm(name="builder4", path="./builder", target="main.wasm")
