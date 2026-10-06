#!/bin/sh
set -eu

project_root=$(CDPATH='' cd -- "$(dirname -- "$0")/.." && pwd)
package_dir="$project_root/cli/pkg-uplc-wasm"
# keyan-m/uplc-wasm, branch fix/wasm-memory-growth.
revision=8982704804f45b2b27f6323814f333b66dc7b14e

if [ -f "$package_dir/.revision" ] \
    && [ "$(cat "$package_dir/.revision")" = "$revision" ] \
    && [ -f "$package_dir/pkg-node/uplc_wasm.js" ] \
    && [ -f "$package_dir/pkg-node/uplc_wasm_bg.wasm" ] \
    && [ -f "$package_dir/pkg-web/uplc_wasm.js" ] \
    && [ -f "$package_dir/pkg-web/uplc_wasm_bg.wasm" ]; then
    exit 0
fi

work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT
trap 'exit 1' HUP INT TERM
mkdir "$work_dir/source"
curl --fail --location --retry 3 \
    "https://codeload.github.com/keyan-m/uplc-wasm/tar.gz/$revision" \
    --output "$work_dir/source.tar.gz"
tar -xzf "$work_dir/source.tar.gz" --strip-components=1 -C "$work_dir/source"
(
    cd "$work_dir/source"
    CARGO_TARGET_DIR="$project_root/target/uplc-wasm" ./build.sh
)

mkdir -p "$package_dir"
rm -rf "$package_dir/pkg-node" "$package_dir/pkg-web"
mv "$work_dir/source/pkg-node" "$work_dir/source/pkg-web" "$package_dir/"
printf '%s\n' "$revision" > "$package_dir/.revision"
