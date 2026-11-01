# Usage: road21-hosts FILE [IP NAME...]
# Keeps one marked block in FILE (normally /etc/hosts) mapping each NAME
# to IP. With only FILE, removes the block. Lines outside the block are
# left alone; FILE is rewritten in place so its owner and mode stay.

file=$1
shift

begin="# BEGIN road21 (nix-darwin)"
end="# END road21 (nix-darwin)"

tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT

# Everything except an existing block; awk also ends the last line with a
# newline so the block can't be glued onto it
awk -v begin="$begin" -v end="$end" '
  $0 == begin { skip = 1; next }
  $0 == end { skip = 0; next }
  !skip { print }
' "$file" > "$tmp"

if [ $# -gt 1 ]; then
  ip=$1
  shift
  {
    echo "$begin"
    for name in "$@"; do
      echo "$ip $name"
    done
    echo "$end"
  } >> "$tmp"
fi

cat "$tmp" > "$file"
