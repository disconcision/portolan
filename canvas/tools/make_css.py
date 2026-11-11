#!/usr/bin/env python3
"""Build canvas/canvas.css from Hazel's canvas.css and animations.css.

    python3 canvas/tools/make_css.py /path/to/hazel-worktree

What it does, mechanically, so the result can be regenerated when the
upstream stylesheets change:

1. Every custom property Hazel's rules read (--token-typ, --df-bg, ...)
   is renamed with a --pc- prefix, so a host page's own variables cannot
   collide with them. (--card-zoom and --card-bleed-zoom keep their names:
   the canvas code sets them inline.)
2. Every literal colour (hex, rgb(a), white, black) becomes a reference to
   a palette variable, named by hue family and lightness rank
   (--pc-purple-2, --pc-paper-1, --pc-shadow-40, ...).
3. All of those variables get their Hazel values as defaults in one
   zero-specificity block at the top, so any rule in the host page
   overrides them.
4. canvas/tools/portolan.css (the few rules the port itself needs) is
   appended.
"""
import colorsys
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
OUT = HERE.parent / "canvas.css"

KEEP = {"--card-zoom", "--card-bleed-zoom"}
NAMED = {"white": "#ffffff", "black": "#000000"}

COLOR_RE = re.compile(
    r"#[0-9a-fA-F]{3,8}\b|rgba?\([^)]*\)|(?<![-\w])(?:white|black)(?![-\w(])"
)


def parse(lit):
    """literal -> (r, g, b, a) with channels in 0..1"""
    lit = lit.strip().lower()
    if lit in NAMED:
        lit = NAMED[lit]
    if lit.startswith("#"):
        h = lit[1:]
        if len(h) in (3, 4):
            h = "".join(c * 2 for c in h)
        r, g, b = (int(h[i : i + 2], 16) / 255 for i in (0, 2, 4))
        a = int(h[6:8], 16) / 255 if len(h) == 8 else 1.0
        return (r, g, b, a)
    nums = [float(x) for x in re.findall(r"[\d.]+", lit)]
    r, g, b = (n / 255 for n in nums[:3])
    a = nums[3] if len(nums) > 3 else 1.0
    return (r, g, b, a)


def family(rgba):
    r, g, b, a = rgba
    h, l, s = colorsys.rgb_to_hls(r, g, b)
    hue = h * 360
    if l < 0.02:
        return "shadow" if a < 1 else "black"
    if l > 0.995:
        return "white"
    if s < 0.12:
        return "grey"
    if hue < 20 or hue >= 345:
        return "red"
    if hue < 65:
        if l > 0.86:
            return "paper"
        if l < 0.36:
            return "ink"
        return "gold" if s > 0.6 else "tan"
    if hue < 170:
        return "green"
    if hue < 250:
        return "blue"
    return "purple"


def canonical(rgba):
    r, g, b, a = rgba
    if a >= 0.999:
        return "#%02x%02x%02x" % tuple(round(c * 255) for c in (r, g, b))
    return "rgba(%d, %d, %d, %s)" % (
        round(r * 255),
        round(g * 255),
        round(b * 255),
        ("%.2f" % a).rstrip("0").rstrip("."),
    )


def main():
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    hazel = Path(sys.argv[1])
    style = hazel / "src/web/www/style"
    canvas = (style / "canvas.css").read_text()
    anims = (style / "animations.css").read_text()
    theme = (style / "theme-generated.css").read_text()
    variables = (style / "variables.css").read_text()
    portolan = (HERE / "portolan.css").read_text()
    src = (
        "/* ---- Hazel: src/web/www/style/animations.css ---- */\n\n"
        + anims
        + "\n/* ---- Hazel: src/web/www/style/canvas.css ---- */\n\n"
        + canvas
    )

    # 1. theme variables: every --name read through var() and not defined
    #    by the stylesheet itself gets Hazel's value as its default
    read = sorted(set(re.findall(r"var\((--[A-Za-z0-9_-]+)", src)) - KEEP)
    defined_here = set(re.findall(r"^\s*(--[A-Za-z0-9_-]+)\s*:", src, re.M))
    theme_defaults = {}
    for name in read:
        m = re.search(
            r"^\s*" + re.escape(name) + r"\s*:\s*([^;]+);", theme + variables, re.M
        )
        if m:
            theme_defaults[name] = m.group(1).strip()
        elif name not in defined_here:
            # read with a fallback only: the fallback is the default
            print("no Hazel default for", name, file=sys.stderr)

    def rename(m):
        return m.group(0) if m.group(1) in KEEP else "--pc-" + m.group(1)[2:]

    src = re.sub(r"(--[A-Za-z][A-Za-z0-9_-]*)", rename, src)

    # 2. the palette: one variable per distinct literal colour
    seen = {}
    for m in COLOR_RE.finditer(src):
        rgba = parse(m.group(0))
        seen.setdefault(canonical(rgba), rgba)
    fams = {}
    for canon, rgba in seen.items():
        fams.setdefault(family(rgba), []).append((canon, rgba))
    names = {}
    for fam, members in fams.items():
        if fam in ("white", "black"):
            for canon, _ in members:
                names[canon] = "--pc-" + fam
            continue
        if fam == "shadow":
            for canon, rgba in members:
                names[canon] = "--pc-shadow-%d" % round(rgba[3] * 100)
            continue
        # solid colours first, lightest first; a tint is named after its
        # solid sibling when there is one, else takes the next number
        solids = sorted(
            (m for m in members if m[1][3] >= 0.999),
            key=lambda m: -colorsys.rgb_to_hls(*m[1][:3])[1],
        )
        for i, (canon, rgba) in enumerate(solids, 1):
            names[canon] = "--pc-%s-%d" % (fam, i)
        tints = [m for m in members if m[1][3] < 0.999]
        n = len(solids)
        bases = {}
        for canon, rgba in sorted(tints, key=lambda m: (m[1][:3], -m[1][3])):
            solid = canonical(rgba[:3] + (1.0,))
            if solid in names:
                base = names[solid]
            elif rgba[:3] in bases:
                base = bases[rgba[:3]]
            else:
                n += 1
                base = bases[rgba[:3]] = "--pc-%s-%d" % (fam, n)
            names[canon] = "%s-a%d" % (base, round(rgba[3] * 100))
    assert len(set(names.values())) == len(names), "palette name clash"

    def to_var(m):
        return "var(%s)" % names[canonical(parse(m.group(0)))]

    out_lines = []
    for line in src.split("\n"):
        # comments keep their words ("black", "white") as written
        if line.lstrip().startswith(("/*", "*")) and ":" not in line.split("*/")[-1]:
            out_lines.append(line)
            continue
        out_lines.append(COLOR_RE.sub(to_var, line))
    body = "\n".join(out_lines)
    left = [
        m.group(0)
        for line in body.split("\n")
        if not line.lstrip().startswith(("/*", "*"))
        for m in COLOR_RE.finditer(re.sub(r"/\*.*?\*/", "", line))
    ]
    assert not left, left

    head = [
        "/* Portolan canvas stylesheet.",
        "",
        "   GENERATED by canvas/tools/make_css.py from Hazel's canvas.css and",
        "   animations.css (see canvas/UPSTREAM.md). Do not edit by hand: edit",
        "   canvas/tools/portolan.css or the generator and run it again.",
        "",
        "   Theming: every colour below is a custom property. Override any of",
        "   them on :root (or on .portolan-canvas) in the host page; the",
        "   defaults are Hazel's values and have zero specificity. */",
        "",
        ":where(:root) {",
        "  /* Hazel's theme roles */",
    ]
    for name in sorted(theme_defaults):
        head.append("  --pc-%s: %s;" % (name[2:], theme_defaults[name]))
    head.append("")
    head.append("  /* the palette: literal colours in Hazel's canvas.css */")
    order = sorted(names.items(), key=lambda kv: (re.sub(r"\d+", lambda d: d.group(0).zfill(3), kv[1])))
    for canon, name in order:
        head.append("  %s: %s;" % (name, canon))
    head.append("}")
    head.append("")
    OUT.write_text("\n".join(head) + "\n" + body + "\n" + portolan)
    print(
        "wrote %s: %d theme roles, %d palette colours"
        % (OUT, len(theme_defaults), len(names))
    )


if __name__ == "__main__":
    main()
