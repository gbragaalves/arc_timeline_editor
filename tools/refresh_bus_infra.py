#!/usr/bin/env python3
"""Builds reference/bus_infra.geojson, the "Bus infrastructure" map layer, from
an OSM extract (default ~/osrm-data/clipped-custom.osm.pbf, the routing base
with my own changesets and the local overrides).

Ways kept (highway=* only), with a kind:
  busway    highway=busway, or service=bus
  bus_only  access / motor_vehicle = no|private with a bus / psv permission
  bus_lane  busway[:left|:right|:both]=lane, lanes:bus / lanes:psv,
            bus:lanes / psv:lanes (and their :forward / :backward)
  bus_ok    other ways with bus / psv = yes|designated
  blocked   access = no|private without a bus / psv permission, within
            NEAR_M of a bus way (what is closed to buses next to them)
Each feature: id (OSM way id), kind, oneway (1, -1, 0; for buses), name, tags
(all of them), its geometry simplified (Douglas-Peucker, SIMPLIFY_M).
Run by ~/osrm-data/nightly_refresh.sh after each rebuild of the extract;
manually: python3 -I tools/refresh_bus_infra.py [extract.osm.pbf] [out.geojson]
"""

import json
import math
import os
import subprocess
import sys
import tempfile
import time

NEAR_M = 60
SIMPLIFY_M = 1.5
PERMIT = {"yes", "designated", "permissive"}
CLOSED = {"no", "private"}
LANE_KEYS = ["lanes:bus", "lanes:psv", "bus:lanes", "psv:lanes"]
LANE_KEYS += [k + d for k in LANE_KEYS for d in (":forward", ":backward")]
FILTERS = ["w/highway=busway", "w/service=bus", "w/bus=yes,designated,permissive", "w/psv=yes,designated,permissive",
           "w/busway=lane", "w/busway:left=lane", "w/busway:right=lane", "w/busway:both=lane",
           "w/access=no,private", "w/motor_vehicle=no,private"] + ["w/" + k for k in LANE_KEYS]


def kind_of(t):
    if t.get("highway") == "busway" or t.get("service") == "bus":
        return "busway"
    permit = t.get("bus") in PERMIT or t.get("psv") in PERMIT
    closed = t.get("access") in CLOSED or t.get("motor_vehicle") in CLOSED
    if closed and permit:
        return "bus_only"
    if any(t.get(k) == "lane" for k in ("busway", "busway:left", "busway:right", "busway:both")) or \
            any(k in t for k in LANE_KEYS):
        return "bus_lane"
    if t.get("bus") in ("yes", "designated") or t.get("psv") in ("yes", "designated"):
        return "bus_ok"
    if t.get("access") in CLOSED and not permit:
        return "blocked"
    return None


def oneway_of(t):
    if t.get("oneway:bus") == "no" or t.get("oneway:psv") == "no":
        return 0
    v = t.get("oneway:bus") or t.get("oneway:psv") or t.get("oneway")
    if v in ("yes", "true", "1") or t.get("junction") in ("roundabout", "circular"):
        return 1
    if v == "-1":
        return -1
    return 0


def _metres(lat):
    return 111320 * math.cos(math.radians(lat)), 110540


def simplify(coords, tol_m=SIMPLIFY_M):
    """Douglas-Peucker on lon/lat with a local metric scale."""
    if len(coords) < 3:
        return coords
    kx, ky = _metres(coords[0][1])
    pts = [(x * kx, y * ky) for x, y in coords]
    keep = [False] * len(pts)
    keep[0] = keep[-1] = True
    stack = [(0, len(pts) - 1)]
    while stack:
        a, b = stack.pop()
        ax, ay = pts[a]
        bx, by = pts[b]
        dx, dy = bx - ax, by - ay
        l2 = dx * dx + dy * dy
        far, idx = 0.0, None
        for i in range(a + 1, b):
            px, py = pts[i]
            if l2 == 0:
                d = math.hypot(px - ax, py - ay)
            else:
                f = max(0, min(1, ((px - ax) * dx + (py - ay) * dy) / l2))
                d = math.hypot(px - ax - f * dx, py - ay - f * dy)
            if d > far:
                far, idx = d, i
        if idx is not None and far > tol_m:
            keep[idx] = True
            stack += [(a, idx), (idx, b)]
    return [c for c, k in zip(coords, keep) if k]


def cells(coords, cell_deg, step_m=30):
    """Grid cells of points every step_m along the line."""
    out = set()
    for (x0, y0), (x1, y1) in zip(coords, coords[1:]):
        kx, ky = _metres(y0)
        n = max(1, int(math.hypot((x1 - x0) * kx, (y1 - y0) * ky) / step_m))
        for i in range(n + 1):
            f = i / n
            out.add((int((x0 + f * (x1 - x0)) // cell_deg), int((y0 + f * (y1 - y0)) // cell_deg)))
    if len(coords) == 1:
        out.add((int(coords[0][0] // cell_deg), int(coords[0][1] // cell_deg)))
    return out


def build(pbf, out):
    t0 = time.time()
    with tempfile.TemporaryDirectory() as tmp:
        filt = os.path.join(tmp, "filtered.osm.pbf")
        seq = os.path.join(tmp, "ways.geojsonseq")
        subprocess.run(["osmium", "tags-filter", pbf] + FILTERS + ["-o", filt, "-O"], check=True, capture_output=True)
        subprocess.run(["osmium", "export", filt, "-f", "geojsonseq", "-a", "id", "--geometry-types=linestring",
                        "-o", seq, "-O"], check=True, capture_output=True)
        bus, blocked = [], []
        with open(seq, encoding="utf-8") as f:
            for line in f:
                ft = json.loads(line.lstrip("\x1e"))
                props = dict(ft["properties"])
                way_id = props.pop("@id", None)
                if "highway" not in props or props["highway"] in ("platform", "bus_stop", "proposed", "construction"):
                    continue
                kind = kind_of(props)
                if kind is None:
                    continue
                coords = ft["geometry"]["coordinates"]
                rec = (way_id, kind, props, coords)
                (blocked if kind == "blocked" else bus).append(rec)
    cell_deg = NEAR_M / 111320.0
    bus_cells = set()
    for _, _, _, coords in bus:
        bus_cells |= cells(coords, cell_deg)
    near = set()
    for cx, cy in bus_cells:
        for dx in (-1, 0, 1):
            for dy in (-1, 0, 1):
                near.add((cx + dx, cy + dy))
    blocked = [b for b in blocked if cells(b[3], cell_deg) & near]
    features = []
    for way_id, kind, props, coords in bus + blocked:
        geom = [[round(x, 6), round(y, 6)] for x, y in simplify(coords)]
        features.append({"type": "Feature", "geometry": {"type": "LineString", "coordinates": geom},
                         "properties": {"id": int(way_id) if way_id is not None else None, "kind": kind,
                                        "oneway": oneway_of(props), "name": props.get("name", ""), "tags": props}})
    counts = {}
    for ft in features:
        counts[ft["properties"]["kind"]] = counts.get(ft["properties"]["kind"], 0) + 1
    os.makedirs(os.path.dirname(os.path.abspath(out)), exist_ok=True)
    tmp_out = out + ".tmp"
    with open(tmp_out, "w", encoding="utf-8") as f:
        json.dump({"type": "FeatureCollection", "source": os.path.basename(pbf),
                   "built": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()), "features": features},
                  f, ensure_ascii=False, separators=(",", ":"))
    os.replace(tmp_out, out)
    print("%s: %d ways (%s) from %s in %.0f s, %.1f MB" % (
        out, len(features), ", ".join("%s %d" % kv for kv in sorted(counts.items())), os.path.basename(pbf),
        time.time() - t0, os.path.getsize(out) / 1e6))


if __name__ == "__main__":
    here = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    pbf = sys.argv[1] if len(sys.argv) > 1 else os.path.expanduser("~/osrm-data/clipped-custom.osm.pbf")
    out = sys.argv[2] if len(sys.argv) > 2 else os.path.join(here, "reference", "bus_infra.geojson")
    if not os.path.exists(pbf):
        sys.exit("extract not found: " + pbf)
    build(pbf, out)
