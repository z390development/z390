#!/usr/bin/env python3
"""Report Ctrl-Z bytes, NUL bytes, and missing final line endings in text files.

A file is in scope when this repository's .gitattributes marks it as plain
text. ``text=auto`` counts as text: Git's content probe is ignored, because
that probe misclassifies EBCDIC files. ``-text`` files are out of scope.
Setting ``eol`` also marks a file as text, which is how Git treats it.

Zero-byte files are ignored. Every other in-scope file is reported when:

* it contains a Ctrl-Z byte (0x1A)
* it contains a NUL byte (0x00)
* it does not end with a line-end marker (CR 0x0D, LF 0x0A, or CRLF)

Tracked paths come from ``git ls-files``. Attribute values come from
``git check-attr`` so matching follows Git's .gitattributes rules. System
and global attribute files are not consulted.
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path

CTRL_Z = 0x1A
NUL = 0x00
# CR, LF. A CRLF file ends in LF, so the last byte is enough.
LINE_END_BYTES = frozenset((0x0A, 0x0D))


def repo_root() -> Path:
    """Return the checkout root, whether this script sits there or under bat/."""
    here = Path(__file__).resolve().parent
    for candidate in (here, *here.parents):
        if (candidate / ".git").exists() and (candidate / ".gitattributes").is_file():
            return candidate
    return here


def git_output(repo: Path, args: list[str], stdin: bytes | None = None) -> bytes:
    """Run git in *repo* and return stdout.

    System gitattributes and core.attributesFile are disabled so the result
    comes from the repository's .gitattributes (and .git/info/attributes).
    """
    env = os.environ.copy()
    env["GIT_ATTR_NOSYSTEM"] = "1"
    result = subprocess.run(
        ["git", "-c", "core.attributesFile=", *args],
        cwd=repo,
        env=env,
        input=stdin,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if result.returncode != 0:
        detail = result.stderr.decode("utf-8", "replace").strip()
        command = " ".join(args)
        raise SystemExit(f"git {command} failed ({result.returncode}): {detail}")
    return result.stdout


def tracked_files(repo: Path) -> list[tuple[str, str]]:
    """Return (mode, path) for every index entry, path using forward slashes."""
    raw = git_output(repo, ["ls-files", "-s", "-z"])
    entries: list[tuple[str, str]] = []
    for record in raw.split(b"\0"):
        if not record:
            continue
        meta, path_bytes = record.split(b"\t", 1)
        mode = meta.split(b" ", 1)[0].decode("ascii")
        entries.append((mode, path_bytes.decode("utf-8", "surrogateescape")))
    return entries


def attribute_map(repo: Path, paths: list[str]) -> dict[str, dict[str, str]]:
    """Return {path: {attribute: value}} for text and eol."""
    if not paths:
        return {}
    payload = b"\0".join(path.encode("utf-8", "surrogateescape") for path in paths)
    payload += b"\0"
    raw = git_output(
        repo,
        ["check-attr", "--stdin", "-z", "text", "eol"],
        stdin=payload,
    )
    info: dict[str, dict[str, str]] = {}
    parts = raw.split(b"\0")
    # Each result is path, attribute, value, then a trailing empty field.
    if parts and parts[-1] == b"":
        parts.pop()
    if len(parts) % 3 != 0:
        raise SystemExit("git check-attr returned an unexpected record layout")
    for index in range(0, len(parts), 3):
        path = parts[index].decode("utf-8", "surrogateescape")
        attr = parts[index + 1].decode("ascii")
        value = parts[index + 2].decode("utf-8", "surrogateescape")
        info.setdefault(path, {})[attr] = value
    return info


def byte_hit(data: bytes, value: int) -> tuple[int, int] | None:
    """Return (count, first offset) when *value* occurs in *data*."""
    needle = bytes((value,))
    count = data.count(needle)
    if count == 0:
        return None
    return count, data.find(needle)


def report_byte_hits(label: str, hits: list[tuple[str, int, int]]) -> None:
    """Print one Ctrl-Z-style section: count, and the first offset in each file."""
    print()
    if hits:
        print(f"{label} in {len(hits)} file(s):")
        for path, count, first in hits:
            print(f"  {path} ({count}, first at offset {first})")
    else:
        print(f"{label}: none")


def is_plain_text(attrs: dict[str, str]) -> bool:
    """True when .gitattributes says this path is text, probe disregarded."""
    text = attrs.get("text", "unspecified")
    if text == "unset":
        return False
    if text != "unspecified":
        # "set", "auto", or any other explicit value. "auto" is not probed.
        return True
    # eol=lf / eol=crlf turns normalization on without a content check.
    return attrs.get("eol", "unspecified") in ("lf", "crlf")


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Report Ctrl-Z bytes, NUL bytes, and missing final line endings "
            "in files that .gitattributes marks as plain text."
        )
    )
    parser.parse_args()

    repo = repo_root()
    if not (repo / ".gitattributes").is_file():
        print(f"No .gitattributes in {repo}", file=sys.stderr)
        return 2
    if not (repo / ".git").exists():
        print(f"{repo} is not a git checkout", file=sys.stderr)
        return 2

    entries = tracked_files(repo)
    paths = [path for _mode, path in entries]
    attrs_by_path = attribute_map(repo, paths)

    ctrl_z: list[tuple[str, int, int]] = []
    nuls: list[tuple[str, int, int]] = []
    missing_eol: list[tuple[str, int]] = []
    unreadable: list[str] = []
    checked = 0
    skipped_empty = 0
    skipped_not_text = 0
    skipped_not_file = 0

    for mode, path in entries:
        attrs = attrs_by_path.get(path)
        if attrs is None:
            unreadable.append(f"{path}: no attribute result from git check-attr")
            continue
        if not is_plain_text(attrs):
            skipped_not_text += 1
            continue
        # 100644 regular file, 100755 executable. Skip gitlinks and symlinks.
        if mode not in ("100644", "100755"):
            skipped_not_file += 1
            continue

        full = repo / path
        try:
            data = full.read_bytes()
        except OSError as exc:
            unreadable.append(f"{path}: {exc}")
            continue
        if len(data) == 0:
            skipped_empty += 1
            continue

        checked += 1
        ctrl_z_hit = byte_hit(data, CTRL_Z)
        if ctrl_z_hit is not None:
            ctrl_z.append((path, *ctrl_z_hit))
        nul_hit = byte_hit(data, NUL)
        if nul_hit is not None:
            nuls.append((path, *nul_hit))
        if data[-1] not in LINE_END_BYTES:
            missing_eol.append((path, data[-1]))

    ctrl_z.sort(key=lambda item: item[0])
    nuls.sort(key=lambda item: item[0])
    missing_eol.sort(key=lambda item: item[0])

    print(
        f"Checked {checked} text file(s) marked by .gitattributes "
        f"(text=auto included without Git's content probe)."
    )
    print(
        f"Skipped {skipped_empty} empty file(s), "
        f"{skipped_not_text} file(s) not marked as text, "
        f"{skipped_not_file} non-regular path(s)."
    )

    report_byte_hits("Ctrl-Z (0x1A)", ctrl_z)
    report_byte_hits("NUL (0x00)", nuls)

    print()
    if missing_eol:
        print(f"No line-end marker at end of file: {len(missing_eol)} file(s):")
        for path, last in missing_eol:
            print(f"  {path} (last byte 0x{last:02X})")
    else:
        print("No line-end marker at end of file: none")

    if unreadable:
        print()
        print(f"Unreadable: {len(unreadable)} path(s):", file=sys.stderr)
        for line in unreadable:
            print(f"  {line}", file=sys.stderr)

    if ctrl_z or nuls or missing_eol or unreadable:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
