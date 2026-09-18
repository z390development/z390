#!/usr/bin/env python3
#######################################################################
# z390 - Mainframe assembler emulator and run-time engine
# Copyright (C) 2021 z390 Assembler LLC
#
# This file is part of z390.
# z390 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# z390 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <https://www.gnu.org/licenses/>.
#######################################################################
"""Check a Java control-block class against the mapping macro it claims to follow.

The offsets in vzACB and friends are hand-copied out of the DSECT and the width
each one is read with is chosen by hand too, so nothing enforces the pairing.
This assembles the mapping macro, reads the offsets and widths back out of the
listing, and compares them against the Java.

    cbcheck.py ACBD2 src/vzACB.java

Exits non-zero if anything disagrees.
"""

import os
import re
import subprocess
import sys

# What each accessor on zControlBlock reads. Anything not here takes a length
# argument and cannot be checked from the call site alone.
GETTER = {
    "get_byte": 1,
    "get_short": 2,
    "get_int24": 3,
    "get_int": 4,
    "get_long": 8,
}

# DC/DS type letters that carry an implied length when no L modifier is given.
IMPLIED = {"X": 1, "B": 1, "C": 1, "A": 4, "F": 4, "H": 2, "D": 8, "P": 1, "Z": 1}

LISTING_DS = re.compile(
    r"^([0-9A-F]{6})\s+.*?\+\s*([A-Z@#$][A-Z0-9@#$_]*)\s+DS\s+(\S+)")
LISTING_EQU = re.compile(
    r"^([0-9A-F]{6})\s+.*?\+\s*([A-Z@#$][A-Z0-9@#$_]*)\s+EQU\s+(\S+)")
SYMTAB = re.compile(
    r"^\s*SYM=(\S+)\s+LOC=([0-9A-F]{8})\s+LEN=([0-9A-F]{8})\s+ESD=\S+\s+TYPE=(\S+)")
JAVA_CONST = re.compile(
    r"static\s+final\s+\w+\s+([A-Z][A-Z0-9_]*)\s*=\s*\(?\s*\w*\s*\)?\s*0x([0-9A-Fa-f]+)")
JAVA_GET = re.compile(r"\b(get_\w+)\s*\(\s*([A-Z][A-Z0-9_]*)\s*\)")

SYMBOL = re.compile(r"^[A-Z@#$][A-Z0-9@#$_]*$")


def ds_width(operand):
    """Width in bytes of a DS operand, or None if it is not a simple field."""
    m = re.match(r"^(\d*)([A-Z])(?:L(\d+))?$", operand)
    if not m:
        return None
    _dup, typ, length = m.groups()
    if length:
        return int(length)
    return IMPLIED.get(typ)


def assemble(macro, workdir, jvm_cp, macdir):
    """Assemble a one-line driver for the mapping macro, return its listing."""
    stem = "cbchk" + macro.lower()
    src = os.path.join(workdir, stem + ".MLC")
    with open(src, "w") as f:
        f.write("CBCHK    CSECT\n         %s\n         END\n" % macro)
    rc = subprocess.run(
        ["java", "-cp", jvm_cp, "mz390", src,
         "sysmac(+%s)" % macdir, "syscpy(+%s)" % macdir, "zvsam(2)"],
        cwd=workdir, capture_output=True, text=True)
    listing = os.path.join(workdir, stem + ".PRN")
    if rc.returncode != 0 or not os.path.exists(listing):
        sys.exit("cbcheck: mz390 failed on %s\n%s%s" % (macro, rc.stdout, rc.stderr))
    with open(listing, errors="replace") as f:
        return f.read().splitlines()


def read_dsect(lines):
    """Field offsets and widths from a listing, following EQU aliases."""
    width, alias, absolute = {}, {}, set()

    for line in lines:
        m = LISTING_DS.match(line)
        if m:
            _loc, name, operand = m.groups()
            width[name] = ds_width(operand)
            continue
        m = LISTING_EQU.match(line)
        if m:
            _loc, name, operand = m.groups()
            # An EQU to a bare symbol is another name for a field. An EQU to a
            # self-defining term is a bit mask or a flag value and maps nothing.
            if SYMBOL.match(operand):
                alias[name] = operand
            continue

    offset = {}
    for line in lines:
        m = SYMTAB.match(line)
        if m:
            name, loc, _len, typ = m.groups()
            if typ == "ABS":
                absolute.add(name)
            else:
                offset[name] = int(loc, 16)

    # Aliases can chain, so walk each one down to a field with a known width.
    for name, target in alias.items():
        seen = set()
        while target in alias and target not in seen:
            seen.add(target)
            target = alias[target]
        if target in width:
            width.setdefault(name, width[target])

    return offset, width, absolute


def read_java(path):
    with open(path, errors="replace") as f:
        text = f.read()
    consts = {m.group(1): int(m.group(2), 16) for m in JAVA_CONST.finditer(text)}
    reads = {}
    for m in JAVA_GET.finditer(text):
        getter, const = m.groups()
        if getter in GETTER:
            reads.setdefault(const, set()).add(GETTER[getter])
    return consts, reads


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    macro, java = sys.argv[1], sys.argv[2]
    workdir = os.path.abspath(os.path.dirname(java) or ".")
    workdir = os.path.abspath(os.path.join(workdir, ".."))

    lines = assemble(macro, workdir, "classes", "mac")
    offset, width, absolute = read_dsect(lines)
    consts, reads = read_java(java)

    problems = []
    for name, value in sorted(consts.items()):
        if name in absolute or name not in offset:
            continue                        # a flag value, or not a field at all
        if value != offset[name]:
            problems.append("OFFSET  %-10s java 0x%04X, dsect 0x%04X"
                            % (name, value, offset[name]))
        for got in sorted(reads.get(name, ())):
            want = width.get(name)
            if want is not None and got != want:
                problems.append("WIDTH   %-10s read as %d-byte, field is %d-byte"
                                % (name, got, want))

    checked = sum(1 for n in consts if n in offset and n not in absolute)
    print("%s vs %s: %d field(s) checked" % (macro, os.path.basename(java), checked))
    for p in problems:
        print("  " + p)
    print("%d problem(s)" % len(problems))
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
