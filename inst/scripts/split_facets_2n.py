#!/usr/bin/env python3
"""Reshape one flat facets-suite-2n run into the split class subtrees.

facets-suite-2n emits all five fits into a single directory with class-infixed
filenames (<sample>_clinical_purity.*, <sample>_research_ultra_hisens.*, ...).
The rest of facets-preview -- and the whole standard downstream -- expects each
class to look like an ordinary FACETS sample directory, so the pipeline's
SPLIT_FACETS_2N module reshapes the output into

    <pair>/clinical/<fit>/
    <pair>/research/<fit>/
    <pair>/research/<fit>/ultra/

This is the same transformation, applied to a single refit directory produced by
the app rather than to a whole sample: the run tokens, the `# TAG =` rewrite and
the shared-file handling are kept identical so a refit is indistinguishable on
disk from a pipeline-produced fit.

Usage:
    split_facets_2n.py <sample_id> <staging_dir> <pair_dir> <fit_name>

<staging_dir> is the flat directory the wrapper wrote; its files are moved into
<pair_dir>/<class>/<fit_name>/ and the emptied staging dir is removed.
"""

import os
import shutil
import sys


def log(msg):
    sys.stderr.write("[split_facets_2n] " + msg + "\n")


# Per-fit run tokens, longest/most-specific first so 'research_ultra_hisens' is
# matched before 'research_hisens'. (token, class, nested_ultra, replacement)
RUN_TOKENS = [
    ('_research_ultra_hisens', 'research', True,  '_hisens'),
    ('_clinical_purity',       'clinical', False, '_purity'),
    ('_clinical_hisens',       'clinical', False, '_hisens'),
    ('_research_purity',       'research', False, '_purity'),
    ('_research_hisens',       'research', False, '_hisens'),
]


def rewrite_out_tag(path):
    """Collapse the class/ultra infix in the `# TAG = ...` line of a .out file.

    metadata_init resolves a fit's files from this TAG, so it has to match the
    renamed files. Safe to apply across the whole (small) file: only the TAG
    line carries the infix.
    """
    with open(path) as fh:
        txt = fh.read()
    new = (txt.replace('_research_ultra_hisens', '_hisens')
              .replace('_clinical_purity', '_purity')
              .replace('_clinical_hisens', '_hisens')
              .replace('_research_purity', '_purity')
              .replace('_research_hisens', '_hisens'))
    if new != txt:
        with open(path, 'w') as fh:
            fh.write(new)


def place(src, dest_dir, dest_name):
    os.makedirs(dest_dir, exist_ok=True)
    dest = os.path.join(dest_dir, dest_name)
    shutil.move(src, dest)
    if dest.endswith('.out'):
        rewrite_out_tag(dest)


def handle_file(fname, src_dir, pair_dir, fit_name):
    src = os.path.join(src_dir, fname)

    # Per-fit run files (Rdata/out/cncf/rds/png/seg/...).
    for tok, cls, nested, repl in RUN_TOKENS:
        if tok in fname:
            dest_dir = os.path.join(pair_dir, cls, fit_name)
            if nested:
                dest_dir = os.path.join(dest_dir, 'ultra')
            place(src, dest_dir, fname.replace(tok, repl))
            return

    # Class-infixed aggregate files: <sample>.clinical.<x> / <sample>.research.<x>
    for cls in ('clinical', 'research'):
        infix = '.' + cls + '.'
        if infix in fname:
            place(src, os.path.join(pair_dir, cls, fit_name), fname.replace(infix, '.'))
            return

    # Shared (class-agnostic) files -> copy into both class fit dirs.
    if 'facets2n_normal_selection' in fname:
        for cls in ('clinical', 'research'):
            d = os.path.join(pair_dir, cls, fit_name)
            os.makedirs(d, exist_ok=True)
            shutil.copy2(src, os.path.join(d, fname))
        os.remove(src)
        return

    log("unmatched file left in place: %s" % fname)


def main():
    if len(sys.argv) != 5:
        sys.exit(__doc__)

    sample_id, staging_dir, pair_dir, fit_name = sys.argv[1:5]
    staging_dir = staging_dir.rstrip('/')
    pair_dir = pair_dir.rstrip('/')

    if not os.path.isdir(staging_dir):
        sys.exit("staging dir not visible: " + staging_dir)

    log("splitting %s -> %s/{clinical,research}/%s" % (staging_dir, pair_dir, fit_name))
    for fname in sorted(os.listdir(staging_dir)):
        if os.path.isfile(os.path.join(staging_dir, fname)):
            handle_file(fname, staging_dir, pair_dir, fit_name)

    # Drop the now-emptied staging dir; leave it if anything remained.
    try:
        os.rmdir(staging_dir)
    except OSError:
        log("staging dir not empty after split, leaving: " + staging_dir)

    log("done: " + sample_id)


if __name__ == '__main__':
    main()
