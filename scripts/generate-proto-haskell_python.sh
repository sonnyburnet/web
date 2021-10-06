#!/usr/bin/env python3

import glob
import subprocess
import os
import fnmatch

def find_files(directory, pattern):
    for root, dirs, files in os.walk(directory):
        for basename in files:
            if fnmatch.fnmatch(basename, pattern):
                filename = os.path.join(root, basename)
                yield filename

def remove_prefix(text, prefix):
    return text[len(prefix):] if text.startswith(prefix) else text

for file in find_files('sub/proto', '*.proto'):
    subprocess.call(['compile-proto-file', '--out', 'src/proto/', '--includeDir', 'sub/proto/', '--proto', remove_prefix(file, 'sub/proto/')])
