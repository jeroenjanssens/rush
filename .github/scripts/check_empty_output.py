"""Fail when a rendered {term} cell runs rush or ir but prints nothing.

A command such as `rush run 'df'` without input prints nothing and exits 0,
so Quarto renders an empty cell without complaint. This script flags cells
that have no output, and that run `rush run`, `rush sql`, or `ir run` without
writing to a file (`-o`, `--output`, or a `>` redirect).

Usage: python3 check_empty_output.py docs/_site
"""

import glob
import html
import os
import re
import sys

CELL = re.compile(r'<pre class="term-output"><code>(.*?)</code></pre>', re.S)
PROMPT = re.compile(r'<span class="term-prompt" data-prompt="([^"]*)"></span>')
TAG = re.compile(r"<[^>]+>")
QUOTED = re.compile(r"'[^']*'|\"[^\"]*\"")
RUNS = re.compile(r"\b(rush\s+(run|sql)|ir\s+run)\b")
WRITES = re.compile(r"(^|\s)(-o|--output)(\s|=|$)|>")


def commands_and_output(cell):
    """Split a cell into its commands (joined over continuation lines) and
    its output lines."""
    commands, output = [], []
    for line in cell.split("\n"):
        prompt = PROMPT.search(line)
        text = html.unescape(TAG.sub("", line)).strip()
        if prompt and prompt.group(1).strip() == "$":
            commands.append(text)
        elif prompt and commands:
            commands[-1] += " " + text
        elif text:
            output.append(text)
    return commands, output


def silent_commands(commands):
    """The commands that should print something, ignoring quoted text."""
    flagged = []
    for command in commands:
        bare = QUOTED.sub("''", command)
        if RUNS.search(bare) and not WRITES.search(bare):
            flagged.append(command)
    return flagged


def main(site):
    problems = []
    for path in sorted(glob.glob(os.path.join(site, "**", "*.html"), recursive=True)):
        if os.sep + "site_libs" + os.sep in path:
            continue
        with open(path, encoding="utf-8") as f:
            content = f.read()
        for cell in CELL.findall(content):
            commands, output = commands_and_output(cell)
            if output:
                continue
            for command in silent_commands(commands):
                problems.append(f"{os.path.relpath(path, site)}: {command}")
    for problem in problems:
        print(problem)
    if problems:
        print(f"::error::{len(problems)} example(s) printed nothing (listed above)")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else "docs/_site"))
