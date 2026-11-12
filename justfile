# The docs run the `rush` on PATH, so these recipes install this checkout into
# a project-local library (.dev/lib) and put a wrapper for it in .dev/bin. Your
# own `rush` installation is left alone.

dev := justfile_directory() / ".dev"

# List the recipes
default:
    @just --list

# Install this checkout into .dev and create the .dev/bin/rush wrapper
install:
    mkdir -p "{{ dev }}/lib" "{{ dev }}/bin"
    R CMD INSTALL --no-test-load -l "{{ dev }}/lib" "{{ justfile_directory() }}"
    printf '%s\n' '#!/bin/sh' \
        'R_LIBS="{{ dev }}/lib${R_LIBS:+:$R_LIBS}" exec Rscript "{{ justfile_directory() }}/exec/rush.R" "$@"' \
        > "{{ dev }}/bin/rush"
    chmod +x "{{ dev }}/bin/rush"

# Preview the documentation website, re-rendering pages as you edit them.
# Extra arguments go to quarto, e.g. `just preview --port 4200 --no-browser`
preview *args: install
    PATH="{{ dev }}/bin:$PATH" quarto preview docs {{ args }}

# Render the whole documentation website to docs/_site
render: install
    PATH="{{ dev }}/bin:$PATH" quarto render docs

# Remove .dev and the cached example output of the docs
clean:
    rm -rf "{{ dev }}" docs/.quarto-term-cache
