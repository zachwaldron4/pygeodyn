# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

project = 'Pygeodyn Documentation'
copyright = '2022, Zach Waldron'
author = 'Zach Waldron'

# The full version, including alpha/beta/rc tags
release = 'tbd'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'recommonmark',
    'sphinx_markdown_tables',
    'nbsphinx',
    'sphinx.ext.mathjax',
    'sphinxcontrib.pdfembed',
    ]

mathjax_config = {
    'TeX': {'equationNumbers': {'autoNumber': 'AMS', 'useLabelIds': True}},
}

 # mathjax_config = app.config._raw_config.setdefault('mathjax_config', {})
# mathjax_config.setdefault(
#      'tex2jax',
#      {
#          'inlineMath': [['$', '$'], ['\\(', '\\)']],
#          'processEscapes': True,
#          'ignoreClass': 'document',
#          'processClass': 'math|output_area',
#      }
#  )


# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [ ]
highlight_language = 'none'
source_suffix = {
    '.rst': 'restructuredtext',
    '.txt': 'markdown',
    '.md': 'markdown',
}


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
#html_theme = 'alabaster'

import sphinx_rtd_theme
html_theme = "sphinx_rtd_theme"

html_theme_options = {
    "collapse_navigation" : False
}
theme_navigation_depth = 4


# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static', 'pdfs_slides_images']

