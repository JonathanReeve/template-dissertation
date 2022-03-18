# Dissertation Template

A template for a modern, best-practices dissertation, which exports to HTML.

For this to make sense, please [read the blog post which started this, "Best Practices for a 21st Century Dissertation"](http://jonreeve.com/2022/03/best-practices-dissertation/). 

## Features

This is a best-practices dissertation template, which meets [the requirements of Columbia University's Graduate School of Arts and Sciences](https://gsas.columbia.edu/content/formatting-guidelines-and-dissertation-template), and probably the requirements of other schools and universities, as well. The template is: 
 
1. Public by default. (It's already a GitHub repo, so you don't have to worry about making one.) 
2. Written in markdown, compiled to HTML. [Read the blog post, "Best Practices for a 21st Century Dissertation"](http://jonreeve.com/2022/03/best-practices-dissertation/), for the reasoning behind this. 
3. Annotation-ready. It includes a [Hypothes.is](https://web.hypothes.is/) layer, to make your dissertation annotatable. 

Its features include: 

1. Automatic generation of your bibliography, in whatever bibliographic format you want, using [Pandoc's Citeproc](https://pandoc.org/MANUAL.html#citations). You should never have to write these things out by hand. 
2. Annotations using [Hypothes.is](https://web.hypothes.is/). 
3. Support for a powerful markdown derivative, [Pandoc's markdown](https://pandoc.org/MANUAL.html#pandocs-markdown), with tons of features useful to scholarly writing. 
4. Layout in [Tufte.css](https://edwardtufte.github.io/tufte-css/) for beautiful typography and layout. 
5. Sidenotes by default, rather than footnotes or endnotes. (See Tufte for more on this.) 
6. Support for figures and images, automatically numbered sequentially, with captions in the margins. 
7. Support for LaTeX-style math and MathML, via [MathJax](https://www.mathjax.org/). 
8. Modern, standards-compliant CSS and HTML. 
9. Excellent, machine-readable metadata for the semantic web, using [Schema.org](https://schema.org/). 
10. GitHub Actions and GitHub Pages integration, for automatic builds and deploys. Serve to the web at no cost to you. 

## Getting Started

1. Fork this repo. Click the big green button above, on GitHub, marked "use this template." This will make a copy ("fork") the repository in your own GitHub user account, where you can modify it as much as you want. 
2. Clone your fork locally. From your own copy of this repo, in your user account, grab the URL from the green button marked "code," and clone your repository locally, with `git clone` followed by the URL to your fork of the repository. 
3. [Install the Nix package manager, if you don't already have it.](https://nixos.org/download.html)
4. Enter the nix shell, with `nix-shell`. The first time you run it, it might take a while, but will be instantaneous from then on. Alternatively, if you have `direnv`, simply run `direnv allow` once, and the nix shell will be engaged automatically from then on, whenever you enter the directory. 
5. Build the example dissertation by running `shake`. 
6. See the results by running `shake serve` and opening `http://localhost:8080` in your web browser. 
7. Edit the markdown files in `source/` so that they contains your own disseration. 
8. Add your references to `references.bib`, a biblatex file. 
9. Rebuild by running `shake` from the root directory. View the results again with `shake serve`. 

Once it builds, commit your changes and push to your repository. Enable GitHub actions, and it'll automatically serve it to GitHub pages. 

For an example, see [my own disseration](http://dissertation.jonreeve.com/) (still a work in progress).  

## Depositing Using Zenodo

Depositing your dissertation at Zenodo is a great idea. It gives you even more permanent storage, at CERN in Switzerland; a DOI, to help your dissertaiton be better cited; and indexing services, for better discoverability. 

To deposit your dissertation at Zenodo, *even if it's not finished*: 

1. Make sure your GitHub account is linked to your Zenodo account. 
2. Flip the switch next to your dissertation's repository, [from within your Zenodo account](https://zenodo.org/account/settings/github/). (You might want to rename it from `template-dissertation` to something more semantic.) 
3. [Create a release on GitHub](https://help.github.com/articles/creating-releases). Zenodo will automatically download new releases. 
4. Grab the DOI badge from Zenodo, and add it to your README.md. 

[More instructions are here at Zenodo.org](https://zenodo.org/account/settings/github/)

## Technical Details

This templates uses the [Shake build system](https://shakebuild.com/manual) to transform markdown files into HTML, along with the excellent [pandoc](https://pandoc.org/]. Here's what all the files do: 

- `Shakefile.hs`: The main build instructions. See the Shake manual for more on how to customiz this. 
- `Template.hs`: The main HTML template. Change this if you want to change the way the final page looks. 
- `default.nix`: The nix file which specifies all the build dependenies. 
- `templates/modern-language-association.csl`: A Citation Style Language file for MLA style. Change this to something else to change the citation style (see the FAQ)
- `templates/PandocSidenote.hs`: A Pandoc filter for making sidenotes. 
- `source/`: This is where your markdown source files live. Edit these. 
     
## FAQ

1. Can I use a different markup format? Like Org-mode, ASCIIDOC, or something else? 

Sure! Just adapt the Shakefile so that `.md` extensions are whatever extensions your desired input format is. 

2. Can I customize the look and feel? 

Absolutely! It's all in `Template.hs`, in the section `CSS`. It's written using [Clay](http://fvisser.nl/clay/), a delightful CSS preprocessor. 

3. Can I use a different citation format, other than that of the Modern Language Association? 

You betcha! Just grab [a CSL file from here](https://github.com/citation-style-language/styles) which represents your desired style. Then change the filename to the CSL, in the Shakefile. 

    
