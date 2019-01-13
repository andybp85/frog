# This is a work in progress and the below reflects plans, not current status
# GFrog

This fork utilizes [CSS Grid](https://learncssgrid.com/#naming-positioning-items-grid-areas) and [Tachyons](https://tachyons.io/) for the template (instead of Bootstrap/jQuery), as well as implements a command to load new posts from a Google Docs folder (which is based heavily on [google-drive-racket](https://github.com/fgmart/google-drive-racket)).

## To utilize Google Drive
You need to define three params:
```
ga-client-id
ga-client-secret
(define ga-posts-folder
```
How you do so is up to you, but most importantly:

**DO NOT PUT THESE VALUES IN A PUBLIC REPO!!!**

So, putting them in your `frog.rkt` is a bad idea if you're going to put it on Github. You could, for example, put them in a separate file that's gitignore'd and import that file into `frog.rkt`. However, what you should do is use environment variables. I've defined some default env vars that GFrog will check for the required info. The env var names are params, so you can change them if you'd like to.

Note that tokens are stored in files named with a hash of param `current-scheme/host`, so if this is changed you'll be prompted to log in again.

### Default env vars
```
FROG_GA_CLIENT_ID
FROG_GA_CLIENT_SECRET
FROG_GA_POSTS_FOLDER
```

### Set up credentials
(mostly taken from a comment in [google-drive-racket](https://github.com/fgmart/google-drive-racket))
1. go to Google Developers Console and make a project
2. go to Dashboard > Explore other services > Enable APIs and get credentials like keys
3. then Google Apps APIs > Drive API (and enable it)
4. then in the left-column menu, Credentials
5. then Create credentials > Oauth client ID > Other
6. then copy the client ID and secret into the appropriate env vars.
7. in Google Drive, make a folder for your posts, and then copy the folder ID in its env var.

### Writing posts in GDocs
GFrog will load the doc as HTML and strip out the GDocs-specific stuff, so just make a doc as you normally would.

---
**Below is the original readme for Frog and does not accurately reflect this repo**

# Frog

<p><a href="http://www.flickr.com/photos/doug88888/4717363945/" title="Happy Green frog by @Doug88888, on Flickr"><img src="http://farm5.staticflickr.com/4070/4717363945_b73afd78a9.jpg" width="300" height="300" alt="Happy Green frog"></a></p>

<p><sub><em><a href="http://www.flickr.com/photos/doug88888/4717363945/">Frog image by @Goug8888</a>, used under Creative Commons license <a href="http://creativecommons.org/licenses/by-nc-sa/2.0/">Attribution-NonCommercial-ShareAlike 2.0 Generic</a>.</em></sub></p>

[![Build Status](https://travis-ci.org/greghendershott/frog.svg?branch=master)](https://travis-ci.org/greghendershott/frog)
[![raco pkg install frog](https://img.shields.io/badge/raco_pkg_install-frog-aa00ff.svg)](http://pkgs.racket-lang.org/package/frog)
[![MIT License](https://img.shields.io/badge/license-MIT-118811.svg)](frog/LICENSE)
[![Documentation](https://img.shields.io/badge/Docs-Documentation-blue.svg)](http://docs.racket-lang.org/frog/index.html)

Frog is a static web site generator written in [Racket][].

You write content in [Markdown][] or [Scribble][]. You generate
files. To deploy, you push them to a GitHub Pages repo (or copy them
to Amazon S3, or whatever).

Posts get a variety of automatic blog features.

You can also create non-post pages.

[Pygments][] handles syntax highlighting for code blocks.

The generated site uses [Bootstrap][], which is [responsive][],
automatically adapting to various screen sizes.

[Full documentation](http://docs.racket-lang.org/frog/index.html).

[Racket]: http://www.racket-lang.org
[Markdown]: http://daringfireball.net/projects/markdown/syntax
[Scribble]: http://docs.racket-lang.org/scribble/index.html
[Pygments]: http://pygments.org/
[Bootstrap]: http://getbootstrap.com/
[responsive]: https://en.wikipedia.org/wiki/Responsive_web_design
