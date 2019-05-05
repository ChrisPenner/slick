---
title: Shipping Haskell via Homebrew
author: Chris Penner
date: Apr 24, 2017
tags: [haskell, programming]
description: How to set up your Haskell project to distribute via Homebrew
image: homebrew.jpg
---

If you're reading this I assume you already love Haskell; so I won't convince
you of why it's great to work in. One thing that isn't so great is Haskell's
story for distributing code to non-haskellers. `stack install` is great, but
most folks don't have stack installed and compiling Haskell projects from
source is a lengthy process. These barriers prevented me from sharing my
Haskell projects for a long time.

Here's how I eventually set up my project to be installed via Homebrew.

We'll be using Homebrew's binary deployment strategy since it's the easiest to both set up
and for users to install.

If you're content to build binaries using stack locally and upload them to Github yourself then
you can skip down to the Homebrew Formula section.

## Building Binaries with Travis-CI

Here's a look at my `.travis.yml`:

```yaml
addons:
  apt:
    packages:
    - libgmp-dev
language: c
sudo: false
cache:
  directories:
  - $HOME/.local/bin
  - $HOME/.stack
os:
- linux
- osx
before_install:
- sh tools/install-stack.sh
- sh tools/install-ghr.sh
script:
- stack setup
- stack build --ghc-options -O2 --pedantic
after_success:
- sh tools/attach-binary.sh
```

This is just a basic setup for building haskell on Travis-CI; we need the
additional package `libgmp-dev`, cache a few things, and specify to build for
both linux and osx. This way we'll have both linux and osx binaries when we're
done! In the pre-install hooks we install stack manually, then install
[ghr](https://github.com/tcnksm/ghr) a github resource management tool.

You can find
[install-stack.sh](https://github.com/ChrisPenner/tempered/blob/master/tools/install-stack.sh)
and
[install-ghr.sh](https://github.com/ChrisPenner/tempered/blob/master/tools/install-ghr.sh)
scripts on my [Tempered](https://github.com/ChrisPenner/tempered) project. They use
Travis Env variables for everything, so you can just copy-paste them into your project.

Inside `script` we build the project as normal you can do this however you like so long
as a binary is produced.

Lastly is the [`attach-binary.sh`](https://github.com/ChrisPenner/tempered/blob/master/tools/attach-binary.sh) script.
This runs after the build and uploads the generated binaries to the releases page on Github.
It first checks if the current release is tagged and will only build and upload tagged releases, so make sure you
`git tag vx.y.z` your commits before you push them or it won't run the upload step.
Next it pulls in your github token which ghr will use to do the upload. You must manually
add this to your Travis-CI Environment variables for the project.
Create a new github access token [here](https://github.com/settings/tokens)
then add it to your Travis-CI project at `https://travis-ci.org/<user>/<repo>/settings`
under the name `GITHUB_TOKEN`.

The script assumes the binary has the same name as your repo, if that's not the case
you can hard-code the script to something else. At this point whenever you upload
a tagged release Travis-CI should run a mac and a linux build and upload the result
of each to your Github Repo's releases page. You'll likely need to trouble-shoot
one or two things to get it just right.

## Setting up a Homebrew Formula

You can follow [this guide by octavore](http://octavore.com/posts/2016/02/15/distributing-go-apps-os-x)
to set up your own homebrew tap; then we'll make a formula. Here's what mine for my tempered project looks like:

```ruby
class Tempered < Formula
  desc "A dead-simple templating utility for simple shell interpolation"
  homepage "https://github.com/ChrisPenner/tempered"
  url "https://github.com/ChrisPenner/tempered/releases/download/v0.1.0/tempered-v0.1.0-osx.tar.gz"
  sha256 "9241be80db128ddcfaf9d2fc2520d22aab47935bcabc117ed874c627c0e1e0be"

  bottle :unneeded

  def install
    bin.install "tempered"
  end

  test do
    system "#{bin}/tempered"
  end
end
```

You'll of course have to change the names, and you'll need to change the url to
match the uploaded tar.gz file (for osx) on your github releases page from step
one.

Lastly we'll need to get the `sha 256` of the bundle; you can just download it
and run `shasum -a 256 <filename>` if you like; or you can look in your
Travis-CI logs for the osx build under the `attach-binary.sh` step; the script
logs out the sha sum before uploading the binary.

After you've pushed up your homebrew formula pointing to the latest binary then
users can install it by running:

```sh
# Replace the names respectively
brew update && brew install githubuser/tapname/reponame
```

Each time you release a new version you'll need to update the url and sha in
the homebrew formula; you could automate this as a script to run in Travis if
you like; I haven't been bothered enough to do it yet, but if you do it let me
know and I'll update this post!

This guide was inspired by (and guided by) [Taylor Fausak's
post](http://taylor.fausak.me/2016/05/09/add-files-to-github-releases/) on a
similar topic; most of the scripts are adapted from his.

Cheers and good luck!
