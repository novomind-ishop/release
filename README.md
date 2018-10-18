## Release
[![Build Status](https://travis-ci.org/novomind-ishop/release.svg?branch=master)](https://travis-ci.org/novomind-ishop/release)
### Synopsis
We created ```release``` as alternative to ```maven-release-plugin``` for git based projects.
It does the job much faster and with less commits. It also helps us to create
branches, tags, standardize commit-messages and ```pom.xml``` modifications with and without [Gerrit](https://www.gerritcodereview.com).
It also checks your ```pom.xml``` for problems and suggests hints to fix them.

### Features
* no -SNAPSHOTS in releases
* matching release major versions of ishop-core
* create branches
* modify GAV (GroupId ArtifactId Version)
* suggest rebase
* smart suggest of next/current version
* handle local changes
* support different shell variants (gitbash, cygwin, linux, ...)
* ...
* release from master/feature/detached HEAD/...

### Usage
This projects contains an executable called ```release```. To use it, checkout this
project and execute it in your project.

e.g.
```
cd the/project
${HOME}/git/release/release
```
This assumes you checked out "release" in ```${HOME}/git```. It is also possible
to add ```release``` to your ```PATH``` and call it without an absolute path.
```
cd the/project
release
```
If you want to learn other options of ```release``` try ```release --help```.

### Config
The default config is located in ```${HOME}/.ishop-release```. ```${HOME}``` should be equal with
```System.getProperty("user.home")```; to check this see ```release --help``` where the path to your home directory is shown.

The config file looks like the following:
```
ishop-release.gerrit.hostname=your-gerrit.example.org
ishop-release.gerrit.signedOfBy=Signer Name <signer@example.org>
ishop-release.gerrit.port=29418
ishop-release.jenkins.base=https://your-jenkins.example.org
ishop-release.branch.prefix=your-branch-prefix
ishop-release.release.prefix=your-release-prefix
ishop-release.gerrit.url=https://your-gerrit.example.org/
ishop-release.nexus.mirror.url=https://your-nexus-mirror.example.org/content/groups/public/
ishop-release.nexus.work.url=http://your-nexus/nexus/content/repositories/public
```
