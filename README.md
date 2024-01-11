## Release
### Synopsis
We created ```release``` as alternative to ```maven-release-plugin``` for git based projects.
It does the job much faster and with fewer commits. It also helps us to create
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

### Show libyears of dependency updates
See https://libyear.com/ for details. It works for maven projects ```pom.xml``` and SBT ```build.sbt```.
```
release showDependencyUpdates --show-libyears
[...]
╠═╦═ com.typesafe:config:1.4.0
║ ╚═══ 1.4.1 (libyears: 1.0 [376 days])
╠═╦═ org.jline:jline-reader:3.16.0
║ ╚═══ 3.17.0, .., 3.18.0, 3.19.0 (libyears: 0.6 [185 days])
╠═╦═ org.jline:jline-terminal:3.16.0
║ ╚═══ 3.17.0, .., 3.18.0, 3.19.0 (libyears: 0.6 [185 days])
╠═╦═ org.scala-lang:scala-library:2.13.3
║ ╚═══ 2.13.4, 2.13.5 (libyears: 0.7 [242 days])
╠═╦═ org.scalatest:scalatest_2.13:3.2.2
║ ╚═══ 3.2.3, 3.2.4, 3.2.5 (libyears: 0.5 [181 days])
║
[...]
libyears: 6.3 (2304 days)
```

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
